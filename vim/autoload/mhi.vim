"
" Jump to definitions of s:foo(), <sid>bar, and foo#bar().
"
function! mhi#lookup()
  let isk = &iskeyword
  setlocal iskeyword+=:,.,<,>
  let name = expand('<cword>')
  if name =~# '^s:'
    call s:find_local_definition(name[2:])
  elseif name =~ '^<sid>'
    call s:find_local_definition(name[5:])
  elseif stridx(name, '.') > 0
    call search('\c\v^\s*fu%[nction]!?\s+.{-}\.'. name[stridx(name,'.')+1:], 'cesw')
  elseif name =~ '#' && name[0] != '#'
    call s:find_autoload_definition(name)
  endif
  let &iskeyword = isk
endfunction

function! s:find_local_definition(name)
  call search('\c\v^\s*fu%[nction]!?\s+%(s:|\<sid\>)\V'. a:name, 'cesw')
endfunction

function! s:find_autoload_definition(name)
  let [path, function] = split(a:name, '.*\zs#')
  let pattern = '\c\v^\s*fu%[nction]!?\s+\V'. path .'#'. function .'\>'
  let name = printf('autoload/%s.vim', substitute(path, '#', '/', 'g'))
  let autofiles = globpath(&runtimepath, name, '', 1)
  if empty(autofiles) && exists('b:git_dir')
    let autofiles = [ fnamemodify(b:git_dir, ':h') .'/'. name ]
  endif
  if empty(autofiles)
    call search(pattern)
  else
    let autofile = autofiles[0]
    let lnum = match(readfile(autofile), pattern)
    if lnum > -1
      execute 'edit +'. (lnum+1) autofile
    endif
  endif
endfunction

"
" Smarter tag-based jumping.
"
function! mhi#jump()
  if (&filetype == 'vim' && &buftype == 'nofile') || &buftype == 'quickfix'
    execute "normal! \<cr>"
  else
    if exists('g:cscoped')
      " Todo: https://gist.github.com/mhinz/1a23d24f88b396b65aec
      " nmap <expr> <cr> mhi#jump()
      " execute "normal \<leader>cg"
      execute 'cscope find g' expand('<cword>')
      normal! zvzt
    else
      execute "normal! g\<c-]>zvzt"
    endif
    call mhi#pulse()
  endif
endfunction

"
" Showing [+1 -2 ~3] in statusline.
"
function! mhi#sy_stats_wrapper()
  let symbols = ['+', '-', '~']
  let [added, modified, removed] = sy#repo#get_stats()
  let stats = [added, removed, modified]  " reorder
  let hunkline = ''
  for i in range(3)
    if stats[i] > 0
      let hunkline .= printf('%s%s ', symbols[i], stats[i])
    endif
  endfor
  if !empty(hunkline)
    let hunkline = '%3*[%5*'. hunkline[:-2] .'%3*]%*'
  endif
  return hunkline
endfunction

"
" Verbatim matching for *.
"
function! mhi#search() abort
  let regsave = @@
  normal! gvy
  let @/ = '\V' . substitute(escape(@@, '\'), '\n', '\\n', 'g')
  let @@ = regsave
endfunction

function! mhi#search_all() abort
  call mhi#search()
  call setqflist([])
  execute 'bufdo vimgrepadd! /'. @/ .'/ %'
endfunction

"
" For 'foldtext'.
"
function! mhi#foldy()
  let linelen = &tw ? &tw : 80
  let marker  = strpart(&fmr, 0, stridx(&fmr, ',')) . '\d*'
  let range   = foldclosedend(v:foldstart) - foldclosed(v:foldstart) + 1

  let left    = substitute(getline(v:foldstart), marker, '', '')
  let leftlen = len(left)

  let right    = range . ' [' . v:foldlevel . ']'
  let rightlen = len(right)

  let tmp    = strpart(left, 0, linelen - rightlen)
  let tmplen = len(tmp)

  if leftlen > len(tmp)
    let left    = strpart(tmp, 0, tmplen - 4) . '... '
    let leftlen = tmplen
  endif

  let fill = repeat(' ', linelen - (leftlen + rightlen))

  return left . fill . right . repeat(' ', 100)
endfunction

"
" Switch to VCS root, if there is one.
"
function! mhi#cd(bang) abort
  if &buftype =~# '\v(nofile|terminal)' || expand('%') =~# '^fugitive'
    return
  endif
  if !exists('s:cache')
    let s:cache = {}
  endif
  let dirs   = [ '.git', '.hg', '.svn' ]
  let curdir = resolve(expand('%:p:h'))
  if !isdirectory(curdir)
    echohl WarningMsg | echo 'No such directory: '. curdir | echohl NONE
    return
  endif
  if has_key(s:cache, curdir)
    execute (a:bang ? 'cd' : 'lcd') fnameescape(s:cache[curdir])
    return
  endif
  for dir in dirs
    let founddir = finddir(dir, curdir .';')
    if !empty(founddir)
      break
    endif
  endfor
  let dir = empty(founddir) ? curdir : resolve(fnamemodify(founddir, ':p:h:h'))
  let s:cache[curdir] = dir
  execute (a:bang ? 'cd' : 'lcd') fnameescape(dir)
endfunction

"
" Capture output of any command in a new window.
"
function! mhi#scratch(cmd) abort
  let more = &more
  set nomore
  try
    let lines = 1
    redir => lines
    silent execute a:cmd
  finally
    redir END
    let &more = more
  endtry
  noautocmd new
  setlocal buftype=nofile bufhidden=hide noswapfile
  silent put =lines
  1
  nnoremap <buffer> q :q<cr>
endfunction

"
" Make <tab> a little bit more useful. Stolen from @junegunn.
"
function! s:can_complete(func, prefix)
  if empty(a:func) || call(a:func, [1, '']) < 0
    return 0
  endif
  let result = call(a:func, [0, matchstr(a:prefix, '\k\+$')])
  return !empty(type(result) == type([]) ? result : result.words)
endfunction

function! mhi#tab_yeah(k, o)
  if pumvisible()
    return a:k
  endif

  let line = getline('.')
  let col = col('.') - 2
  if empty(line) || line[col] !~ '\k\|[/~.]' || line[col + 1] =~ '\k'
    return a:o
  endif

  let prefix = expand(matchstr(line[0:col], '\S*$'))
  if prefix =~ '^[~/.]'
    return "\<c-x>\<c-f>"
  endif
  if s:can_complete(&omnifunc, prefix)
    return "\<c-x>\<c-o>"
  endif
  if s:can_complete(&completefunc, prefix)
    return "\<c-x>\<c-u>"
  endif
  return a:k
endfunction

"
" Guess what! Stolen from @sjl.
"
function! mhi#pulse()
  redir => old_cul
    silent execute 'highlight CursorLine'
  redir END

  let old_cul = split(old_cul, '\n')[0]
  let old_cul = substitute(old_cul, 'xxx', '', '')
  let steps   = 8
  let width   = 1
  let start   = width
  let end     = steps * width
  let color   = 233

  for i in range(start, end, width)
    execute 'highlight CursorLine ctermbg=' . (color+i)
    redraw
    sleep 6m
  endfor
  for i in range(end, start, -1 * width)
    execute 'highlight CursorLine ctermbg=' . (color+i)
    redraw
    sleep 6m
  endfor

  execute 'highlight' old_cul
endfunction

"
" Get syntax group information. Stolen from @jamessan.
"
function! s:synnames()
  let syn                 = {}
  let [lnum, cnum]        = [line('.'), col('.')]
  let [effective, visual] = [synID(lnum, cnum, 0), synID(lnum, cnum, 1)]
  let syn.effective       = synIDattr(effective, 'name')
  let syn.effective_link  = synIDattr(synIDtrans(effective), 'name')
  let syn.visual          = synIDattr(visual, 'name')
  let syn.visual_link     = synIDattr(synIDtrans(visual), 'name')
  return syn
endfunction

function! mhi#syninfo()
  let syn = s:synnames()
  let info = ''
  if syn.visual != ''
    let info .= printf('visual: %s', syn.visual)
    if syn.visual != syn.visual_link
      let info .= printf(' (as %s)', syn.visual_link)
    endif
  endif
  if syn.effective != syn.visual
    if syn.visual != ''
      let info .= ', '
    endif
    let info .= printf('effective: %s', syn.effective)
    if syn.effective != syn.effective_link
      let info .= printf(' (as %s)', syn.effective_link)
    endif
  endif
  return info
endfunction

" vim: fdm=syntax
