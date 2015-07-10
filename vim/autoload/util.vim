"
" Jump to definitions of s:foo(), <sid>bar, and foo#bar().
"
function! util#lookup()
  let name = matchstr(expand('<cWORD>'), '^[^(]*')
  if name =~# '^s:'
    call s:find_local_definition(name[2:])
  elseif name =~ '<sid>'
    call s:find_local_definition(name[5:])
  elseif name =~ '#' && name[0] != '#'
    call s:find_autocmd_definition(name)
  endif
endfunction

function! s:find_local_definition(name)
  call search('\v^\s*fu%[nction]!?\s+%(s:|\<sid\>)\V'. a:name, 'cesw')
endfunction

function! s:find_autocmd_definition(name)
  let [path, function] = split(a:name, '.*\zs#')
  let name = printf('autoload/%s.vim', substitute(path, '#', '/', 'g'))
  let audirs = globpath(&runtimepath, name, '', 1)
  if !empty(audirs)
    let aufile = audirs[0]
    let lnum = match(readfile(aufile),
          \ '\v^\s*fu%[nction]!?\s+\V'. path .'#'. function .'\>')
    if lnum > -1
      execute 'edit +'. (lnum+1) aufile
    endif
  endif
endfunction

"
" Showing [+1 -2 ~3] in statusline.
"
function! util#sy_stats_wrapper()
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
function! util#search() abort
  let regsave = @@
  normal! gvy
  let @/ = '\V' . substitute(escape(@@, '\'), '\n', '\\n', 'g')
  let @@ = regsave
endfunction

function! util#search_all() abort
  call util#search()
  call setqflist([])
  execute 'bufdo vimgrepadd! /'. @/ .'/ %'
endfunction

"
" For 'foldtext'.
"
function! util#foldy()
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
" I prefer two vertically splitted windows per tabpage.
"
function! util#helpme() abort
  if &buftype == 'help'
    if winnr('$') == 2
      silent wincmd L
    else
      silent wincmd T
    endif
  endif
endfunction

"
" Switch to VCS root, if there is one.
"
function! util#cd(bang) abort
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
    execute (a:bang ? 'cd' : 'lcd') s:cache[curdir]
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
function! util#scratch(cmd) abort
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
" Make <tab> a little bit more useful.
"
function! util#tab_yeah(new, default)
  let line = getline('.')
  let col = col('.') - 2
  if !empty(line) && line[col] =~ '\k' && line[col + 1] !~ '\k'
    return a:new
  else
    return a:default
  endif
endfunction
