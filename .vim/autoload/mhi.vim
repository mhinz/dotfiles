"
" Github
"
function! mhi#github_open_issue() abort
  if !exists('b:git_dir')
    return
  endif
  let issue = matchstr(expand('<cWORD>'), '\d\+')
  if empty(issue)
    return
  endif
  let remote = systemlist('git config branch.master.remote || echo origin')[0]
  let url = systemlist(printf('git config remote.%s.url', remote))[0]
  let url = 'https://github.com/'. matchstr(url, '\v[/:]\zs.*') .'/issues/'. issue
  silent execute '!open -a Google\ Chrome' url
endfunction

"
" Tmux
"
function! mhi#tmux_navigate(direction) abort
  let oldwin = winnr()
  execute 'wincmd' a:direction
  if !empty($TMUX) && winnr() == oldwin
    let sock = split($TMUX, ',')[0]
    let direction = tr(a:direction, 'hjkl', 'LDUR')
    silent execute printf('!tmux -S %s select-pane -%s', sock, direction)
  endif
endfunction

"
" Jump to definitions of s:foo(), <sid>bar, and foo#bar().
"
function! mhi#lookup()
  let isk = &iskeyword
  setlocal iskeyword+=:,.,<,>,#
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
  elseif &filetype == 'neoman'
    execute "normal \<c-]>"
  else
    if exists('g:cscoped')
      try
        execute 'cscope find g' expand('<cword>')
      catch /E259/
        echohl WarningMsg
        redraw | echomsg 'No match found: '. expand('<cword>')
        echohl NONE
      endtry
    else
      try
        execute "normal! g\<c-]>"
      catch /E349/ " no identifier under cursor
      catch /E433/
        echohl WarningMsg
        redraw | echomsg 'No match found: '. expand('<cword>')
        echohl NONE
      endtry
    endif
    normal! zvzt
    call halo#run()
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
function! mhi#cd() abort
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
    execute 'lcd' fnameescape(s:cache[curdir])
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
  execute 'lcd' fnameescape(dir)
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
" Blink! Blink!
"
if v:version >= 800 || has('nvim')
  " Stolen from @justinmk.
  highlight Halo guifg=white guibg=#F92672 ctermfg=white ctermbg=197
  let g:halo = {}
  function! s:halo_clear(id) abort
    silent! call matchdelete(g:halo['hl_id'])
  endfunction
  function! s:halo() abort
    call s:halo_show(-1)
    call timer_start(100, function('s:halo_show'))
    call timer_start(200, function('s:halo_show'))
  endfunction
  function! s:halo_show(id) abort
    call s:halo_clear(-1)
    let lcol = col('.') > 10 ? col('.') - 10 : 1
    let g:halo['hl_id'] = matchaddpos('Halo',
          \[[line('.'),   lcol, 20],
          \ [line('.')-1, lcol, 20],
          \ [line('.')-2, lcol, 20],
          \ [line('.')+1, lcol, 20],
          \ [line('.')+2, lcol, 20]]
          \)
    call timer_start(50, function('s:halo_clear'))
  endfunction
  augroup halo_plugin
    autocmd!
    autocmd WinLeave * call s:halo_clear(-1)
  augroup END
  function! mhi#pulse()
    call s:halo()
  endfunction
else
  " Idea stolen from @sjl.
  function! mhi#pulse()
    echomsg 'FOO'
    let fg = synIDattr(hlID('CursorLine'), 'fg', 'cterm')
    let bg = synIDattr(hlID('CursorLine'), 'bg', 'cterm')

    if fg == -1
      let fg = synIDattr(hlID('Normal'), 'fg', 'cterm')
    endif

    highlight CursorLine ctermfg=15 cterm=bold

    for color in [204,203,162,161,161,161,162,203,204]
      execute 'highlight CursorLine ctermbg='. color
      redraw
      sleep 10m
    endfor

    execute printf('highlight CursorLine ctermfg=%s ctermbg=%s cterm=NONE', fg, bg)
  endfunction
endif

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

"
" Dump Neovim API
"
function! mhi#dump_api()
  let api = msgpackparse(systemlist('nvim --api-info'))[0]

  for v in api.functions
    echohl Function
    echomsg v.name._VAL[0]
    echohl NONE

    echohl Title
    echon '('. join(map(v.parameters, 'v:val[1]._VAL[0] .":". v:val[0]._VAL[0]'), ', ') .')'
    echohl NONE

    echon ' -> '

    echohl Constant
    echon v.return_type._VAL[0]
    echohl NONE

    echohl Comment
    echon ' async: '. (v.async ? '✓' : '☓') .', can fail: '. (has_key(v, 'can_fail') && v.can_fail ? '✓' : '☓')
    echohl NONE
  endfor
endfunction

" vim: fdm=syntax
