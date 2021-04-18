"
" Show the commit that touched the current line last.
"
function! mhi#git_blame_current_line() abort
  let gitdir = finddir('.git', expand('%:p').';')
  if empty(gitdir)
    echo 'no git'
    return
  endif
  let sha = matchstr(systemlist(printf('git blame --porcelain -lL%d,+1 %s',
        \ line('.'), expand('%')))[0], '\x\+')
  new
  execute 'silent file fugitive://'.gitdir.'//'.sha
  edit
  silent! %foldopen
  set bufhidden=wipe
  nnoremap <silent><buffer> q :quit<cr>
endfunction

"
" Show evolution of current line
"
function mhi#git_log_evolution(startline, endline, file) abort
  let cmd = printf('git log -L %s,%s:%s', a:startline, a:endline, a:file)
  enew
  set buftype=nofile
  setfiletype git
  let &l:statusline = cmd
  nnoremap <silent><buffer> q :bd<cr>
  execute 'silent! %!' cmd
endfunction

"
" Close nearest open bracket.
"
function! mhi#close_bracket() abort
  highlight BracketRange cterm=underline
  let pos = [0, 0]
  let pairs = {}
  for [open, close] in map(split(&matchpairs, ','), 'split(v:val, ":")')
    let pairs[open] = close
    let m = searchpairpos(escape(open, '['), '', close, 'bnW')
    if m[0] > pos[0]
      let pos = m
    elseif m[1] > pos[1] && m[0] == pos[0]
      let pos[1] = m[1]
    endif
  endfor
  if pos != [0, 0]
    let cur = getcurpos()[1:2]
    call clearmatches()
    call matchadd('BracketRange',
          \ '\%'.pos[0].'l\%'.pos[1].'c.*\_.\+\%'.cur[0].'l\%'.cur[1].'c')
    call timer_start(&matchtime * 100, {-> clearmatches()})
    return pairs[matchstr(getline(pos[0]), '.', pos[1]-1)]
  endif
  return ''
endfunction

"
" GitHub
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
  let slug = substitute(matchstr(url, '\v[/:]\zs.*'), '\.git', '', '')
  let url = 'https://github.com/'. slug .'/issues/'. issue
  silent execute '!open -a Google\ Chrome' url
endfunction

function! mhi#github_open_slug() abort
  let old_isk = &iskeyword
  let &iskeyword = 'a-z,A-z,48-57,-,_,/,.'
  let slug = expand('<cword>')
  let &iskeyword = old_isk
  call system('open https://github.com/'. slug)
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
" Smarter jumping.
"
function! mhi#jump()
  if (&filetype ==# 'vim' && &buftype ==# 'nofile') || &buftype ==# 'quickfix'
    execute "normal! \<cr>"
  elseif &filetype ==# 'vim'
    call lookup#lookup()
  elseif &filetype ==# 'man'
    execute "normal \<c-]>"
  elseif has_key(get(g:, 'ls#servers', {}), &filetype)
    call ls#feature#definition()
  else
    let word = expand('<cword>')
    if empty(word)
      return
    endif
    try
      execute 'tag' word
    catch
      echoerr substitute(v:exception, '.\{-}:', '', '')
      return
    endtry
    call halo#run()
  endif
  " normal! zvzt
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

"
" Jump forward/backward to next match and highlight it.
"
function! mhi#search_highlight_next_match(cmd) abort
  silent! call matchdelete(s:match)
  try
    execute 'normal!' a:cmd.'zvzz'
  catch /E486/  " pattern not found
    echohl ErrorMsg | echo substitute(v:exception, '^.\{-}:', '', '') | echohl NONE
  endtry
  let line = line('.')
  let col = col('.')
  call search(@/, 'zce', line, 100)
  let len = col('.') - col + 1
  call cursor(line, col)
  let s:match = matchaddpos('IncSearch', [[line, col, len]])
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
  let curdir = mhi#normalize(resolve(expand('%:p:h')))
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
  let dir = empty(founddir) ? curdir : mhi#normalize(resolve(fnamemodify(founddir, ':p:h:h')))
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
" Make <tab> a little bit more useful. Stolen from junegunn.
"
function! s:can_complete(func, prefix)
  if empty(a:func) || a:func(1, '') < 0
    return 0
  endif
  let result = a:func(0, matchstr(a:prefix, '\k\+$'))
  return !empty(type(result) == type([]) ? result : result.words)
endfunction

function! mhi#tab_yeah()
  if pumvisible()
    return "\<c-n>"
  endif
  let line = getline('.')
  let col = col('.') - 2
  if empty(line) || line[col] !~ '\k\|[/~.]' || line[col + 1] =~ '\k'
    return "\<tab>"
  endif
  let prefix = expand(matchstr(line[0:col], '\S*$'))
  if prefix =~ '^[~/.]'
    return "\<c-x>\<c-f>"
  endif
  if !empty(&completefunc) && s:can_complete(function(&completefunc), prefix)
    return "\<c-x>\<c-u>"
  endif
  if !empty(&omnifunc) && s:can_complete(function(&omnifunc), prefix)
    return "\<c-x>\<c-o>"
  endif
  return "\<c-n>"
endfunction

"
" Get syntax group information. Stolen from jamessan.
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
" Run Exercism tests
"
function! mhi#run_exercism_tests() abort
  if expand('%:e') == 'vim'
    let testfile = printf('%s/%s.vader', expand('%:p:h'),
          \ tr(expand('%:p:h:t'), '-', '_'))
    if !filereadable(testfile)
      echoerr 'File does not exist: '. testfile
      return
    endif
    source %
    execute 'Vader' testfile
  else
    let sourcefile = printf('%s/%s.vim', expand('%:p:h'),
          \ tr(expand('%:p:h:t'), '-', '_'))
    if !filereadable(sourcefile)
      echoerr 'File does not exist: '. sourcefile
      return
    endif
    execute 'source' sourcefile
    Vader
  endif
endfunction

"
" Find and source project-specific Vim configs
"
function! mhi#source_project_config() abort
  let projectfile = findfile('.project.vim', expand('%:p').';')
  if filereadable(projectfile)
    execute 'source' projectfile
  endif
endfunction

"
" Cycle through completion functions.
"
function! mhi#next_completion() abort
  let compls = [ 'emoji#complete', 'tmuxcomplete#complete' ]
  let &cfu = compls[(index(compls, &cfu) + 1) % len(compls)]
  echomsg 'Using '. &cfu
endfunction

"
" Wrapper for :terminal
"
function! mhi#terminal(bang, mods, cmd) abort
  let terms = filter(map(tabpagebuflist(), 'getbufvar(v:val, ''terminal_job_id'')'), '!empty(v:val)')
  if empty(terms)
    execute a:mods 'new'
    let g:terminal = termopen($SHELL)
    $
    if a:bang | wincmd p | endif
  endif
  call chansend(g:terminal, a:cmd."\n")
endfunction

"
" Switch buffer. Skip buffers already shown in another window.
"
function! mhi#switch_buffer(cmd) abort
  try
    execute a:cmd
  catch /E85/  " There is no listed buffer
    return
  endtry
  let bufs = tabpagebuflist()
  while !empty(bufs)
    let buf = bufnr('')
    if count(bufs, buf) == 1 | break | endif
    execute a:cmd
    call filter(bufs, 'v:val != '.buf)
  endwhile
  stopinsert
  call halo#run()
endfunction

"
" :cd chokes on UNC paths with two backslashes. Use one backslash instead.
"
" Bad:  :cd z:\\share
" Good: :cd z:\share
"
function! mhi#normalize(path) abort
  return has('win32') && &shellslash == 0
        \ ? substitute(a:path, '\v^(\w):\\\\', '\1:\\', '')
        \ : a:path
endfunction

"
" Highlight the '123' in 'ctermfg=123' according to its number.
"
function! mhi#vim_highlight_groups()
  for c in range(256)
    execute 'syntax match CtermFg'.c '/\<\%(ctermfg=\)\@<='.c.'\>/ display containedin=vimHiNmbr'
    execute 'syntax match CtermBg'.c '/\<\%(ctermbg=\)\@<='.c.'\>/ display containedin=vimHiNmbr'
    execute 'highlight CtermFg'.c 'ctermfg='.c 'cterm=NONE'
    execute 'highlight CtermBg'.c 'ctermfg='.c 'cterm=NONE'
  endfor
endfunction

" vim: fdm=syntax
