function! s:make() abort
  silent! make
  redraw!
  cwindow
  wincmd p
  echo printf('Compiled.. %s (Time taken: %dm%2.2ds)',
        \ expand('%'),
        \ s:make_etime / 60,
        \ s:make_etime % 60)
endfunction

function! s:find_cscope_db() abort
  if exists('b:cscoped') && filereadable(b:cscoped)
    return b:cscoped
  endif
  if !empty($CSCOPE_DB)
    return $CSCOPE_DB
  endif
  return findfile('cscope.out', '.;')
endfunction

function! s:add_cscope_db(...) abort
  let db = a:0 ? a:1 : s:find_cscope_db()
  if empty(db)
    return
  endif
  silent cscope reset
  silent! execute 'cscope add' db
  let b:cscoped = db
endfunction

function! s:build_cscope_db(...) abort
  let db = s:find_cscope_db()
  if !empty(db)
    let dbdir = fnamemodify(db, ':h')
    silent execute 'lcd' dbdir
  endif
  let exts = empty(a:000)
        \ ? [ 'c', 'h', 'cc', 'hh', 'cpp', 'hpp', 'java' ]
        \ : a:000
  let cmd = "find . " . join(map(exts, "\"-name '*.\" . v:val . \"'\""), ' -o ')
  let tmp = tempname()
  try
    echon 'Building '. getcwd() .'/cscope.files.. '
    call system(cmd .'  > '. tmp)
    echon 'generating database.. '
    call system('cscope -b -q -i '. tmp)
    echon 'complete!'
    call s:add_cscope_db(db)
  finally
    silent! call delete(tmp)
  endtry
endfunction

command! -bar -nargs=? CSadd   call s:add_cscope_db(<f-args>)
command! -bar -nargs=* CSbuild call s:build_cscope_db(<f-args>)

let c_gnu            = 1
let c_no_curly_error = 1
let c_space_errors   = 1
let c_syntax_for_h   = 1

setlocal cinoptions  =>4,l1,p0,)50,*50,t0
setlocal comments    =sO:*\ -,mO:*\ \ ,exO:*/,s1:/*,mb:*,ex:*/
setlocal shiftwidth  =4
setlocal softtabstop =4

nnoremap <buffer> <F8>      :!clear && ./%:r<CR>
nnoremap <buffer> <F9>      :call <sid>make()<CR>
nnoremap <buffer> <leader>d ][V%{d

autocmd QuickFixCmdPre  make let s:make_stime = localtime()
autocmd QuickFixCmdPost make let s:make_etime = localtime() - s:make_stime

if !filereadable('Makefile')
  compiler gcc
  if &filetype == 'c'
    let &l:makeprg = 'gcc -std=c11 -pedantic -Wall -Wextra -Wfloat-equal -ftrapv -o %:r %'
  elseif &ft == 'cpp'
    let &l:makeprg = "clang++ -std=c++11 -pedantic -Wall -Wextra -Weffc++ -o %:r %"
  endif
endif

CSadd
