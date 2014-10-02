let c_gnu            = 1
let c_no_curly_error = 1
let c_space_errors   = 1
let c_syntax_for_h   = 1

nnoremap <buffer> <F8>       :!clear && ./%:r<CR>
nnoremap <buffer> <F9>       :call <sid>make()<CR>
nnoremap <buffer> <leader>d  ][V%{d

setlocal cinoptions  =>4,l1,p0,)50,*50,t0
setlocal comments    =sO:*\ -,mO:*\ \ ,exO:*/,s1:/*,mb:*,ex:*/
setlocal shiftwidth  =4
setlocal softtabstop =4

autocmd QuickFixCmdPre  make let s:make_stime = localtime()
autocmd QuickFixCmdPost make let s:make_etime = localtime() - s:make_stime

if !filereadable('Makefile')
  compiler gcc
  if &filetype == 'c'
    let &l:makeprg = "gcc -std=c11 -pedantic -Wall -Wextra -Wfloat-equal -ftrapv -o %:r %"
  elseif &ft == 'cpp'
    let &l:makeprg = "clang++ -std=c++11 -pedantic -Wall -Wextra -Weffc++ -o %:r %"
  endif
endif

function! s:make() abort
  silent! make
  redraw!
  cwindow
  normal! "<C-w>p"
  echo printf('Compiled.. %s (Time taken: %dm%2.2ds)',
        \ expand('%'),
        \ s:make_etime / 60,
        \ s:make_etime % 60)
endfunction
