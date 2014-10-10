augroup help
  autocmd!
  autocmd BufEnter <buffer> let b:so = &scrolloff | let &scrolloff = 0
  autocmd BufLeave <buffer> let &scrolloff = b:so
augroup END

nnoremap <buffer> q :call Qlose()<cr>

silent wincmd T
