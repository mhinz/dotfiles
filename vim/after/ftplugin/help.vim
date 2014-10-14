augroup help
  autocmd!
  autocmd BufEnter <buffer> let b:so = &scrolloff | let &scrolloff = 0
  autocmd BufLeave <buffer> let &scrolloff = b:so
augroup END

setlocal bufhidden=delete

nnoremap <buffer> q :call Qlose()<cr>
