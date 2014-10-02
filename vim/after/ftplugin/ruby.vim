compiler ruby

let ruby_operators    = 1
let ruby_space_errors = 1

setlocal comments-=:#
setlocal shiftwidth=2 softtabstop=2

nnoremap <buffer><f8> :!ruby %<cr>
