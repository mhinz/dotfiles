compiler perl

let perl_include_pod = 1

setl comments-=:#
let &l:keywordprg = 'perldoc -f'

nnoremap <buffer><f8> :!perl %<cr>
