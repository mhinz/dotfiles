let &l:makeprg = 'ghc %'

let hs_highlight_boolean    = 1
let hs_highlight_types      = 1
let hs_highlight_more_types = 1
let hs_highlight_debug      = 1
let hs_allow_hash_operator  = 1

setlocal comments-=--

nnoremap <buffer><f8> :!./%:r<cr>
