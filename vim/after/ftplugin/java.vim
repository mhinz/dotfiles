compiler javac

let &l:makeprg = 'javac %'

let g:java_highlight_java_lang_ids = 1
let g:java_highlight_java_io       = 1
let g:java_highlight_functions     = 'style'

setlocal comments-=://
setlocal omnifunc=javacomplete#Complete
setlocal completefunc=javacomplete#CompleteParamsInfo

inoremap <buffer><c-x><c-u> <c-x><c-u><c-p>
nnoremap <buffer><f8>       :!java %:r<cr>
