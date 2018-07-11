if exists('b:lua_compiler')
  finish
endif

let b:lua_compiler = ''

for compiler in ['luac', 'luac5.3', 'lua5.2', 'luac5.1']
  if executable(compiler)
    let b:lua_compiler = compiler
  endif
endfor

if empty(b:lua_compiler)
  finish
endif

let &l:errorformat = '%s: %f:%l:%m'
let &l:makeprg = b:lua_compiler .' -p %'

autocmd BufWritePost <buffer> silent make | cwindow
