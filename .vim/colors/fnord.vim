let g:colors_name = 'fnord'

if !has('gui_running') || &termguicolors == 0
  if exists('+termguicolors')
    let &termguicolors = 1
  else
    echohl Error
    echomsg "vim-fnord requires a Vim supporting the 'termguicolors' option."
    echohl NONE
    finish
  endif
endif

highlight clear
syntax reset

function! s:hi(group, fg, bg, gui, guisp)
  let config = a:group
  if !empty(a:fg)    | let config .= ' guifg='.a:fg                    | endif
  if !empty(a:bg)    | let config .= ' guibg='.a:bg                    | endif
  if !empty(a:gui)   | let config .= ' gui='  .a:gui . ' cterm='.a:gui | endif
  if !empty(a:guisp) | let config .= ' guisp='.a:guisp                 | endif
  execute 'highlight' config
endfunction

let s:blue0   = '#35d4d9'
let s:blue1   = '#25909c'
let s:blue2   = '#013969'
let s:blue3   = '#05224a'
let s:blue4   = '#68b3da'
let s:blue5   = '#00c2f4'
let s:blue6   = '#5879bd'
let s:blue7   = '#124a7b'
let s:gray0   = '#afb8c4'
let s:gray1   = '#eeeeee'
let s:gray2   = '#555588'
let s:yellow0 = '#e5e568'
let s:purple  = '#5e50ba'
let s:orange  = '#d69a66'
let s:orange2 = '#e3c08e'
" let s:red     = '#772d26'
let s:green   = '#97dd7b'
let s:green2  = '#103850'
let s:red     = '#d2554f'
let s:red2    = '#d42a60'
let s:red3    = '#fb6c89'
let s:pink    = '#d371e3'

call s:hi('Normal', s:gray0, s:blue2, 'NONE', '')

" call s:hi('NormalFloat', '', s:blue3,  'NONE', '')
" call s:hi('FloatBorder', s:red3, s:blue3, 'NONE', '')

call s:hi('Visual', s:blue2, s:green, 'NONE', 'NONE')
call s:hi('CursorLine', '', s:blue7, 'NONE', 'NONE')

call s:hi('Special', s:gray0, '', 'NONE', '')
call s:hi('Statement' , s:pink, '' , 'NONE', '')
call s:hi('Function', s:yellow0, '', 'NONE', '')
call s:hi('Identifier', s:blue0, '', 'NONE', '')
call s:hi('PreProc', s:blue0, '', 'NONE', '')
call s:hi('String', s:green, '', 'NONE', '')
" call s:hi('StatusLine', 'black', s:blue0, 'NONE', '')
call s:hi('StatusLine', s:gray0, s:blue7, 'NONE', '')
call s:hi('StatusLineNC', 'black', s:blue1, 'NONE', '')
call s:hi('Type', s:blue0, '', 'bold', '')
call s:hi('VertSplit', '', s:blue7, 'NONE', '')
call s:hi('Comment', s:blue4, '', 'NONE', '')
call s:hi('Folded', s:gray1, s:blue1, 'NONE', '')
call s:hi('EndOfBuffer', s:blue4, '', 'NONE', '')
call s:hi('Constant', s:orange, '', 'NONE', '')
call s:hi('NonText', s:blue6, '', 'NONE', '')

call s:hi('LineNr', s:blue6, s:blue2, 'NONE', '')
call s:hi('CursorLineNr', s:yellow0, s:blue2, 'NONE', '')
call s:hi('SignColumn', s:orange, s:blue2, 'NONE', '')

call s:hi('DiffAdd', s:green, s:blue2, 'NONE', '')
call s:hi('DiffChange', s:orange, s:blue2, 'NONE', '')
call s:hi('DiffDelete', s:red3, s:blue2, 'NONE', '')

call s:hi('Pmenu', s:blue4, s:blue3, 'NONE', '')
call s:hi('PmenuSel', 'black', s:orange, 'NONE', '')
call s:hi('PmenuSbar', 'black', s:blue3, 'NONE', '')
call s:hi('PmenuThumb', 'black', s:blue2, 'NONE', '')

call s:hi('RFCType', s:blue6, '', 'NONE', '')
call s:hi('RFCID', s:blue4, '', 'NONE', '')
call s:hi('RFCDelim', s:blue6, '', 'NONE', '')      " fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff

call s:hi('ErrorMsg', s:red3, s:blue2, 'NONE', '')

highlight default link diffAdded     DiffAdd
highlight default link diffRemoved   DiffDelete
highlight default link diffFile      Comment
highlight default link diffNewFile   Comment
highlight default link diffLine      Comment
highlight default link diffIndexLine Comment
highlight default link diffSubname   Comment
