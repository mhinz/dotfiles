"
"                         ███████████████████████████
"                         ███████▀▀▀░░░░░░░▀▀▀███████
"                         ████▀░░░░░░░░░░░░░░░░░▀████
"                         ███│░░░░░░░░░░░░░░░░░░░│███
"                         ██▌│░░░░░░░░░░░░░░░░░░░│▐██
"                         ██░└┐░░░░░░░░░░░░░░░░░┌┘░██
"                         ██░░└┐░░░░░░░░░░░░░░░┌┘░░██
"                         ██░░┌┘▄▄▄▄▄░░░░░▄▄▄▄▄└┐░░██
"                         ██▌░│██████▌░░░▐██████│░▐██
"                         ███░│▐███▀▀░░▄░░▀▀███▌│░███
"                         ██▀─┘░░░░░░░▐█▌░░░░░░░└─▀██
"                         ██▄░░░▄▄▄▓░░▀█▀░░▓▄▄▄░░░▄██
"                         ████▄─┘██▌░░░░░░░▐██└─▄████
"                         █████░░▐█─┬┬┬┬┬┬┬─█▌░░█████
"                         ████▌░░░▀┬┼┼┼┼┼┼┼┬▀░░░▐████
"                         █████▄░░░└┴┴┴┴┴┴┴┘░░░▄█████
"                         ███████▄░░░░░░░░░░░▄███████
"                         ██████████▄▄▄▄▄▄▄██████████
"                         ███████████████████████████
"
"   You are about to experience a potent dosage of Vim. Watch your steps.
"
"                ╔══════════════════════════════════════════╗
"                ║           ⎋ HERE BE VIMPIRES ⎋           ║
"                ╚══════════════════════════════════════════╝

let s:is_win = has('win32')
let $v = $HOME.(s:is_win ? '\vimfiles' : '/.vim')

if s:is_win
  set shell=cmd.exe
  set shellcmdflag=/c
  set encoding=utf-8
endif

" Plug {{{1
let s:bundle_dir = $v.'/bundle'

let g:plug_shallow = 0
let g:plug_window  = 'enew'
let g:plug_pwindow = 'vertical rightbelow new'

call plug#begin(s:bundle_dir)
  Plug 'jonathanfilip/vim-lucius'

  Plug 'AndrewRadev/splitjoin.vim'
  Plug 'chrisbra/unicode.vim', {'on': ['UnicodeName', 'UnicodeTable']}
  Plug 'easymotion/vim-easymotion', {'on': '<plug>(easymotion-s2)'}
  Plug 'godlygeek/csapprox', {'for': 'fugitiveblame' }
  Plug 'junegunn/fzf', has('win32') ? {} : {'on': 'FZF', 'dir': '~/local/fzf', 'do': './install --bin'}
  Plug 'junegunn/gv.vim', {'on': 'GV'}
  Plug 'junegunn/vader.vim'
  Plug 'junegunn/vim-easy-align', {'on': '<plug>(LiveEasyAlign)'}
  Plug 'junegunn/vim-plug'
  Plug 'justinmk/vim-dirvish'
  Plug 'majutsushi/tagbar', {'on': 'TagbarToggle'}
  Plug 'mbbill/undotree', {'on': 'UndotreeToggle'}
  Plug 'rhysd/committia.vim'
  Plug 'tpope/vim-commentary'
  Plug 'tpope/vim-endwise'
  Plug 'tpope/vim-fugitive'
  Plug 'tpope/vim-rhubarb'
  Plug 'tpope/vim-rsi'
  Plug 'tpope/vim-scriptease'
  Plug 'tpope/vim-surround'
  Plug 'tweekmonster/helpful.vim'

  Plug 'rust-lang/rust.vim', {'for': 'rust'}
  Plug 'rhysd/rust-doc.vim', {'for': 'rust'}
  Plug 'lifepillar/pgsql.vim', {'for': 'sql'}
  Plug 'mattn/emmet-vim', {'for': 'html'}
  Plug 'AndrewRadev/tagalong.vim', {'for': 'html'}
  Plug 'pangloss/vim-javascript'
  Plug 'HerringtonDarkholme/yats.vim'
  Plug 'elixir-lang/vim-elixir'

  let g:plug_url_format = 'git@github.com:%s.git'
  Plug 'mhinz/nvim-langserver'
  Plug 'mhinz/nvim-panacea', {'for': 'elixir'}
  Plug 'mhinz/vim-grepper'
  Plug 'mhinz/vim-halo'
  Plug 'mhinz/vim-janah'
  Plug 'mhinz/vim-lookup'
  Plug 'mhinz/vim-mix-format'
  Plug 'mhinz/vim-rfc'
  Plug 'mhinz/vim-sayonara', {'on': 'Sayonara'}
  Plug 'mhinz/vim-signify'
  Plug 'mhinz/vim-startify'
  Plug 'mhinz/vim-tree'
  unlet! g:plug_url_format
call plug#end()
" }}}1

" Init {{{1
if has('nvim')
  let $GIT_EDITOR = 'nvr -cc split --remote-wait'
  let $ELIXIR_EDITOR = 'nvr -l +__LINE__ __FILE__'
  if v:progpath =~# 'build/bin/nvim$'
    " for build/runtime/syntax/vim/generated.vim
    let &runtimepath .= ','.$VIM.'/build/runtime'
  endif
else
  filetype plugin indent on
  syntax on
  packadd! matchit
endif

let mapleader = ';'

let g:vimsyn_folding = 'f'
let g:is_bash        = 1
let g:lisp_rainbow   = 1

let g:markdown_fenced_languages = ['vim']

let g:loaded_2html_plugin     = 1
let g:loaded_getscriptPlugin  = 1
let g:loaded_gzip             = 1
let g:loaded_logipat          = 1
" let g:loaded_matchparen       = 1
let g:loaded_rrhelper         = 1
let g:loaded_spellfile_plugin = 1
let g:loaded_tarPlugin        = 1
let g:loaded_vimballPlugin    = 1
let g:loaded_zipPlugin        = 1

augroup vimrc
  autocmd!
augroup END

" Option {{{1
" indent settings
set autoindent
set cinoptions    =l1,p0,)50,*50,t0
set expandtab
set smarttab
set softtabstop   =2
set shiftwidth    =2
set shiftround

" backup/swap/info/undo settings
set backup
set backupext     =-vimbackup
set backupskip    =
set undofile
set updatecount   =100
if has('nvim')
  set backupdir  -=.
  set shada       ='100
else
  set backupdir   =$v/files/backup
  set directory   =$v/files/swap//
  set undodir     =$v/files/undo
  set viewdir     =$v/files/view
  set viminfo     ='100,n$v/files/info/viminfo
endif

" better navigation
set cursorline
set foldmethod    =marker
set foldopen     +=jump
set foldtext      =mhi#foldy()
set hlsearch
set ignorecase
set incsearch
set mouse         =a
set scrolloff     =4
set sidescroll    =5
set smartcase
set tagcase       =match

" misc settings
set autoread
set backspace     =indent,eol,start
set clipboard     =unnamed
set complete     -=i
set completeopt  +=noselect
set cpoptions    -=e
set diffopt      +=vertical,foldcolumn:0,indent-heuristic,algorithm:patience
set fileformats   =unix,dos,mac
set hidden
set history       =1000
set lazyredraw
set more
set mousemodel    =popup
set noautowrite
set noautowriteall
set noequalalways
set noerrorbells
set nofsync
set nojoinspaces
set wrapscan
set nrformats     =hex
set pastetoggle   =<F5>
set norelativenumber
set report        =0
set sessionoptions-=options
set sessionoptions+=localoptions
set showfulltag
set showtabline   =1
set splitbelow
set splitright
set switchbuf     =
set synmaxcol     =200
set tags         ^=./.git/tags;
set timeout
set timeoutlen    =1000
set titlestring   =VIM:\ %f
set ttimeout
set ttimeoutlen   =10
set ttyfast
set virtualedit   =onemore,block
set whichwrap     =h,l

" wild stuff
set suffixes     +=.a,.1,.class
set wildignore   +=*.o,*.so,*.zip,*.png
set wildmenu
set wildoptions   =tagfile
if has('nvim-0.4.0')
  set wildoptions+=pum
else
  set wildmode=list:longest,full
endif

" display settings
set display      +=lastline
set laststatus    =2
set list
set modeline
set modelines     =1
set nostartofline
set numberwidth   =1
set ruler
set shortmess     =aoOTI
set showcmd
set showmatch
set showmode

" spelling
set spellfile     =~/.vim/spell/en.utf-8.add,~/.vim/spell/de.utf-8.add
set spelllang     =en,de

" breaking
set wrap
set nolinebreak
set breakindent
set breakindentopt=min:40
"set highlight    +=@:SpecialKey

set cpoptions     =aABcefFqsZ
set formatoptions =tcrqnj

if has('multi_byte') && &encoding ==# 'utf-8'
  let &listchars = 'tab:▸ ,extends:❯,precedes:❮,nbsp:±'
  let &fillchars = 'vert: ,diff: '  " ▚
  let &showbreak = '↪ '
  highlight VertSplit ctermfg=242
  augroup vimrc
    autocmd InsertEnter * set listchars-=trail:⣿
    autocmd InsertLeave * set listchars+=trail:⣿
  augroup END
else
  let &listchars = 'tab:> ,extends:>,precedes:<,nbsp:.'
  let &fillchars = 'vert: ,stlnc:#'
  let &showbreak = '-> '
  augroup vimrc
    autocmd InsertEnter * set listchars-=trail:.
    autocmd InsertLeave * set listchars+=trail:.
  augroup END
endif

" Mapping {{{1
nnoremap \   ;
inoremap jf  <esc>

nnoremap gh :call mhi#github_open_issue()<cr>
nnoremap gH :call mhi#github_open_slug()<cr>

inoremap <expr> <tab>    mhi#tab_yeah("\<c-n>", "\<tab>")
inoremap <expr> <s-tab>  mhi#tab_yeah("\<c-p>", "\<s-tab>")

nnoremap <c-i>  <c-i>zvzz
nnoremap <c-o>  <c-o>zvzz

inoremap <silent> <c-g><c-t>  <c-r>=repeat(complete(col('.'), map(["%Y-%m-%d %H:%M:%S","%a, %d %b %Y %H:%M:%S %z","%Y %b %d","%d-%b-%y","%a %b %d %T %Z %Y"],'strftime(v:val)')+[localtime()]),0)<cr>

nnoremap <f12>  :setlocal spell!<cr>

nnoremap <expr> n  'Nn'[v:searchforward].'zvzz'
nnoremap <expr> N  'nN'[v:searchforward].'zvzz'

nnoremap gg  ggzv
nnoremap G   Gzv

inoremap <expr> <c-]> mhi#close_bracket()

nnoremap <silent> <c-p> :cprevious \| call halo#run()<cr>
nnoremap <silent> <c-n> :cnext \| call halo#run()<cr>
nnoremap <silent> [q    :cprevious \| call halo#run()<cr>
nnoremap <silent> ]q    :cnext \| call halo#run()<cr>
nnoremap <silent> [l    :lprevious \| call halo#run()<cr>
nnoremap <silent> ]l    :lnext \| call halo#run()<cr>
nnoremap <silent> [b    :call mhi#switch_buffer('bprevious')<cr>
nnoremap <silent> ]b    :call mhi#switch_buffer('bnext')<cr>
nnoremap <silent> [t    :tabprevious \| call halo#run()<cr>
nnoremap <silent> ]t    :tabnext \| call halo#run()<cr>
nnoremap <silent> <tab> <c-w>w:call halo#run()<cr>

nnoremap [<space>  :<c-u>put! =repeat(nr2char(10), v:count1)<cr>'[
nnoremap ]<space>  :<c-u>put =repeat(nr2char(10), v:count1)<cr>

nnoremap [e  :<c-u>execute 'move -1-'. v:count1<cr>
nnoremap ]e  :<c-u>execute 'move +'. v:count1<cr>

" switch comma-separated arguments
nnoremap [w  :normal F,h]w2w<cr>
nnoremap ]w  :let s=&isk \| setl isk+=\",' \| exe "norm diwmmebgPdw'mP" \| let &isk=s<cr>

nnoremap }   }zz
nnoremap {   {zz
nnoremap ]]  ]]zz
nnoremap [[  [[zz
nnoremap []  []zz
nnoremap ][  ][zz

nnoremap g;  g;zvzz
nnoremap g,  g,zvzz

nnoremap <left>   <c-w>>
nnoremap <right>  <c-w><
nnoremap <up>     <c-w>-
nnoremap <down>   <c-w>+

nnoremap <silent><a-h>  :<c-u>call mhi#tmux_navigate('h')<cr>
nnoremap <silent><a-j>  :<c-u>call mhi#tmux_navigate('j')<cr>
nnoremap <silent><a-k>  :<c-u>call mhi#tmux_navigate('k')<cr>
nnoremap <silent><a-l>  :<c-u>call mhi#tmux_navigate('l')<cr>

xnoremap <  <gv
xnoremap >  >gv

onoremap gv  :<c-u>normal! gv<cr>

nnoremap Q  gqap

nnoremap <leader>ev  :edit $MYVIMRC<cr>
nnoremap <leader>em  :edit ~/.vim/autoload/mhi.vim<cr>

nnoremap <leader>si  :echo mhi#syninfo()<cr>
nnoremap <leader>ss  :echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')<cr>

nnoremap <leader>m  :Messages<cr>
nnoremap <leader>M  :<c-u><c-r><c-r>='let @'. v:register .' = '. string(getreg(v:register))<cr><c-f><left>
" nnoremap <leader>o  :!echo `git url`/blob/`git rev-parse --abbrev-ref HEAD`/%\#L<C-R>=line('.')<cr> \| xargs open<cr><cr>
nnoremap <leader>o  :silent !open <c-r>=fnameescape('<c-r><c-a>')<cr><cr>
nnoremap <leader>w  :update<cr>
nnoremap <leader>x  :xit<cr>
nnoremap <leader>l  :noh<cr>:diffupdate<cr>:syntax sync fromstart<cr><c-l>
nnoremap <leader>v  :Runtime<cr>
nnoremap <leader>t  :Tree<cr>
nnoremap <silent> <leader>d  :exe 'sil !open' fnameescape(printf('https://devdocs.io/#q=%s%%20%s', &ft, expand('<cword>')))<cr>

nnoremap <silent><leader>b  :call mhi#git_blame_current_line()<cr>

nnoremap '  `
nnoremap `  '

nnoremap <bs>  <c-t>
nnoremap <silent><cr>  :call mhi#jump()<cr>

cnoremap <c-n>  <down>
cnoremap <c-p>  <up>

cnoremap <leader>r   $VIMRUNTIME/
cnoremap <leader>ft  ~/.vim/after/ftplugin/

xnoremap *          :<c-u>call mhi#search()<cr>//<cr>
xnoremap #          :<c-u>call mhi#search()<cr>??<cr>
xnoremap <leader>*  :<c-u>silent! execute mhi#search_all()<cr>

if has('nvim')
  nnoremap <leader>T      :vsplit +terminal<cr>
  tnoremap <esc>          <c-\><c-n>
  tnoremap <silent><a-h>  <c-\><c-n>:<c-u>call mhi#tmux_navigate('h')<cr>
  tnoremap <silent><a-j>  <c-\><c-n>:<c-u>call mhi#tmux_navigate('j')<cr>
  tnoremap <silent><a-k>  <c-\><c-n>:<c-u>call mhi#tmux_navigate('k')<cr>
  tnoremap <silent><a-l>  <c-\><c-n>:<c-u>call mhi#tmux_navigate('l')<cr>
endif

if !has('nvim')
  if empty($TMUX)
    let &t_SI = "\<Esc>]50;CursorShape=1\x7"
    let &t_EI = "\<Esc>]50;CursorShape=0\x7"
    if v:version >= 800
      let &t_SR = "\<Esc>]50;CursorShape=2\x7"
    endif
  else
    let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
    let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
    if v:version >= 800
      let &t_SR = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=2\x7\<Esc>\\"
    endif
  endif
endif

" Autocmd {{{1
augroup vimrc
  if has('nvim')
    autocmd TermOpen,BufEnter term://* startinsert
  endif

  autocmd BufReadPost *
        \ if line("'\"") > 1 && line("'\"") <= line("$") |
        \   execute 'normal! g`"zvzz' |
        \ endif

  autocmd WinEnter    * set cursorline
  autocmd WinLeave    * set nocursorline
  autocmd InsertEnter * set nocursorline
  autocmd InsertLeave * set cursorline

  autocmd CompleteDone * pclose
augroup END

" Command {{{1
command! -nargs=1 -complete=command Scratch call mhi#scratch(<f-args>)
command! CleanSpell  runtime! spell/cleanadd.vim
command! Cd call mhi#cd()
" command! Evo exe printf('GV -L :%s:%s', expand('<cword>'), expand('%:p'))
command! Changed exe 'args' join(map(systemlist('git diff --name-only --relative "$@" 2>/dev/null'), 'fnameescape(v:val)'))
command! -nargs=1 -bang T call mhi#terminal(<bang>0, <f-mods>, <f-args>)

" Color {{{1
function! s:colors_default() abort
  highlight Comment cterm=italic
  highlight link User1 StatusLine   " master branch
  highlight link User2 StatusLine   " other branch
  highlight link User3 StatusLine   " separators
  highlight link User4 StatusLine   " filename at beginning
  highlight link User5 StatusLine   " ~changes
  highlight link SignifySignAdd    DiffAdd
  highlight link SignifySignDelete DiffDelete
  highlight link SignifySignChange DiffChange
  highlight Halo guifg=white guibg=#F92672 ctermfg=white ctermbg=197
endfunction

function! s:colors_janah() abort
  highlight User1  ctermfg=192 ctermbg=237 cterm=NONE
  highlight User2  ctermfg=167 ctermbg=237 cterm=NONE
  highlight User3  ctermfg=245 ctermbg=237 cterm=NONE
  highlight User4  ctermfg=215 ctermbg=237 cterm=NONE
  highlight User5  ctermfg=111 ctermbg=237 cterm=NONE
endfunction

function! s:colors_lucius() abort
  if &background ==# 'light'
    highlight Normal                  ctermbg=NONE
    highlight CursorLine              ctermbg=255
    highlight User1      ctermfg=84   ctermbg=237  cterm=NONE
    highlight User2      ctermfg=222  ctermbg=237  cterm=NONE
    highlight User3      ctermfg=237  ctermbg=237  cterm=NONE
    highlight User4      ctermfg=255  ctermbg=237  cterm=NONE
    highlight User5      ctermfg=255  ctermbg=237  cterm=NONE
    highlight StatusLine              ctermbg=237  cterm=NONE
    highlight SignColumn ctermfg=NONE ctermbg=NONE cterm=NONE
    highlight Question   ctermfg=24   ctermbg=255  cterm=NONE
    highlight Search     ctermfg=fg   ctermbg=222  cterm=NONE
    highlight Folded                  ctermbg=253  cterm=NONE
    highlight NormalFloat ctermfg=231 ctermbg=240  cterm=NONE
  endif
endfunction

function! s:colors_snow() abort
  highlight Normal ctermbg=NONE guibg=NONE
endfunction

augroup vimrc
  autocmd ColorScheme *      call s:colors_default()
  autocmd ColorScheme janah  call s:colors_janah()
  autocmd ColorScheme lucius call s:colors_lucius()
  autocmd ColorScheme snow   call s:colors_snow()
augroup END

if has('gui_running')
  set background=light
  colorscheme lucius
  set guioptions=a
  set mousehide
  if s:is_win
    autocmd GUIEnter * simalt ~x
    let &guifont = 'Consolas:h10:b'
  elseif has('gui_macvim')
    set macmeta
    let &guifont = 'Source Code Pro:h13'
  endif
  command! Bigger  let &guifont = substitute(&guifont, '\d\+', '\=submatch(0)+1', '')
  command! Smaller let &guifont = substitute(&guifont, '\d\+', '\=submatch(0)-1', '')
else
  if !exists('$ITERM_PROFILE') || $ITERM_PROFILE == 'Light'
    set background=light
    colorscheme lucius
  else
    set background=dark
    colorscheme janah
  endif
endif

" Statusline {{{1
command! -bar ToggleStatusline let b:stl_location  = !get(b:, 'stl_location')
command! -bar ToggleHighlight  let b:stl_highlight = !get(b:, 'stl_highlight')

nnoremap <silent><f10> :ToggleStatusline<cr>
nnoremap <silent><f11> :ToggleHighlight<cr>

set statusline=%!SetStatusline()

function! SetStatusline()
  let stl = ' %4*%<%f%*'

  if exists('b:git_dir')
    let stl    .= '%3*:%*'
    let branch  = fugitive#head(8)
    let stl    .= (branch == 'master') ? '%1*master%*' : '%2*'. branch .'%*'
    let stl    .= mhi#sy_stats_wrapper()
  endif

  let stl .= '%m%r%h%w '

  " right side
  let stl .=
        \   '%= '
        \ . '%#ErrorMsg#%{&paste ? " paste " : ""}%*'
        \ . '%#WarningMsg#%{&ff != "unix" ? " ".&ff." ":""}%* '
        \ . '%#warningmsg#%{&fenc != "utf-8" && &fenc != "" ? " ".&fenc." " :""}%* '

  if get(b:, 'stl_highlight')
    let id = synID(line('.'), col('.'), 1)
    let stl .=
          \   '%#WarningMsg#['
          \ . '%{synIDattr('.id.',"name")} as '
          \ . '%{synIDattr(synIDtrans('.id.'),"name")}'
          \ . ']%* '
  endif

  if get(b:, 'stl_location')
    let stl .=
          \   '%3*[%*%v%3*,%*%l%3*/%*%L%3*]%* '
          \ . '%p%3*%%%* '
  endif

  return stl
endfunction
" }}}1

" Plugin {{{1
" Plugin: committia {{{2
let g:committia_open_only_vim_starting = 0

" Plugin: exception {{{2
nnoremap <silent><leader>E :call exception#trace()<cr>
" Plugin: fzf {{{2
set runtimepath+=~/local/fzf

nnoremap <leader><leader> :Buffers<cr>
nnoremap <leader>C        :Colorschemes<cr>
nnoremap <leader>P        :Plugins<cr>
nnoremap <leader>S        :Scriptnames<cr>
nnoremap <leader>f        :FZF<cr>

function! s:buflist()
  return map(filter(range(1, bufnr('$')),
        \ 'bufloaded(v:val)'), 'printf("%s: %s", v:val, bufname(v:val))')
endfunction

function! s:bufopen(lines)
  if len(a:lines) < 2 | return | endif
  let cmd = get({
        \ 'ctrl-s': 'split',
        \ 'ctrl-v': 'vsplit',
        \ 'ctrl-t': 'tabedit'
        \ }, a:lines[0], 'edit')
  execute cmd '+b'.matchstr(a:lines[1], '^\d\+')
endfunction

function! s:plugopen(entry)
  execute 'Dirvish' expand(s:bundle_dir) .'/'. a:entry
  execute 'lcd' expand('%')
endfunction

command! Buffers let bl = reverse(s:buflist()) | call fzf#run({
      \ 'source':  bl,
      \ 'sink*':   function('<sid>bufopen'),
      \ 'options': '+m --expect=ctrl-s,ctrl-v,ctrl-t',
      \ 'down':    len(bl) + 2,
      \ }) | unlet bl

command! Plugins call fzf#run({
      \ 'source':  reverse(sort(map(globpath(s:bundle_dir, '*', 0, 1), 'fnamemodify(v:val, ":t")'))),
      \ 'sink':    function('<sid>plugopen'),
      \ 'options': '+m',
      \ 'left':    30
      \ })

command! Colorschemes call fzf#run({
      \ 'source':  reverse(sort(map(globpath(&rtp, 'colors/*.vim', 0, 1), 'fnamemodify(v:val, ":t:r")'))),
      \ 'sink':    'colorscheme',
      \ 'options': '+m',
      \ 'left':    30
      \ })


" Plugin: netrw {{{2
" let g:loaded_netrwPlugin = 1
let g:netrw_banner       = 0
let g:netrw_bufsettings  = 'relativenumber'
let g:netrw_keepdir      = 0
let g:netrw_liststyle    = 1
let g:netrw_sort_options = 'i'

" Plugin: nvim-langserver {{{2
let g:ls = {
      \ 'c':          {'cmd': ['clangd']},
      \ 'cpp':        {'cmd': ['clangd']},
      \ 'objc':       {'cmd': ['clangd']},
      \ 'go':         {'cmd': ['gopls']},
      \ 'python':     {'cmd': ['pyls']},
      \ 'lua':        {'cmd': ['lua-lsp']},
      \ 'rust':       {'cmd': ['rls']},
      \ 'javascript': {'cmd': ['javascript-typescript-stdio']},
      \ 'typescript': {'cmd': ['javascript-typescript-stdio']},
      \ 'elixir':     {'cmd': [$ME.'/elixir/elixir-ls/build/language_server.sh']},
      \ }

nmap <leader>jd <plug>(ls-definition)
nmap <leader>jh <plug>(ls-hover)
nmap <leader>jr <plug>(ls-references)
nmap <leader>js <plug>(ls-signature-help)
nmap <leader>jf <plug>(ls-formatting)

" Plugin: surround {{{2
let g:surround_indent = 1
let g:surround_{char2nr('k')} = "<kbd>\r</kbd>"

" Plugin: tagbar {{{2
nnoremap <f2> :TagbarToggle<cr>

let g:tagbar_width     = 40
let g:tagbar_autoclose = 0
let g:tagbar_autofocus = 1
let g:tagbar_compact   = 1

" Plugin: undotree {{{2
nnoremap <f3>  :UndotreeToggle<cr>

" Plugin: vim-dirvish {{{2
autocmd FileType dirvish nmap <buffer> <c-o> -
autocmd VimEnter * if &ft == 'dirvish' | exe 'lcd' expand('%') | endif
nnoremap <expr> <f1> (&ft == 'dirvish' ? ':close' : ':leftabove 40vs +Dirvish')."\<cr>"

" Plugin: vim-easy-align {{{2
xmap <cr> <plug>(LiveEasyAlign)

" Plugin: vim-easymotion {{{2
let g:EasyMotion_do_mapping        = 0
let g:EasyMotion_do_shade          = 1
let g:EasyMotion_inc_highlight     = 0
let g:EasyMotion_landing_highlight = 0
let g:EasyMotion_off_screen_search = 0
let g:EasyMotion_smartcase         = 0
let g:EasyMotion_startofline       = 0
let g:EasyMotion_use_smartsign_us  = 1
let g:EasyMotion_use_upper         = 0

let g:EasyMotion_skipfoldedline = 0

map <silent><space> <plug>(easymotion-s2)

" Plugin: vim-grepper {{{2
nnoremap <leader>g :Grepper<cr>
nmap gs <plug>(GrepperOperator)
xmap gs <plug>(GrepperOperator)

highlight GrepperSideFile ctermfg=161 cterm=reverse
highlight Conceal ctermfg=NONE ctermbg=250

let g:grepper = {}
let g:grepper.dir = 'repo,filecwd'
let g:grepper.tools = ['git', 'rg', 'rubocop']
let g:grepper.rubocop = {
      \ 'grepprg': 'rubocop -femacs',
      \ 'grepformat': '%f:%l:%c: %t: %m' }

command! Todo Grepper -noprompt -tool rg -query '(TODO|FIX|FIXME|XXX|NOTE|HACK|OPTIMIZE):'

" Plugin: vim-sayonara {{{2
nnoremap <silent><leader>q  :Sayonara<cr>
nnoremap <silent><leader>Q  :Sayonara!<cr>

let g:sayonara_confirm_quit = 0

" Plugin: vim-startify {{{2
nnoremap <leader>st :Startify<cr>

let g:startify_change_to_dir       = 0
let g:startify_custom_header       = 'startify#pad(startify#fortune#boxed())'
let g:startify_enable_special      = 0
let g:startify_fortune_use_unicode = 1
let g:startify_update_oldfiles     = 1
let g:startify_use_env             = 1

" Filetype {{{1
augroup vimrc
  " Filetype: Assembly {{{2
  let g:asmsyntax = 'nasm'
  " vim-commentary uses 'commentstring', not 'comments'
  autocmd FileType nasm let &commentstring = '; %s'

  " Filetype: C {{{2
  autocmd BufLeave *.{c,cpp} mark C
  autocmd BufLeave *.h       mark H

  " -> after/ftplugin/c.vim

  " Filetype: cmake {{{2
  autocmd FileType cmake setlocal commentstring=#\ %s

  " Filetype: crontab {{{2
  autocmd FileType crontab setlocal nobackup nowritebackup

  " Filetype: Erlang {{{2
  autocmd FileType erlang setl com-=:%

  " Filetype: Go {{{2
  let g:go_bin_path    = $v.'/vendor/go'
  let g:go_fmt_command = 'goimports'
  let g:gofmt_on_save  = 0

  autocmd FileType go setlocal noet sts& sw& com-=:// isk-=. lcs+=tab:\ \ 

  " Filetype: Help {{{2
  autocmd FileType help
        \  setl noet
        \| let b:helpful = 1
        \| execute 'silent wincmd' 'TL'[winnr('$') == 2]

  " Filetype: HTML {{{2
  autocmd FileType html setl et sts=2 sw=2

  " Filetype: Lua {{{2
  autocmd FileType lua setl com-=:# sts=2 sw=2

  " Filetype: Perl {{{2
  let perl_include_pod = 1

  autocmd FileType perl
        \  setlocal com-=:# kp=perldoc\ -f
        \| compiler perl

  " Filetype: Python {{{2
  let python_highlight_all = 1

  autocmd FileType python
        \  setlocal kp=pydoc mp=./%
        \| compiler pyunit

  " Filetype: Ruby {{{2
  let g:ruby_operators                 = 1
  let g:ruby_space_errors              = 1
  let g:rubycomplete_buffer_loading    = 1
  let g:rubycomplete_classes_in_global = 1
  let g:rubycomplete_rails             = 1

  function! s:ruby()
    command! -buffer -bang Rubocop exe 'Grepper -tool rubocop -noprompt' ['-buffer',''][<bang>0]
    command! -buffer -nargs=* -bang Rspec exe '<mods> T clear; bundle exec rspec <args>' expand('%:p').['', ':'.line('.')][<bang>0]
    nnoremap <buffer> <leader>rs :vertical Rspec!<cr>
  endfunction

  autocmd BufNewFile *.rb call append(0, ['#!/usr/bin/env ruby', ''])
  autocmd FileType ruby call s:ruby()

  " -> after/ftplugin/ruby.vim

  " Filetype: Sh, Zsh {{{2
  au FileType zsh,sh setl com-=:# sts=4 sw=4
        \| com! -buffer Lint Grepper -noprompt -buffer -grepprg shellcheck -fgcc

  " Filetype: Text, Mail, Markdown {{{2
  let g:instant_markdown_autostart = 0
  let g:instant_markdown_slow      = 1

  autocmd FileType mail     setl tw=72 wrap lbr
  autocmd FileType markdown setl tw=80 mp=gomali\ %
  autocmd FileType text     setl tw=80 fo+=t

  " Filetype: VCS {{{2
  autocmd FileType git,gitcommit setl tw=80 fo+=t

  " fugitive
  autocmd BufReadPost fugitive:// setlocal bufhidden=delete
  autocmd FileType fugitiveblame nmap q gq
  autocmd User fugitive
        \  if get(b:, 'fugitive_type', '') =~# '\v^%(tree|blob)$'
        \|   nnoremap <buffer> .. :edit %:h<cr>
        \| endif

  " gv
  autocmd FileType GV
        \  set foldlevel=1
        \| nnoremap <buffer> q :qa<cr>
        \| nmap     <buffer> j ]]<cr>
        \| nmap     <buffer> k [[<cr>
        \| nmap     <buffer> <LeftRelease> o

  highlight link diffAdded     DiffAdd
  highlight link diffRemoved   DiffDelete
  highlight link diffFile      Directory
  highlight link diffLine      Comment
  highlight link diffIndexLine Comment
  highlight link diffSubname   Comment

  " signify
  let g:signify_sign_show_text = 0
  omap ic <plug>(signify-motion-inner-pending)
  xmap ic <plug>(signify-motion-inner-visual)
  omap ac <plug>(signify-motion-outer-pending)
  xmap ac <plug>(signify-motion-outer-visual)
  nnoremap <silent><leader>p :SignifyHunkDiff<cr>
  nnoremap <silent><leader>u :SignifyHunkUndo<cr>

  " Filetype: Vim, Vader {{{2
  autocmd FileType vim nnoremap <buffer><silent> <cr> :if lookup#lookup()<bar>call halo#run()<bar>endif<cr>
  autocmd FileType vim nnoremap <buffer><silent> <bs> :call lookup#pop()<bar>call halo#run()<cr>
  autocmd FileType vim setl sts=2 sw=2 isk-=#
  autocmd BufRead *.{vader,vim} command! -buffer Test call mhi#run_exercism_tests()
" }}}2
augroup END
" }}}1

" vim: sw=2 sts=2 tw=0 fdm=marker
