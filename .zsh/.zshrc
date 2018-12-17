#!/usr/bin/env zsh

if [[ -r ~/.shrc ]]; then
    source ~/.shrc
else
    echo 'No .shrc found.'
fi

[[ -d $ZDOTDIR/completion ]] && fpath=($ZDOTDIR/completion $fpath)

zmodload -i zsh/complist

autoload -Uz compinit && compinit
autoload -Uz edit-command-line
autoload -Uz run-help

bindkey -e
umask 077

watch=all
logcheck=60
WATCHFMT="%n from %M has %a tty%l at %T %W"

alias -g L='| less -r'
alias -g N='>/dev/null'
alias -g E='2>/dev/null'

# options {{{1

setopt cdablevars
setopt checkjobs
setopt completeinword
setopt correct
setopt globcomplete
setopt interactivecomments
setopt listpacked
setopt longlistjobs
setopt menucomplete
setopt no_autocd
setopt no_beep
setopt no_hist_beep
setopt no_listrowsfirst
setopt no_nomatch
setopt no_print_exit_value
setopt no_rm_star_silent
setopt nohup
setopt nolistambiguous
setopt nolog
setopt notify
setopt numericglobsort
setopt promptsubst
# setopt extendedglob

# history {{{1

HISTFILE=~/.zsh/history
HISTSIZE=2048
SAVEHIST=2048

setopt append_history
setopt bang_hist
setopt hist_ignore_all_dups
setopt hist_ignore_space
setopt hist_reduce_blanks
setopt hist_verify
setopt inc_append_history   # add commands as they're typed
setopt share_history        # share history between sessions

# prompt {{{1

autoload -U colors && colors

precmd() {
    PS1='$(_prompt)'
}
SPROMPT="%R -> %r:%f "
PROMPT2="+%f "
PROMPT3="Select:%f "

# bindings {{{1

_zle_backward_kill_word() {
    LBUFFER="$({ sed -E 's#[^ _/]+[ _/]+$##' | sed -E 's#[^ _/]+$##' } <<< $LBUFFER)"
}

_zle_ctrl_z() {
    if [[ $#BUFFER -eq 0 ]]; then
        BUFFER=fg
        zle accept-line
    else
        zle push-input
        zle clear-screen
    fi
}

_zle_fancy_dot() {
    local -a split
    split=( ${=LBUFFER} )
    local dir=$split[-1]
    case $LBUFFER in
        .)
            LBUFFER='cd ../'
            [[ -d $dir ]] && zle -M $dir(:a:h)
            ;;
        *.)
            zle self-insert
            LBUFFER+=/
            [[ -d $dir ]] && zle -M $dir(:a:h)
            ;;
        *../)
            zle self-insert
            zle self-insert
            LBUFFER+=/
            [[ -d $dir ]] && zle -M $dir(:a:h)
            ;;
        *)
            zle self-insert
    esac
}

_zle_insert_last_typed_word() {
        zle insert-last-word -- 0 -1
}

_zle_jump_after_first_word() {
    CURSOR=$#BUFFER[(w)1]
}

_zle_sudo() {
    LBUFFER="sudo $LBUFFER"
}

zle -N _zle_backward_kill_word
zle -N _zle_ctrl_z
zle -N _zle_fancy_dot
zle -N _zle_insert_last_typed_word
zle -N _zle_jump_after_first_word
zle -N _zle_sudo
zle -N edit-command-line

bindkey ''   up-line-or-search
bindkey ''   down-line-or-search
bindkey ''   _zle_backward_kill_word
bindkey 'e'  edit-command-line
bindkey 'm'  expand-word
bindkey 'n'  list-expand
bindkey ''   _zle_ctrl_z
bindkey '.'    _zle_fancy_dot
bindkey ';f'   _zle_jump_after_first_word
bindkey ';g'   _zle_insert_last_typed_word
bindkey ';s'   _zle_sudo

bindkey -M menuselect 'h' backward-char
bindkey -M menuselect 'i' accept-and-menu-complete
bindkey -M menuselect 'j' down-line-or-history
bindkey -M menuselect 'k' up-line-or-history
bindkey -M menuselect 'l' forward-char

# completion {{{1

zstyle -e ':completion:*:approximate:*'   max-errors   '(( reply=($#PREFIX+$#SUFFIX)/3 ))'

zstyle ':completion:*:kill:*'             command      'ps f -u $USER -wo pid,ppid,state,%cpu,%mem,tty,cmd'
zstyle ':completion:*:*:kill:*:processes' list-colors  '=(#b) #([0-9]#)*=0=01;31'

zstyle ':completion:*'                    matcher-list 'm:ss=Ã m:ue=Ã¼ m:ue=Ã m:oe=Ã¶ m:oe=Ã m:ae=Ã¤ m:ae=Ã m:{a-z}={A-Z} r:|[-_.+,]=** r:|=*'
zstyle ':completion:*:default'            list-colors  ${(s.:.)LS_COLORS} 'ma=01;38;05;255;48;05;161'
# zstyle ':completion:*:default'            list-colors  ${(s.:.)LS_COLORS} 'ma=(01);(38;05;255);(48;05;24)'
zstyle ':completion::complete:*'          use-cache    true
zstyle ':completion:*'                    cache-path   ~/.zsh/cache
zstyle ':completion:*'                    verbose      true
zstyle ':completion:*'                    menu         select=2
zstyle ':completion:*'                    special-dirs ..
zstyle ':completion:*'                    group-name   ''
zstyle ':completion:*:descriptions'       format       $'%{[(00);(38;05;167)m%}=> %d%{[0m%}'
# zstyle ':completion:*:descriptions' format       $'%{\e[0;31m%}completing %B%d%b%{\e[0m%}'

compctl -g '*.class'      java
compctl -g '*.(c|o|a)':   cc gcc
compctl -g '*.el'         erl erlc
compctl -g '*.(hs|hls)'   hugs ghci
compctl -g '*.java'       javac
compctl -g '*.pl'         perl
compctl -g '*.py'         python
compctl -g '*.rb'         ruby

compctl -g '*.pdf'        acrorad xpdf zathura z
compctl -g '*.chm'        chmsee c
compctl -g '*.djvu'       djview
compctl -g '*.lyx'        lyx
compctl -g '*.ps'         gs ghostview ps2pdf ps2ascii
compctl -g '*.tex'        tex latex slitex pdflatex
compctl -g '*.dvi'        dvips dvipdf xdvi dviselect dvitype

compctl -g '*.(bz2|tbz2)' tar bzip2 bunzip2
compctl -g '*.(gz|tgz)'   tar gzip gunzip
compctl -g '*.pax'        pax
compctl -g '*.rar'        rar unrar
compctl -g '*.zip'        zip unzip

compctl -g '*.(htm|html|php)' firefox iceweasel opera lynx w3m link2 dillo uzbl surf

compctl -fg '*.(avi|mp*g|mp4|wmv|ogm|mkv|xvid|divx)' mplayer gmplayer vlc
compctl -g '*.(jp*g|gif|xpm|png|bmp)'                display gimp feh geeqie fbsetbg
compctl -g '*.(mp3|m4a|ogg|au|wav)'                  cmus cmus-remote xmms cr

_completion_tmux_sessions() {
    local -a sessions=( ${(f)"$(command tmux list-sessions)"} )
    _describe -t sessions '' sessions "$@"
}

compdef _completion_tmux_sessions tm

# }}}

command_not_found_handler() { ~/bin/shell_function_missing $* }

m() {
    bc -l <<< $@
}; alias m='noglob m'

# vim: ft=sh fdm=marker
