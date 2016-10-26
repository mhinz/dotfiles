#!/usr/bin/env zsh

if [[ -r $HOME/.shrc ]]; then
    emulate sh -c 'source ~/.shrc'
else
    echo 'No .shrc found.'
fi

alias -g L='| less -r'
alias -g N='>/dev/null'
alias -g E='2>/dev/null'

fpath=($ZDOTDIR/compsys $fpath)

zmodload -i zsh/complist

autoload -Uz compinit && compinit
autoload -Uz edit-command-line
autoload -Uz run-help

bindkey -e
umask 077

watch=all
logcheck=60
WATCHFMT="%n from %M has %a tty%l at %T %W"

# misc options {{{1

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

# zle {{{1

_ciw() {
    setopt localoptions extendedglob
    LBUFFER=${LBUFFER%%[^ ]#}
    RBUFFER=${RBUFFER##[^ ]#}
    #zle vi-insert
}

_insert_last_typed_word() {
        zle insert-last-word -- 0 -1
}

_jump_after_first_word() {
    CURSOR=$#BUFFER[(w)1]
}

_run_with_sudo() {
    LBUFFER="sudo $LBUFFER"
}

zle_keymap_select() {
    [[ $KEYMAP == vicmd ]] && local main="$(tput setaf 197)"
    zle reset-prompt
}

zle -N edit-command-line
zle -N _insert_last_typed_word
zle -N _jump_after_first_word
zle -N _zle_keymap_select

bindkey -M menuselect 'h' backward-char
bindkey -M menuselect 'j' down-line-or-history
bindkey -M menuselect 'k' up-line-or-history
bindkey -M menuselect 'l' forward-char
bindkey -M menuselect 'i' accept-and-menu-complete

bindkey ';f'   _insert_last_typed_word
bindkey ';g'   _jump_after_first_word

bindkey ''   vi-backward-kill-word
bindkey ''   up-line-or-search
bindkey ''   down-line-or-search
bindkey 'e'  edit-command-line
bindkey 'n'  list-expand
bindkey 'm'  expand-word

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
zstyle ':completion:*'                    special-dirs true
zstyle ':completion:*'                    group-name   ''
zstyle ':completion:*:descriptions'       format       $'%{[(00);(38;05;167)m%}=> %d%{[0m%}'
# zstyle ':completion:*:descriptions' format       $'%{\e[0;31m%}completing %B%d%b%{\e[0m%}'

# prompt {{{1
autoload -U colors && colors

precmd() {
    if _prompt_top=$(git rev-parse --show-toplevel 2>/dev/null); then
        _prompt_in_worktree=$(git rev-parse --is-inside-work-tree)
        _prompt_type=git
    else
        unset _prompt_top _prompt_in_worktree _prompt_type
    fi
    _prompt_pwd=$(pwd -P)
}

prompt() {
    print -n '%(1?.%F{15}%K{16} %? .)'
    print -n '%(1j.%F{15}%K{103} %j .)'
    case $_prompt_type in
        git) prompt_git ;;
        *)   print '%F{15}%K{161} λ ' ;;
    esac
}

prompt_git() {
    local p

    # empty within .git/
    [[ -n $_prompt_top ]] && p+="%F{15}%K{161} ${_prompt_top##*/} "

    # empty if not root commit yet
    local branch=$(git name-rev --name-only HEAD 2>/dev/null)
    [[ -n $branch ]] && p+="%F{15}%K{67} $branch "

    if [[ $_prompt_in_worktree == true ]]; then
        git diff --no-ext-diff --quiet &>/dev/null 2>/dev/null
        (( $? && $? != 128)) && p+='%F{15}%K{209} ! '

        git diff-index --cached --quiet HEAD || p+='%F{15}%K{29} ✓ '

        local gitdir="${_prompt_top}/.git"
        if [[ -f "${gitdir}/MERGE_HEAD" ]]; then
            p+='%F{15}%K{16} merge '
        elif [[ -f "${gitdir}/CHERRY_PICK_HEAD" ]]; then
            p+='%F{15}%K{16} cherry '
        elif [[ -f "${gitdir}/REVERT_HEAD" ]]; then
            p+='%F{15}%K{16} revert '
        elif [[ -f "${gitdir}/rebase-merge/interactive" ]]; then
            p+='%F{15}%K{16} rebase-i '
        elif [[ -d "${gitdir}/rebase-apply" ]]; then
            p+='%F{15}%K{16} rebase '
        fi
    fi

    print $p
}

rprompt() {
    if [[ $_prompt_type == git && $_prompt_in_worktree == true ]]; then
        print "%F{243}$_prompt_top/%F{161}${${_prompt_pwd#$_prompt_top}#/}"
    else
        print "%F{243}$PWD"
    fi
}

PROMPT='$(prompt)%f%k '
RPROMPT='$(rprompt)'
SPROMPT="%R -> %r:%f "
PROMPT2="+%f "
PROMPT3="Select:%f "

# hashes {{{1
hash -d asm='/data/programming/asm'
hash -d b='/data/books'
hash -d c='/data/programming/c'
hash -d g='/data/github'
hash -d torrent='/data/torrent/download'
hash -d z='/data/repo/zsh'

# completion {{{1

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

# functions {{{1
command_not_found_handler() { ~/bin/shell_function_missing $* }

m() {
    bc -l <<< $@
}; alias m='noglob m'

fancy-dot() {
    local -a split
    split=( ${=LBUFFER} )
    local dir=$split[-1]
    if [[ $LBUFFER =~ '(^| )(\.\./)+$' ]]; then
        zle self-insert
        zle self-insert
        LBUFFER+=/
        [ -e $dir ] && zle -M $dir(:a:h)
    elif [[ $LBUFFER =~ '(^| )\.$' ]]; then
        zle self-insert
        LBUFFER+=/
        [ -e $dir ] && zle -M $dir(:a:h)
    else
        zle self-insert
    fi
}
zle -N fancy-dot
bindkey '.' fancy-dot

fancy-ctrl-z() {
    if [[ $#BUFFER -eq 0 ]]; then
        BUFFER=fg
        zle accept-line
    else
        zle push-input
        zle clear-screen
    fi
}
zle -N fancy-ctrl-z
bindkey '^Z' fancy-ctrl-z
# }}}

# Chrome {{{1
ch() {
  export CONF_COLS=$[ COLUMNS/2 ]
  export CONF_SEP='{::}'

  cp -f ~/Library/Application\ Support/Google/Chrome/Default/History /tmp/h

  sqlite3 -separator $CONF_SEP /tmp/h 'select title, url from urls order by last_visit_time desc' \
      | ruby -ne '
  cols = ENV["CONF_COLS"].to_i
  title, url = $_.split(ENV["CONF_SEP"])
  puts "\x1b[33m#{title.ljust(cols)}\x1b[0m #{url}"' \
      | fzf --ansi --multi --no-hscroll --tiebreak=index \
      | grep --color=never -o 'https\?://.*' \
      | xargs open

  unset CONF_COLS CONF_SEP
}

# Git {{{1
pr() {
    local origin pr
    if [[ $# == 0 ]]; then
        echo "usage: pr [remote] <ref>"
        return 1
    elif [[ $# == 1 ]]; then
        origin=$(git config branch.master.remote || echo origin)
        pr=$1
    else
        origin=$1
        pr=$2
    fi
    git fetch $origin refs/pull/${pr}/head || return
    git checkout -q FETCH_HEAD
}

b() {
    git checkout $(git branch -a | fzf -1 | cut -c3-)
}

alias gv="nvim +GV +'sil tabc 2' +'exe \"normal \<cr>\"'"

# iTerm2 {{{1
proftoggle() {
    if [[ -z $ITERM_PROFILE ]]; then
        print "Not in iTerm" 1>&2
        return
    fi
    tmup
    if [[ $ITERM_PROFILE == Light ]]; then
        export ITERM_PROFILE=Dark
        eval $(dircolors ~/.zsh/dircolors.dark)
        ln -fs ~/.config/git/config.colors{.dark,}
    else
        export ITERM_PROFILE=Light
        eval $(dircolors ~/.zsh/dircolors.light)
        ln -fs ~/.config/git/config.colors{.light,}
    fi
    local seq="\e]1337;SetProfile=${ITERM_PROFILE}\x7"
    if [[ -n $TMUX ]]; then
        seq="\ePtmux;\e${seq}\e\\"
        tmux setenv ITERM_PROFILE $ITERM_PROFILE
    fi
    printf $seq
    clear
}

# Tmux {{{1
tm() {
    if (( $# )); then
        tmux has-session -t "$*" && tmux attach -t "$*" || tmux new-session -s "$*"
    else
        tmux attach || tmux new-session -s default
    fi
}

tmup() {
    [[ -n $TMUX ]] && export $(tmux showenv | grep --color=never '^[^-]' | xargs)
}

_tmux_sessions() {
    local -a sessions=( ${(f)"$(command tmux list-sessions)"} )
    _describe -t sessions '' sessions "$@"
}
compdef _tmux_sessions tm

_tmux_complete() {
    [ -z $TMUX ] && { _message 'I double dare you!'; return 1 }
    local pane words=()
    for pane ($(tmux list-panes -F '#P')) {
        words+=( ${(u)=$(tmux capture-pane -Jpt $pane)} )
    }
    _wanted values expl '' compadd -a words
}
zle -C tmux-comp-prefix   complete-word _generic
zle -C tmux-comp-anywhere complete-word _generic
bindkey '^X^U' tmux-comp-prefix
bindkey '^X^X' tmux-comp-anywhere
zstyle ':completion:tmux-comp-(prefix|anywhere):*' completer _tmux_complete
zstyle ':completion:tmux-comp-(prefix|anywhere):*' ignore-line current-shown
zstyle ':completion:tmux-comp-anywhere:*' matcher-list 'b:=* m:{A-Za-z}={a-zA-Z}'

# Vim {{{1
alias vu='vim -u NONE -U NONE -i NONE -N'
alias v='VIMRUNTIME=/data/repo/neovim/runtime /data/repo/neovim/build/bin/nvim'

# Run a legacy test in ~v/src/testdir
vt() {
    vim -u unix.vim -U NONE --noplugin -s dotest.in $1
    test -f ${1%.*}.failed && diff -u ${1%.*}.ok ${1%.*}.failed | diff-so-fancy
}

# vim: et sts=4 sw=4 fdm=marker
