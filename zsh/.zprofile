#!/usr/bin/zsh

# For login shells. Sourced before zshrc.

[[ -n $TMUX || $SHLVL -gt 1 ]] && return

export TERM=xterm-256color
export EDITOR=vim
export MANPAGER="/bin/sh -c \"col -b | vim -c 'set ft=man' -\""
export GOPATH=/data/go GOROOT=/data/languages/go GOARCH=amd64 GOOS=${$(uname -s):l}
export FZF_DEFAULT_OPTS='--color bg+:235,hl+:137,hl:167,info:245,pointer:245,prompt:235' 

typeset -aU path
path=(
    $HOME/bin
    $HOME/local/*/bin

    /data/languages/elixir/bin
    $HOME/.cabal/bin
    $GOPATH/bin
    /data/languages/go/bin

    /usr/local/opt/coreutils/libexec/gnubin

    $path

    /Library/PostgreSQL/9.4/bin
    /Library/Frameworks/Python.framework/Versions/3.4/bin

    /usr/local/sbin
)

typeset -aU manpath
manpath=(
    /usr/local/opt/coreutils/libexec/gnuman
    $manpath
)
