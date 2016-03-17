#!/usr/bin/env zsh

# For login shells. Sourced before zshrc.

[[ -n $TMUX || $SHLVL -gt 1 ]] && return

export TERM=xterm-256color
export EDITOR=vim
export MANPAGER="/bin/sh -c \"col -b | vim -c 'set ft=man' -\""
export GOPATH=/data/go GOROOT=/data/languages/go GOARCH=amd64 GOOS=${$(uname -s):l}
export FZF_DEFAULT_OPTS='--inline-info --color bg+:235,hl+:137,hl:167,info:245,pointer:245,prompt:235'
export GH=/data/github

typeset -aU path
path=(
    ~/bin
    ~/local/*/bin
    ~/.npm-packages/bin

    /data/languages/elixir/bin
    $GOPATH/bin
    $GOROOT/bin

    /usr/local/opt/coreutils/libexec/gnubin
    /usr/local/sbin

    $path
)

typeset -aU manpath
manpath=(
    /usr/local/opt/coreutils/libexec/gnuman
    $manpath
)

# vim: fdm=marker
