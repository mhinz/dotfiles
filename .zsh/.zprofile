#!/usr/bin/env zsh

# For login shells. Sourced before zshrc.

[[ -n $TMUX || $SHLVL -gt 1 ]] && return

export TERM=xterm-256color
export EDITOR=nvim
export MANPAGER="nvim +'set ft=man' -"
export MANWIDTH=80
export GOPATH=/data/go GOROOT=/data/languages/go GOARCH=amd64 GOOS=${$(uname -s):l}
export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -g ""'
export FZF_DEFAULT_OPTS='--inline-info --color=light'
export GH=/data/github

typeset -aU path
path=(
    ~/bin
    ~/local/*/bin

    $(ruby -rubygems -e 'puts Gem.user_dir')/bin
    ~/.npm-packages/bin
    /data/languages/elixir/bin
    $GOPATH/bin
    $GOROOT/bin

    /data/repo/camlistore/bin
    /usr/local/opt/coreutils/libexec/gnubin
    /usr/local/sbin

    $path
)

typeset -aU manpath
manpath=(
    /usr/local/opt/coreutils/libexec/gnuman
    $manpath
)
