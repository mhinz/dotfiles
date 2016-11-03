# Keep it POSIX.

# PATH {{{1
read -d '' newpath <<EOF
  /usr/local/sbin
  /usr/local/opt/coreutils/libexec/gnubin
  /data/repo/camlistore/bin
  $GOROOT/bin
  $GOPATH/bin
  /data/languages/elixir/bin
  $HOME/.npm-packages/bin
  $(ruby -rubygems -e 'puts Gem.user_dir' 2>/dev/null)
  $HOME/local/*/bin
  $HOME/bin
EOF

# Apple's path_helper gets called from /etc/profile and
# /etc/zprofile and mangles $PATH. Work around it.
if [ -x /usr/libexec/path_helper ]; then
  PATH=
  eval $(/usr/libexec/path_helper -s)
fi

# Only unique elements, please.
for dir in $newpath; do
  case $PATH in
    *:"$dir":*) ;;
             *) [ -d $dir ] && PATH=$dir:$PATH ;;
  esac
done

export PATH
unset newpath dir

# MANPATH {{{1
export MANPATH=/usr/local/opt/coreutils/libexec/gnuman:$MANPATH
# }}}1

export TERM=xterm-256color
export LANG=en_US.UTF-8

if command -v nvim 1>/dev/null; then
    export EDITOR='nvim'
elif command -v vim 1>/dev/null; then
    export EDITOR='vim'
elif command -v vi 1>/dev/null; then
    export EDITOR='vi'
elif command -v nano 1>/dev/null; then
    export EDITOR='nano'
elif command -v emacs 1>/dev/null; then
    export EDITOR='emacs -nw'
else
    echo 'Install a proper editor.'
fi

case $EDITOR in
    nvim) export MANPAGER="nvim +'set ft=man' -" ;;
     vim) export MANPAGER="/bin/sh -c \"col -b | vim -c 'set ft=man' -\"" ;;
       *) export MANPAGER='less' ;;
esac
export MANWIDTH=80

export GH=/data/github

export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -g ""'
export FZF_DEFAULT_OPTS='--inline-info --color=light'

export GOPATH=/data/go
export GOROOT=/data/languages/go
export GOARCH=amd64
export GOOS=$(uname -s | tr '[:upper:]' '[:lower:]')
