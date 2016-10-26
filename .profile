# For login shells.

export TERM=xterm-256color
export EDITOR=nvim

export MANPAGER="nvim +'set ft=man' -"
export MANWIDTH=80

export GH=/data/github

export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -g ""'
export FZF_DEFAULT_OPTS='--inline-info --color=light'

export GOPATH=/data/go
export GOROOT=/data/languages/go
export GOARCH=amd64
export GOOS=$(uname -s | tr '[:upper:]' '[:lower:]')

# PATH {{{1
[ -z $ZSH_VERSION ] || setopt sh_word_split

newpath=~/bin

for dir in ~/local/*/bin; do
  newpath="$dir":$newpath
done

gembin=`ruby -rubygems -e 'puts Gem.user_dir'`
[ -n "$gembin" ] && newpath=$gembin:$newpath

newpath=~/.npm-packages/bin:$newpath
newpath=/data/languages/elixir/bin:$newpath
newpath=$GOPATH/bin:$newpath
newpath=$GOROOT/bin:$newpath
newpath=/data/repo/camlistore/bin:$newpath
newpath=/usr/local/opt/coreutils/libexec/gnubin:$newpath
newpath=/usr/local/sbin:$newpath

# Apple's path_helper gets called from /etc/profile and
# /etc/zprofile and mangles $PATH. Work around it.
if [ -x /usr/libexec/path_helper ]; then
  PATH=
  eval `/usr/libexec/path_helper -s`
fi

# Only unique elements, please.
IFS=:
for dir in $newpath; do
  case :$PATH in
    *:"$dir":*) ;;
    *) [ -d "$dir" ] && PATH=$dir:$PATH ;;
  esac
done

export PATH
unset newpath gembin dir

# MANPATH {{{1
export MANPATH=/usr/local/opt/coreutils/libexec/gnuman:$MANPATH
