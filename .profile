# This file gets sourced by every login shell.

# PATH {{{1
newpath=(
    /usr/local/sbin  # homebrew
    /usr/local/opt/coreutils/libexec/gnubin  # homebrew; keg-only
    /usr/local/opt/ncurses/bin  # homebrew; keg-only
    /usr/local/opt/ruby/bin  # homebrew; keg-only
    ~/data/go/bin  # $GOPATH
    ~/.gem/ruby/*/bin  # --user-install from ~/.gemrc
    ~/.npm/bin
    ~/.asdf/bin
    ~/.asdf/shims
    ~/.cargo/bin
    ~/local/*/bin
    ~/bin
)

# Apple's path_helper gets called from /etc/profile and
# /etc/zprofile and mangles $PATH. Work around it.
if [[ -x /usr/libexec/path_helper ]]; then
    PATH=
    eval "$(/usr/libexec/path_helper -s)"
fi

# Only unique elements, please.
for dir in "${newpath[@]}"; do
    case $PATH in
        *:"$dir":*) ;;
        *) [[ -d $dir ]] && PATH="$dir:$PATH" ;;
    esac
done

export PATH
unset newpath dir
# }}}

export ME=$HOME/data
export LANG=en_US.UTF-8

export FZF_DEFAULT_COMMAND="rg --files --hidden --glob '!.git'"
export FZF_DEFAULT_OPTS='--inline-info --color=light'

export ELIXIR_EDITOR='ec +__LINE__ __FILE__'

export GOPATH=$ME/go
export GOARCH=amd64
export GOOS="$(uname -s | tr '[:upper:]' '[:lower:]')"

export PGDATA=/usr/local/var/postgres

if   command -v nvim  1>/dev/null; then export EDITOR='nvim'
elif command -v vim   1>/dev/null; then export EDITOR='vim'
elif command -v vi    1>/dev/null; then export EDITOR='vi'
elif command -v emacs 1>/dev/null; then export EDITOR='emacs -nw'
elif command -v nano  1>/dev/null; then export EDITOR='nano'
else echo 'Install a proper editor.'
fi

export MANPATH=/usr/local/opt/coreutils/libexec/gnuman:"$MANPATH"
export MANPATH=/usr/local/opt/erlang/lib/erlang/man:"$MANPATH"
export MANPATH=/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/share/man:"$MANPATH"
export MANWIDTH=82

case "$EDITOR" in
    nvim) export MANPAGER="nvim +'set ft=man' -" ;;
    vim)  export MANPAGER="/bin/sh -c \"col -b | vim -c 'set ft=man ro nomod nolist' -\"" ;;
    *)    export MANPAGER='less' ;;
esac
