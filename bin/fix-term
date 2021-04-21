#!/bin/sh

# kbs        backspace key
# sitm/ritm  italic
# smso/rmso  standout
# smxx/rmxx  strikethrough
# Smulx      undercurl (https://sw.kovidgoyal.net/kitty/protocol-extensions.html#colored-and-styled-underlines)

os="$(uname -s)"
tinfo="$(mktemp)"

if [ "$os" = Darwin ]; then
    export TERMINFO=/usr/share/terminfo
fi

cat > "$tinfo" <<EOF
xterm-256color|xterm with 256 colors and italic,
    kbs=\177,
    sitm=\E[3m, ritm=\E[23m,
    smxx=\E[9m, rmxx=\E[29m,
    Smulx=\E[4\:%p1%dm,
    use=xterm-256color,
tmux-256color|tmux with 256 colors and italic,
    kbs=\177,
    sitm=\E[3m, ritm=\E[23m,
    smso=\E[7m, rmso=\E[27m,
    smxx=\E[9m, rmxx=\E[29m,
    Smulx=\E[4\:%p1%dm,
    use=screen-256color,
EOF

if [ "$os" = FreeBSD ]; then
    tcap="$(mktemp)"
    tic -CKr0x "$tinfo" > "$tcap"
    cap_mkdb -f ~/.termcap "$tcap"
else
    rm -rf ~/.terminfo
    tic -xo ~/.terminfo "$tinfo"
fi

# vim: nowrap
