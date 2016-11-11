#!/bin/sh

TERMINFO=~/.terminfo
rm -rf "$TERMINFO"

tmp=$(mktemp)

cat > "$tmp" <<EOF
xterm-256color|xterm with 256 colors and italic,
    kbs=\177,
    sitm=\E[3m, ritm=\E[23m,
    use=xterm-256color,
tmux-256color|tmux with 256 colors and italic,
    kbs=\177,
    sitm=\E[3m, ritm=\E[23m,
    smso=\E[7m, rmso=\E[27m,
    use=screen-256color,
EOF

tic -x "$tmp"
