#!/usr/bin/zsh

eval $(dircolors ~/.zsh/dircolors)

[[ $DISPLAY == screen* ]] && stty erase '^?'

autoload -Uz compinit && compinit
autoload -Uz edit-command-line
autoload -Uz run-help

zmodload -i zsh/complist

bindkey -e
umask 077

watch=all
logcheck=60
WATCHFMT="%n from %M has %a tty%l at %T %W"

source '/Users/mhi/local/fzf/shell/key-bindings.zsh'

# misc options {{{1

# changing directories
setopt auto_cd
setopt cdable_vars

setopt checkjobs
setopt completeinword
setopt correct
setopt globcomplete
setopt interactivecomments
setopt print_exit_value
setopt listpacked
setopt longlistjobs
setopt no_beep
setopt no_listrowsfirst
setopt no_nomatch
setopt no_rm_star_silent
setopt nohup
setopt nolistambiguous
setopt nolog
setopt no_hist_beep
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

zstyle -e ':completion:*:approximate:*'   max-errors   '(( reply=($#PREFX+$#SUFFIX)/3 ))'

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
autoload -Uz vcs_info

precmd() {
    vcs_info
}

typeset -A c p

# Color definitions.
c[blue]='%F{111}'
c[gray]='%F{240}'
c[green]='%F{84}'
c[orange]='%F{216}'
c[red]='%F{168}'
c[white]='%F{255}'
c[yellow]='%F{222}'

# Prompt configuration.
p[deli_left]="${c[gray]}%f"
p[deli_right]="${c[gray]}%f"
p[jobcount]="%(1j.${c[orange]}%j${c[gray]}:%f.)"

p[colon]="${c[gray]}:%f"
p[vcs]="${c[green]}%s${p[colon]}"
p[repo]="${c[yellow]}%r${p[colon]}"
p[branch]="${c[red]}%b%f"
p[action]="${p[colon]}${c[orange]}%a%f"
p[host]="${c[gray]}%M:%f"

p[staged]="${p[colon]}%F{49}↻%f"
p[unstaged]="${p[colon]}%F{81}↻%f"

p[l_nvcsformats]="${c[yellow]}λ%f"
p[r_nvcsformats]="${c[gray]}%~%f"

#p[l_formats]="${p[vcs]}${p[repo]}${p[branch]}%u%c"
p[l_formats]="${p[repo]}${p[branch]}%u%c"
p[r_formats]="${c[gray]}%R/${c[yellow]}%S%f"

# p[l_actionformats]="${p[vcs]}${p[repo]}${p[action]}${p[branch]}%u%c"
p[l_actionformats]="${p[repo]}${p[branch]}${p[action]}%u%c"

zstyle ':vcs_info:*'  enable             git hg svn
zstyle ':vcs_info:*'  disable-patterns   '/data/linux/stable(|/*)'
zstyle ':vcs_info:*'  check-for-changes  true

zstyle ':vcs_info:*'  stagedstr          $p[staged]
zstyle ':vcs_info:*'  unstagedstr        $p[unstaged]

zstyle ':vcs_info:*'  nvcsformats        $p[l_nvcsformats]   $p[r_nvcsformats]
zstyle ':vcs_info:*'  formats            $p[l_formats]       $p[r_formats]
zstyle ':vcs_info:*'  actionformats      $p[l_actionformats] $p[r_formats]

PROMPT='%B$p[deli_left]$p[jobcount]${vcs_info_msg_0_}$p[deli_right]%b '
RPROMPT='%B$p[host]${vcs_info_msg_1_}%b'
SPROMPT="${c[orange]}%R -> %r:%f "
PROMPT2="${c[orange]}+%f "
PROMPT3="${c[orange]}Select:%f "

# hashes {{{1
hash -d asm='/data/programming/asm'
hash -d b='/data/books'
hash -d c='/data/programming/c'
hash -d g='/data/github'
hash -d torrents='/data/torrents/downloads'
hash -d z='/data/repo/zsh'

# aliases {{{1
alias -g L='| less -r'
alias -g N='> /dev/null'

alias help='run-help'

alias pprof='/usr/bin/google-pprof'
alias pp='LD_PRELOAD=/usr/lib/libprofiler.so.0 CPUPROFILE=out.prof'

alias ju='java -cp ".:test:../junit.jar" org.junit.runner.JUnitCore'
alias jc='javac -cp ".:../junit.jar"'

alias sc='systemctl'
alias sda='systemd-analyze'

alias mars='java -jar /data/uni/techgi2/code/Mars4_4.jar'

alias xssh='ssh -XCc arcfour,blowfish-cbc'

alias scan='make clean && make cmake CMAKE_EXTRA_FLAGS="-DCMAKE_C_COMPILER=/usr/share/clang/scan-build/ccc-analyzer" && scan-build --use-analyzer=/usr/bin/clang make'

alias add='cmus-remote -l'
alias big='dpkg-query -W --showformat="\${Installed-Size}\t\${Package}\n" | sort -n'

alias c11='( zathura /data/c/standards/n1516.pdf & ) && exit'
alias j8='( zathura /data/programming/java/standards/jls8.pdf & ) && exit'
alias j8v='( zathura /data/programming/java/standards/jvms8.pdf & ) && exit'

alias conf='edit_configs'
alias cr='cmus-remote'
alias dis='gcc -S -masm=intel'
alias e='emacs'
alias g='git'
alias gdb='gdb -q'
alias gitk='gitk --all'
alias grep='grep --color=always'
alias i='$EDITOR /data/life/ideas'
alias kdwm='killall dwm'
alias mirror='noglob wget --mirror --no-parent --recursive --timestamping --continue --recursive $1'
alias mps='ps $@ f -u $USER -wo pid,ppid,state,%cpu,%mem,tty,cmd'
alias n2t='sh /data/nand2tetris/nand2tetris/tools/HardwareSimulator.sh'
alias ob='objdump -Mintel'
alias p1='patch -p1 -g1 --dry-run'
alias rt='cd ~torrents/../session && rtorrent'
alias sx="startx -- -dpi 100 -nolisten tcp >> ~/logs/startx-$(date +%F).log"
alias t='$EDITOR /data/life/todo'
alias val='valgrind -v --leak-check=full --show-reachable=yes'
alias z='zathura'

alias ips="ifconfig -a | perl -nle'/(\d+\.\d+\.\d+\.\d+)/ && print \$1'"
alias myip='dig +short myip.opendns.com @resolver1.opendns.com'
alias flush='dscacheutil -flushcache'

# alias v='vim'
alias vu='vim -u NONE -U NONE -i NONE -N'
alias v='VIMRUNTIME=/data/repo/neovim/runtime nvim'

alias h="cd ..; l"
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."

alias ls='ls --color=always'
alias j='ls -lhd *(D-/)'
alias k='ls -lhX *(D-^/)'
alias l='ls -lh --group-directories-first'
alias ll='ls -lhX --group-directories-first'
alias la='ls -lhXA --group-directories-first'
alias n='ls -lhS *(DOL[1,5]^/)'

alias 1='fg %1'
alias 2='fg %2'
alias 3='fg %3'
alias 11='bg %1'
alias 22='bg %2'
alias 33='bg %3'

if [[ $(uname) == Darwin ]]; then
    alias upg='brew update && brew upgrade --all && brew cleanup'
else
    if [[ -x ${commands[sudo]} ]]
    then
        alias ap='sudo aptitude'
        alias ag='sudo apt-get'

        alias aps='sudo aptitude search'
        alias api='sudo aptitude install'
        alias aga='sudo apt-get autoremove --purge'
        alias upg='sudo apt-get update && sudo apt-get upgrade'
        alias pur='sudo apt-get remove --purge'
        alias cache='sudo apt-cache'

        alias dpkg='sudo dpkg'
        alias dpkg-reconfigure='sudo dpkg-reconfigure'

        alias orp='sudo deborphan'
        alias orph='sudo deborphan --libdevel --find-config'

        alias arp='sudo arp'
        alias pt='sudo powertop'

        alias reboot='sudo shutdown -r now'
        alias halt='sudo shutdown -h now'
    else
        echo "Please install 'sudo'"
    fi
fi

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

c() { cd *$1* }

vt() {
    nv -u unix.vim -U NONE --noplugin -s dotest.in $1
    test -f ${1%.*}.failed && diff -u ${1%.*}.ok ${1%.*}.failed | colordiff
}

foo() {
    base="${1%.*}"
    [[ $# -eq 2 ]] && time="${2}ns" || time="100ns"
    ghdl -a "$1"
    ghdl -e "$base"
    ghdl -r "$base" --vcd=wave.vcd --stop-time="${time}"
}
bar() { gtkwave wave.vcd }

chmsee() { command chmsee $@ && rm -r ~/.chmsee }
pcolors() { for i ({0..255}) ( echoti setab $i && echo -n " $i "  )}
#colors() { for i ({0..255}) ( tput setab $i && printf " %-6s" $i && [[ $[$i % 8] -eq 0 ]] && echo )}
df()     { [[ -x ${commands[pydf]} ]] && pydf $@ || command df -h $@ }
f()      { find . -iname "*$@*" }
hx()     { printf "%d\n" "$1" }
md()     { command mkdir $1 && builtin cd $1 }
mount()  { command mount $@ | awk '{ print $1, $3, $5, $6 }' | sort -V | column -t }
o()      { objdump -Mintel -wrzD $2 | awk "/^.*<$1>:$/,/^$/" }
om()     { objdump -Mintel -wrzD -j .text $1 | awk '/^.*<main>:$/,/^$/'; }
secs()   { echo $(($(date +'%s') - $(date --date="$1 12:00:00" +'%s'))) }
slrn()   { awk '/hinz/ { print $4 }' ~/.slrnrc && command slrn }
top()    { [[ -x ${commands[htop]} ]] && htop $@ || command top $@ }

rd() {
    dir=$PWD
    builtin cd ..
    if command rmdir $dir 2>/dev/null; then
        echo 'Removed empty directory:' $dir
    else
        builtin cd $dir
        echo 'Directory is not empty:'
        ls -A
    fi
    unset dir
}

urxvtc() {
    command urxvtc $@
    [[ $? -eq 2 ]] && urxvtd -q -f && command urxvtc $@
}

edit_configs() {
    local -A configs
    configs=(
        b           ~/.bashrc
        dwm         ~/.dotfiles/dwm/config.h
        g           ~/.gitconfig
        m           ~/.muttrc
        p           ~/.pentadactyl/pentadactylrc
        t           ~/.tmux.conf
        v           ~/.vim/vimrc
        x           ~/.Xdefaults
        xi          ~/.xinitrc
        zp          ~/.zsh/.zprofile
        zr          ~/.zsh/.zshrc
    )
    #select i in ${(k)configs}; do $EDITOR ${configs[$i]}; break; done

    tput setaf 204
    [[ -z $1 ]] && echo "${(k)configs}\n" && return 0

    for i in ${(k)configs}
    do
        [[ $1 == $i ]] && $EDITOR ${configs[$i]} && return 0
    done

    echo "ERROR: Config not found." && return 1
}

asm() {
    green=$(echoti setaf 119)
    reset=$(echoti sgr0)
    tmp=${1%.*}

    nasm -g -felf -Fdwarf -o $tmp.o $1 && printf "%-11s %s\n" Assembling: ${green}OK${reset}
    #as -gstabs -o $tmp.o $1 && printf "%-11s %s\n" Assembling: ${green}OK${reset}
    #[[ $? == 0 ]] && ld -o $tmp $tmp.o && printf "%-11s %s\n" Linking: ${green}OK${reset}
    [[ $? == 0 ]] && gcc -g -nostdlib -o $tmp $tmp.o && printf "%-11s %s\n" Linking: ${green}OK${reset}
}

tm() {
    if [[ $# -ge 1 ]]; then
        tmux has-session -t $1 && tmux attach -t $1 || tmux new-session -s $1
    else
        tmux attach
    fi
}

_tmux-sessions() {
    local expl
    local -a sessions
    sessions=( ${${(f)"$(command tmux list-sessions)"}/:[ $'\t']##/:} )
    _describe -t sessions 'sessions' sessions "$@"
}
compdef _tmux-sessions tm

_tmux_pane_words() {
    local expl
    local -a w
    if [[ -z $TMUX_PANE ]]
    then
        _message "not running inside tmux!"
        return 1
    fi
    w=( ${(u)=$(tmux capture-pane \; show-buffer \; delete-buffer)} )
    _wanted values expl 'words from current tmux pane' compadd -a w
}

zle -C tmux-pane-words-prefix   complete-word _generic
zle -C tmux-pane-words-anywhere complete-word _generic
bindkey '^Xt' tmux-pane-words-prefix
bindkey '^X^X' tmux-pane-words-anywhere
zstyle ':completion:tmux-pane-words-(prefix|anywhere):*' completer _tmux_pane_words
zstyle ':completion:tmux-pane-words-(prefix|anywhere):*' ignore-line current
zstyle ':completion:tmux-pane-words-anywhere:*' matcher-list 'b:=* m:{A-Za-z}={a-zA-Z}'

rationalise-dot() {
    local MATCH dir split
    split=(${(z)LBUFFER})
    if (( $#split > 1 )); then
        dir=$split[-1]
    else
        dir=$split
    fi
    if [[ $LBUFFER =~ '(^|/| | |'$'\n''|\||;|&)\.\./$' ]]; then
        zle self-insert
        zle self-insert
        LBUFFER+=/
        [[ -e $dir ]] && zle -M $dir(:a:h)
    elif [[ $LBUFFER[-1] == '.' ]]; then
        zle self-insert
        LBUFFER+=/
        [[ -e $dir ]] && zle -M $dir(:a:h)
    else
        zle self-insert
    fi
}

zle -N rationalise-dot
bindkey '.' rationalise-dot

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

here() {
    if (($+TMUX)); then
        tmux new-window -c "#{pane_current_path}"
    else
        command urxvtc -cd $PWD
    fi
}

# vim: et sts=4 sw=4
