if [[ -r $HOME/.shrc ]]; then
  . $HOME/.shrc
else
  echo 'No .shrc found.'
fi

_p() {
    PS1="$(_prompt)"
}
PROMPT_COMMAND=_p

shopt -s globstar

stty werase undef
bind '\C-w:unix-filename-rubout'
