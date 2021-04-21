if [[ -r $HOME/.shrc ]]; then
  . $HOME/.shrc
else
  echo 'No .shrc found.'
fi

shopt -s globstar

stty werase undef
bind '\C-w:unix-filename-rubout'

PROMPT_COMMAND=_prompt

[[ -n $KITTY_WINDOW_ID ]] && trap 'printf "\033]0;%s\007" "${BASH_COMMAND/_prompt/bash}"' DEBUG
