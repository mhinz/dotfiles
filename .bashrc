if [[ -r $HOME/.shrc ]]; then
  . $HOME/.shrc
else
  echo 'No .shrc found.'
fi

_p() {
    PS1="$(prompt)"
}
PROMPT_COMMAND=_p
