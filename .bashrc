if [[ -r $HOME/.shrc ]]; then
  . $HOME/.shrc
else
  echo 'No .shrc found.'
fi

prompt() {
    PS1="$(prompt_git)\[\e[1m\]\u@\h:\w\n\[\e[0m\]"
}
PROMPT_COMMAND=prompt
