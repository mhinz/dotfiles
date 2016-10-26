if [[ -r $HOME/.shrc ]]; then
  . $HOME/.shrc
else
  echo 'No .shrc found.'
fi
