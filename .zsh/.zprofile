# For login shells. Sourced before zshrc.

if [[ -r $HOME/.profile ]]; then
  . $HOME/.profile
else
  echo 'No ~/.profile found.'
fi
