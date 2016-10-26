# Opposed to Bash, Zsh doesn't source ~/.profile on its own.

if [[ -r ~/.profile ]]; then
  emulate sh -c 'source ~/.profile'
else
  echo 'No ~/.profile found.'
fi
