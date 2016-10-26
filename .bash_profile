if [[ -r ~/.profile ]]; then
  . $HOME/.profile
else
  echo 'No ~/.profile found.'
fi

# bashrc doesn't get sourced for login shells; do it here.

if [[ $- == *i* ]]; then
  if [[ -r ~/.bashrc ]]; then
    . $HOME/.bashrc
  else
    echo 'No ~/.bashrc found.'
  fi
fi
