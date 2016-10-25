# For login shells. The bashrc doesn't get sourced by default, so do it here.

if [[ -r $HOME/.profile ]]; then
  . $HOME/.profile
else
  echo 'No ~/.profile found.'
fi

if [[ $- == *i* ]]; then
  if [[ -r $HOME/.bashrc ]]; then
    . $HOME/.bashrc
  else
    echo 'No ~/.bashrc found.'
  fi
fi
