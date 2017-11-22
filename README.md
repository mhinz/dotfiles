[![Build Status](https://travis-ci.org/mhinz/dotfiles.svg?branch=master)](https://travis-ci.org/mhinz/dotfiles)

I use [Vim/Neovim](.vim/vimrc), [Emacs](.emacs.d/init.el),
[git](.config/git/config), [tmux](.tmux.conf), and [zsh](.zsh/.zshrc)
([profile](.profile), [shrc](.shrc)) on a daily basis and thus these configs are
the most customized. Have fun reading them and steal what you need.

*Disclaimer: No Reddit users were hurt during the creation of these
configuration files.*

---

I prefer light colors at day and dark colors at night, so I use [this little
function](https://github.com/mhinz/dotfiles/blob/f1cae979e9e72ab414b4c8b3444144c30aa4cde3/.zsh/.zshrc#L448-L470)
to switch between two custom iTerm2 profiles. Things like Vim, `$LS_COLORS`,
`git log` output etc. automatically adapt their colors depending on the current
setting of `$ITERM_PROFILE`.

![Git light](https://raw.githubusercontent.com/mhinz/dotfiles/master/.github/screenshot-git-light.png)
![Vim light](https://raw.githubusercontent.com/mhinz/dotfiles/master/.github/screenshot-vim-light.png)
![Vim dark](https://raw.githubusercontent.com/mhinz/dotfiles/master/.github/screenshot-vim-dark.png)
