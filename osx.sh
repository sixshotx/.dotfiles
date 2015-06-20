# One-time install (i.e. repeatedly running this script is bad)
brew install caskroom/cask/brew-cask
brew cask install amethyst

# Totalspaces config
# http://discuss.binaryage.com/t/please-add-an-option-to-select-the-active-monitor-based-on-the-active-window-not-the-mouse-position/1950

# common.sh
# oh-my-zsh
# https://github.com/robbyrussell/oh-my-zsh
sh -c "$(curl -fsSL https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

# FZF
# https://github.com/junegunn/fzf
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
~/.fzf/install
