#!/usr/bin/env bash
# Ask for the administrator password upfront.
sudo -v

# Keep-alive: update existing `sudo` time stamp until the script has finished.
while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

# Make sure we’re using the latest Homebrew
brew update

brew install caskroom/cask/brew-cask
brew tap caskroom/versions
brew cask install 1password
brew cask install alfred
brew cask install amethyst
brew cask install android-file-transfer
brew cask install dropbox
brew cask install google-chrome
brew cask install iterm2
brew cask install karabiner
brew cask install keyboard-maestro
brew cask install seil
brew cask install slack
brew cask install slate
brew cask install sublime-text3
brew cask install sogouinput
brew cask install vlc

# Install GNU core utilities (those that come with OS X are outdated)
brew install coreutils
echo "Don’t forget to add $(brew --prefix coreutils)/libexec/gnubin to \$PATH."
# Install GNU `find`, `locate`, `updatedb`, and `xargs`, g-prefixed
brew install findutils
# Install Bash 4
brew install bash

# FZF. Shell fuzzy finder
brew reinstall --HEAD fzf
# Install shell extensions
/usr/local/Cellar/fzf/HEAD/install

# Macvim!
brew install macvim --override-system-vim --with-lua --with-luajit --with-python3

# curl alternative
brew install httpie

# Install wget with IRI support
brew install wget --enable-iri

# Install more recent versions of some OS X tools
brew tap homebrew/dupes
brew install homebrew/dupes/grep
brew tap josegonzalez/homebrew-php
brew install php54

# We're installing python from brew so that pip comes with it
brew install python
pip install click
pip install pushbullet.py

# Install everything else
brew install ag

#########
# Emacs #
#########
brew install emacs --with-cocoa
# Gtags. ctags-like for Emacs.
brew install global --with-exuberant-ctags --with-pygments --with-sqlite3

brew install tmux

#######
# Git #
#######
# Install updated git
brew install git
# hub - Awesome git wrapper
brew install hub

brew install node

# Remove outdated versions from the cellar
brew cleanup

# Totalspaces config
# http://discuss.binaryage.com/t/please-add-an-option-to-select-the-active-monitor-based-on-the-active-window-not-the-mouse-position/1950
