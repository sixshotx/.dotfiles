#!/usr/bin/env bash

# Ask for the administrator password upfront.
sudo -v

# Keep-alive: update existing `sudo` time stamp until the script has finished.
while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

# Make sure we’re using the latest Homebrew
brew update

# One-time install (i.e. repeatedly running this script is bad)
brew install caskroom/cask/brew-cask
brew cask install amethyst
brew cask install slate

# Install GNU core utilities (those that come with OS X are outdated)
brew install coreutils
echo "Don’t forget to add $(brew --prefix coreutils)/libexec/gnubin to \$PATH."
# Install GNU `find`, `locate`, `updatedb`, and `xargs`, g-prefixed
brew install findutils
# Install Bash 4
brew install bash

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