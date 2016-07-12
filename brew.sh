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
# Going to install chrome normally instead, sometimes there are odd
# errors w/ this version. For example, the karma test runner
# doesn't automatically detect the Chrome binary in this case.
# brew cask install google-chrome
brew cask install iterm2
brew cask install karabiner
brew cask install keyboard-maestro
brew cask install seil
brew cask install slack
brew cask install slate
brew cask install sublime-text3
brew cask install sogouinput
# VLC
brew cask install vlc


# Install GNU core utilities (those that come with OS X are outdated and sometimes differ
# in parameters from the standard Linux ones)
brew install coreutils findutils gnu-tar gnu-sed gawk gnutls gnu-indent gnu-getopt
echo "Don’t forget to add $(brew --prefix coreutils)/libexec/gnubin to \$PATH."
# Install GNU `find`, `locate`, `updatedb`, and `xargs`, g-prefixed
brew install findutils
# Install Bash 4
brew install bash
# Install Zsh 5
brew install zsh

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
brew cask install hammerspoon

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
# Needed to make terminal-notifier work w/ tmux.
brew install reattach-to-user-namespace

# Remove outdated versions from the cellar
brew cleanup

# Totalspaces config
# http://discuss.binaryage.com/t/please-add-an-option-to-select-the-active-monitor-based-on-the-active-window-not-the-mouse-position/1950

npm i -g diff-so-fancy

#########
# Music #
#########
# Cmus is a terminal music player
brew install cmus

# mpsyt
# https://github.com/mps-youtube/mps-youtube
sudo pip3 install mps-youtube --upgrade
brew cask install vlc
# In mps, do a `set player vlc`.
