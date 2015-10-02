#!/bin/bash

for filename in bashrc zshenv zshrc gitconfig gitignore vimrc gvimrc tmux.conf pryrc rspec tigrc agignore sbclrc
do
    if [ -e $HOME/.$filename ]; then
        rm -i $HOME/.$filename
    fi
    ln -s `pwd`/.$filename $HOME/.$filename
done

for dirname in .zsh .lein
do
  if [ -d `pwd`/$dirname ]; then
    if [ -d $HOME/$dirname ]; then
      rm -i -r $HOME/$dirname
    fi
    ln -s `pwd`/$dirname $HOME/$dirname
  fi
done

# source ~/.zshrc
