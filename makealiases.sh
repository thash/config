#!/bin/sh

for filename in vimrc vimrc.vimshell gvimrc zshrc zshenv tmux.conf gitconfig vimperatorrc gemrc tigrc pryrc ackrc agignore
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
