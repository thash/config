#!/bin/sh

for filename in vimrc vimrc.vimshell gvimrc zshrc zshenv tmux.conf gitconfig vimperatorrc gemrc tigrc pryrc ackrc agignore
do
    if [ -e $HOME/.$filename ]; then
        rm -i $HOME/.$filename
    fi
    ln -s `pwd`/.$filename $HOME/.$filename
done

if [ -d `pwd`/.zsh ]; then
  if [ -d $HOME/.zsh ]; then
    rm -i -r $HOME/.zsh
  fi
  ln -s `pwd`/.zsh $HOME/.zsh
fi

# source ~/.zshrc
