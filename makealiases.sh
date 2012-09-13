#!/bin/sh

for filename in vimrc gvimrc zshrc zshenv tmux.conf gitconfig vimperatorrc gemrc tigrc pryrc
do
    if [ -e $HOME/.$filename ]; then
        rm -i $HOME/.$filename
    fi
    ln -s `pwd`/.$filename $HOME/.$filename
done

if [ -d $HOME/.zsh ]; then
  rm -i -r $HOME/.zsh
fi
ln -s `pwd`/.zsh $HOME/.zsh

# source ~/.zshrc
