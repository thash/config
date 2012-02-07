#!/bin/sh

for filename in vimrc gvimrc zshrc zshenv tmux.conf gitconfig vimperatorrc profile gemrc
do
    if [ -e $HOME/.$filename ]; then
        rm -i $HOME/.$filename
        ln -s `pwd`/.$filename $HOME/.$filename
    else
        ln -s `pwd`/.$filename $HOME/.$filename
    fi
done

ln -s `pwd`/.zsh $HOME/.zsh

# source ~/.zshrc
