#!/bin/sh

for filename in vimrc gvimrc bashrc zshrc zshenv screenrc gitconfig
do
    if [ -e $HOME/.$filename ]; then 
        rm -i $HOME/.$filename 
    fi
    ln -s `pwd`/.$filename $HOME/.$filename
done

source ~/.zshrc
