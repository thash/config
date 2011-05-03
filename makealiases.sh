#!/bin/sh

for filename in vimrc gvimrc bashrc zshrc zshenv screenrc gitconfig vimperatorrc profile
do
    if [ -e $HOME/.$filename ]; then 
        rm -i $HOME/.$filename 
        ln -s `pwd`/.$filename $HOME/.$filename
    else
        ln -s `pwd`/.$filename $HOME/.$filename
    fi
done

source ~/.zshrc
