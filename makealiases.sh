#!/bin/sh

for filename in vim vimrc gvimrc bashrc zshrc zshenv gitconfig
do
	ln -s `pwd`/.$filename $HOME/.$filename
done

