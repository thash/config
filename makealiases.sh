#!/bin/sh

for filename in vim vimrc gvimrc bashrc zshrc zshenv
do
	ln -s `pwd`/.$filename $HOME/.$filename
done

