#!/bin/sh

for filename in vim vimrc grimrc bashrc zshrc zshenv
do
	ln -s `pwd`/.$filename $HOME/.$filename
done

