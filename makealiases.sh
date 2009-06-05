#!/bin/sh

for filename in vim vimrc bashrc
do
	ln -s `pwd`/.$filename $HOME/.$filename
done

