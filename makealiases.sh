#!/bin/bash

declare -a configfiles=(
  ".bashrc"
  ".zshenv"
  ".zshrc"
  ".gitconfig"
  ".gitignore"
  ".vimrc"
  ".tmux.conf"
) # .gvimrc .pryrc .rspec .gemrc .tigrc .vimperatorrc .sbclrc

for filename in ${configfiles[@]}
do
    if [ -e $HOME/$filename ]; then
        rm -i $HOME/$filename
    fi
    ln -s `pwd`/$filename $HOME/$filename
done


declare -a configdirs=( "zsh" ) # lein vimperator

if [ "$(uname -s)" == "Linux" ]; then
  configdirs+=("xkb" "xkeysnail")
fi

for dirname in ${configdirs[@]}
do
  if [ -d `pwd`/$dirname ]; then
    if [ -d $HOME/.$dirname ]; then
      rm -i -r $HOME/.$dirname
    fi
    ln -s `pwd`/$dirname $HOME/.$dirname # NOTE: directories are symlinked with 'dot'
  fi
done
