config
========================

Mainly dot files like .vimrc, .zshrc .gitconfig.
The repo includes KeyRemap4Macbook and daily Rakefile.


## Vim

```bash
$ git clone git@github.com:memerelics/vimdir.git ~/git/vimdir
$ cd ~ && ln -s ~/git/vimdir .vim
$ curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
     https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
```

Then exec vim and call `:PlugInstall`.


## Karabiner (karabiner-Elements)

`Karabiner` directory -> `~/.config/karabiner/`


TODO
====================================

* merge vimdir


Tips
====================================

how to change commiter/author of old commits

```
$ git config user.name "memerelics"
$ git config user.email "xxxxxxxxxxxxxxxxx@gmail.com"

$ cat .git/config

$ git rebase -i <earlier commit>

// if --amend without --author, only commiter information will be changed.
$ git commit --amend --author="memerelics <xxxxxxxxxxxxxxxxx@gmail.com>"
$ git rebase --continue
```

