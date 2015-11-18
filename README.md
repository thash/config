config
========================

Mainly dot files like .vimrc, .zshrc .gitconfig.
The repo includes KeyRemap4Macbook and daily Rakefile.


## Vim

```bash
$ git clone git@github.com:memerelics/vimdir.git ~/git/vimdir
$ cd ~ && ln -s ~/git/vimdir .vim
$ mkdir -p ~/.vim/bundle
$ git clone git://github.com/Shougo/neobundle.vim ~/.vim/bundle/neobundle.vim
```


## private.xml

copy private.xml to

``````````````
/Users/hash/Library/Application\ Support/KeyRemap4MacBook/private.xml
``````````````

It's bothering, but KeyRemap4Macbook doesn't recognize symbolic link.


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

