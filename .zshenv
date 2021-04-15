export LANG=ja_JP.UTF-8
export LC_ALL="$LANG"

export LESSCHARSET=utf-8
export MAILCHECK=0 # on sakura rental server
export EDITOR=vim

# Latest Mac OS is doing some silly things when configuring PATH and I've commented out /etc/zprofile to shut them up.
# And explicitly export standard paths (defined in /etc/paths) here, before adding cutom paths in the head of $PATH.
if which /usr/libexec/path_helper > /dev/null; then
  eval `/usr/libexec/path_helper -s`
fi

# configure PATHs
export PATH=/opt/local/bin:/opt/local/sbin:/usr/local/bin:/usr/local/sbin:$PATH

# golang settings
export GOPATH=$HOME/go
export PATH=$GOPATH/bin:$PATH

# as ghq depends on go, following definition should placed after adding $GOPATH/bin into $PATH.
declare -a pathdirs=( "$HOME/git/local_gems/bin" "$HOME/bin" )
if [ `which ghq > /dev/null; echo $?` = 0 ]; then; pathdirs+=( "$(ghq root)/github.com/thash/bin"); fi
for dir in ${pathdirs[@]}; do
  if [ -d "$dir" ];then; export PATH=$dir:$PATH; fi
done

if [ "`echo $COLORTERM`" = "gnome-terminal" ]; then
  export TERM="xterm-256color"
fi

# export LS_COLORS='no=0:fi=0:di=32:ln=36:ex=35'
export CLICOLOR='true'
export PERL_BADLANG=0

if [ `which zsh > /dev/null; echo $?` = 0 ]; then
  export SHELL=`which zsh`
fi

# setting for node(including nvm, npm)
# NOTE: installing nvm via brew make shell startup slower https://github.com/creationix/nvm/issues/1277
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm

# Haskell (cabal) settings
if [ -d "$HOME/.cabal/bin" ];then; export PATH=$HOME/.cabal/bin:$PATH; fi

# https://github.com/Homebrew/brew/blob/master/share/doc/homebrew/Analytics.md
export HOMEBREW_NO_ANALYTICS=1

# Common Lisp - Roswell
if [ -d $HOME/.roswell/bin ];then; export PATH=$HOME/.roswell/bin:$PATH; fi
