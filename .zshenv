export PATH=$HOME/bin:$HOME/.rvm/bin:$HOME/local/bin:/opt/local/bin:/opt/local/sbin:/usr/local/bin:/usr/local/sbin:$PATH

export LANG=ja_JP.UTF-8
export LC_ALL="$LANG"
export LS_COLORS='no=0:fi=0:di=32:ln=36:ex=35'
export CLICOLOR='true'
export PERL_BADLANG=0

if [ "`ls -a ~ | grep .cabal`" != "" ]; then
  export PATH=$HOME/.cabal/bin:$PATH
fi

# setting for node
if [ "`ls -a ~ | grep .nave`" = ".nave/" ]; then
    ~/.nave/nave.sh use 0.4.7
fi

### rvm settings -- should in zshenv, not in zshrc
if [[ -s $HOME/.rvm/scripts/rvm ]] ; then
    source $HOME/.rvm/scripts/rvm
    # when called from quickrun, load .rvmrc
    if [ `uname` = "Darwin" ]; then
        rvm use 1.9.3-p0
        rvm gemset use global
    fi
fi
