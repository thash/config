export PATH=$HOME/bin:$HOME/.rvm/bin:$HOME/local/bin:/opt/local/bin:/opt/local/sbin:/usr/local/bin:/usr/local/sbin:$PATH

export LANG=ja_JP.UTF-8
export LC_ALL="$LANG"
if [ "`echo $COLORTERM`" = "gnome-terminal" ]; then
  export TERM="xterm-256color"
fi
# export LS_COLORS='no=0:fi=0:di=32:ln=36:ex=35'
export CLICOLOR='true'
export PERL_BADLANG=0

if [ "`ls -a ~ | grep .cabal`" != "" ]; then
  export PATH=$HOME/.cabal/bin:$PATH
fi

# setting for node
if [ `which npm` != "" ]; then
  export PATH=`npm -g bin 2>/dev/null`:$PATH
fi

### rvm settings -- should in zshenv, not in zshrc
if [[ -s $HOME/.rvm/scripts/rvm ]] ; then
    source $HOME/.rvm/scripts/rvm
    # when called from quickrun, load .rvmrc
    if [ `uname` = "Darwin" ]; then
        rvm use 2.0.0
    fi
fi

AWS_CONFIG_FILE=$HOME/.aws.config

############ Android development ##############
if [ -f /usr/libexec/java_home ]; then
  export JAVA_HOME=`/usr/libexec/java_home`
fi

# installed ADK in some places
if [ -d $HOME/work/android-sdk-macosx/ ]; then
  export PATH=$HOME/work/android-sdk-macosx/platform-tools:$HOME/work/android-sdk-macosx/tools:$PATH
fi

if [ -d $HOME/adk/ ]; then
  export PATH=$HOME/adk/platform-tools:$HOME/adk/tools:$PATH
fi
