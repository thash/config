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

### setting for node(including nvm, npm)
# nvm: https://github.com/creationix/nvm/blob/master/README.markdown
if [ -d $HOME/.nvm ]; then
  source ~/.nvm/nvm.sh
  # TODO: find installed versions
  # nvm use v0.9.5
fi
if [ `which npm > /dev/null; echo $?` = 0 ]; then
  export PATH=`npm -g bin 2>/dev/null`:$PATH
fi

export AWS_CONFIG_FILE=$HOME/.aws.config

############ Android development ##############
if [ -f /usr/libexec/java_home ]; then
  export JAVA_HOME=`/usr/libexec/java_home`
fi

# installed ADK in some places
if [ -d $HOME/work/android/sdk ]; then
  export PATH=$HOME/work/android/sdk/tools:$PATH
  export PATH=$HOME/work/android/sdk/platform-tools:$PATH
  export PATH=$HOME/work/android/sdk/build-tools/17.0.0:$PATH
fi

if [ -d $HOME/adk/ ]; then
  export PATH=$HOME/adk/platform-tools:$HOME/adk/tools:$PATH
fi
