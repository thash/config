export LANG=ja_JP.UTF-8
export LC_ALL="$LANG"
if [ "`echo $COLORTERM`" = "gnome-terminal" ]; then
  export TERM="xterm-256color"
fi
# export LS_COLORS='no=0:fi=0:di=32:ln=36:ex=35'
export CLICOLOR='true'
export PERL_BADLANG=0

if [ `which zsh > /dev/null; echo $?` = 0 ]; then
  export SHELL=`which zsh`
fi

### setting for node(including nvm, npm)
export NVM_DIR="/Users/$USER/.nvm"
if [ -s "$NVM_DIR/nvm.sh" ]; then
  . "$NVM_DIR/nvm.sh"
  # nvm use stable
elif [ -s "$(brew --prefix nvm)/nvm.sh" ]; then
  . $(brew --prefix nvm)/nvm.sh
  # nvm use stable
fi

if [ `which npm > /dev/null; echo $?` = 0 ]; then
  export PATH=`npm -g bin 2>/dev/null`:$PATH
fi

# WARNING: Found the legacy environment variable [AWS_CONFIG_FILE=/Users/thash/.aws.config].
# Please use the latest environment variable to specify your credentials file override: [AWS_CREDENTIAL_PROFILES_FILE=/Users/thash/.aws.config]
export AWS_CREDENTIAL_PROFILES_FILE=$HOME/.aws/credentials

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

# brew install android-sdk
if [ -d /usr/local/opt/android-sdk/ ];then
  export ANDROID_HOME=/usr/local/opt/android-sdk
fi

if [ -d $HOME/adk/ ]; then
  export PATH=$HOME/adk/platform-tools:$HOME/adk/tools:$PATH
fi

# golang settings
export GOPATH=$HOME/.go
export PATH=$PATH:$GOPATH/bin

# Emacs path (mainly for emacsclient)
export PATH=/usr/local/Cellar/emacs/24.3/bin:$PATH

# Python installation based on:
# https://python-guide-ja.readthedocs.org/en/latest/starting/install/osx/
export PATH=/usr/local/share/python:$PATH
export WORKON_HOME=~/.virtualenvs
