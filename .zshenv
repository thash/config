export PATH=$HOME/bin:$HOME/local/bin:/opt/local/bin:/opt/local/sbin:/usr/local/bin:$PATH
export PATH=$HOME/node_modules/.bin:$PATH


#first $HOME... is for sakura rental server.
export LANG=ja_JP.UTF-8
export LC_ALL="$LANG"
export LS_COLORS='no=0:fi=0:di=32:ln=36:ex=35'
export CLICOLOR='true'
export PERL_BADLANG=0

# setting for node
if [ "`ls -a ~ | grep .nave`" = ".nave/" ]; then
    ~/.nave/nave.sh use 0.4.7
fi

### rvm settings -- should in zshenv, not in zshrc
if [[ -s $HOME/.rvm/scripts/rvm ]] ; then
    source $HOME/.rvm/scripts/rvm
    if [ `uname` = "Darwin" ]; then
        rvm 1.9.2-p180
    fi
fi
