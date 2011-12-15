# Prompt settings
autoload -U colors; colors
setopt prompt_subst
unsetopt transient_rprompt

# define prompt color
if [ $SSH_CONNECTION ] || [ $REMOTEHOST ]; then
    color_ok="blue"; color_ng="red"
else
    color_ok="green"; color_ng="yellow"
fi

# prompt format
    PROMPT='%{%(!.$bg[default].%(?.$bg[$color_ok].$bg[$color_ng]))%}\
[%h]%n%#\
%{$reset_color%} '
    RPROMPT='`rprompt-git-current-branch`%{%(!.$bg[default].%(?.$bg[$color_ok].$bg[$color_ng]))%}\
%(4~,%-1~/.../%2~,%~) [`date +%Y/%m/%d` %T]@%m\
%{$reset_color%}'
    SPROMPT="%{${bg[red]}%}correct: %R -> %r [nyae]? %{${reset_color}%}"

#Environmant Variables
export LANG=ja_JP.UTF-8
export LESSCHARSET=utf-8
export MAILCHECK=0 # on sakura rental server


# Aliases
setopt aliases
alias ls='ls -vFG' sl='ls -vFG' la='ls -avFG' ll='ls -lhrtvFG' l='ls -lhrtvFG'
alias laa='ls -vA | grep --ignore-case --color=auto ^\\.'
alias lg='ll -va | grep --ignore-case --color=auto'
alias mv='mv -i' cp='cp -i' rm='rm -i'
alias ..='cd ../'
alias his='history'
alias hig='history 500 | grep --color=auto --ignore-case'
alias grep='grep --color=auto --ignore-case'
alias pgrep='pgrep -i' pkill='pkill -i'
alias nameru='find . -name "*" -print | xargs grep --color=auto --ignore-case -n'
alias s='screen -r'
alias vi='nocorrect vim' vim='nocorrect vim'
alias r='rails'
alias tailf='tail -f'
alias lan='landslide icampresen.md && open presentation.html'
alias scon='vim ~/.ssh/config'

# Global Aliases
alias -g G='| grep --color=auto --ignore-case'
alias -g H='| head'
alias -g T='| tail'
alias -g L='| less'
alias -g TX='tar xvzf'
alias -g TC='tar cvzf'
### Edit Stdout with vim, making tmpfile.
export VIM_TMP=~/tmp/vim_stdout.tmp
alias -g V="> $VIM_TMP$$; vim $VIM_TMP$$"
alias -g BGEM="vendor/bundle/ruby/1.9.1/gems/"

# Aliases -- for git
alias g='nocorrect git'
alias gst='g st'
alias st='g st'
alias glg='g logg | head'
alias gln='g logn | head'
alias ga='git add'
alias gc='git commit -m'
alias gd='git diff'
alias gstd='nocorrect gstd'
alias gsta='nocorrect gsta'

# Aliases -- for Programming
alias node='nocorrect node'
alias gemst='rvm gemset list'
alias gemset='rvm gemset list'
alias irb='pry'
alias b='bundle'
alias be='bundle exec'
alias cuc='bundle exec cucumber 2>/dev/null'
alias rsp='bundle exec rspec'

# Local Mac {{{1
if [ `uname` = "Darwin" ]; then
    alias vim='/usr/local/Cellar/vim/7.3.333/bin/vim'
    alias gvim='open -a MacVim.app'
    export VMAIL_VIM=mvim #Vmail settings
    alias pymol='open -a MacPyMOL.app'
    alias gitx='open -a GitX'
    alias hoge='cd /Users/hash/'
    alias calc='ruby ~/unix/bin/calc.rb'
    alias rest='sudo vim /etc/hosts'
    alias ssh2relics='ssh hash@10.0.1.5'
    alias refe='/Users/hash/work/src/refe/refe-1_9_2'
#    source /usr/local/Cellar/coreutils/8.12/aliases # GNU utils
fi #}}}1

# Set Options
autoload -U compinit
compinit
setopt auto_cd
setopt auto_pushd
setopt pushd_ignore_dups
setopt correct
setopt list_packed
setopt nolistbeep

# ignore CASE when complete
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*' menu select=1

# Advanced completion settings
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format 'No matches for: %d'
zstyle ':completion:*' group-name ''

# History
# by default, display latest histories.
 HISTFILE=${HOME}/.zsh_history
 SAVEHIST=500000
 HISTSIZE=500000
 setopt append_history
 setopt hist_ignore_all_dups
 setopt hist_ignore_space
 setopt hist_reduce_blanks
 setopt hist_save_nodups
 setopt share_history


### refer words from alc using w3c
function alc() {
  if [ $# != 0 ]; then
    w3m "http://eow.alc.co.jp/$*/UTF-8/?ref=sa"
  else
    w3m "http://www.alc.co.jp/"
  fi
}


### display git branch on the prompt
autoload -Uz VCS_INFO_get_data_git; VCS_INFO_get_data_git 2> /dev/null

function rprompt-git-current-branch {
  local name st color gitdir action
  if [[ "$PWD" =~ '/¥.git(/.*)?$' ]]; then
    return
  fi
  name=$(basename "`git symbolic-ref HEAD 2> /dev/null`")
  if [[ -z $name ]]; then
    return
  fi

  gitdir=`git rev-parse --git-dir 2> /dev/null`
  action=`VCS_INFO_git_getaction "$gitdir"` && action="($action)"

  st=`git status 2> /dev/null`
  if [[ -n `echo "$st" | grep "^nothing to"` ]]; then
    color=%F{green}
  elif [[ -n `echo "$st" | grep "^nothing added"` ]]; then
    color=%F{yellow}
  elif [[ -n `echo "$st" | grep "^# Untracked"` ]]; then
    color=%B%F{red}
  else
     color=%F{red}
  fi
  echo "$color$name$action%f%b "
}
