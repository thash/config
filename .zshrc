# Prompt settings
#
autoload -U colors; colors
setopt prompt_subst
unsetopt transient_rprompt

# define prompt color
if [ $SSH_CONNECTION ] || [ $REMOTEHOST ]; then
    color_ok="blue"; color_ng="red"
else
    color_ok="green"; color_ng="yellow"
fi

# define color by $?, [count]name, and float $.
PROMPT='%{%(!.$bg[default].%(?.$bg[$color_ok].$bg[$color_ng]))%}\
%m%{$reset_color%} \
%{$fg[blue]%}%B\$%b%{$reset_color%} '

# git branch, path, date@hostname(define color by $?)
RPROMPT='`rprompt-git-current-branch`\
%{$reset_color%}%{$fg[blue]%}%B%(4~,%-1~|%2~,%~)%b \
%{%(!.$bg[default].%(?.$bg[$color_ok].$bg[$color_ng]))%}\
%{$fg[black]%}[`date +%Y/%m/%d` %T]%{$reset_color%}'

SPROMPT='%{${bg[red]}%}\
m9(^Д^) \
%R -> %r ?[nyae]%{${reset_color}%} '

#Environmant Variables
export LANG=ja_JP.UTF-8
export LESSCHARSET=utf-8
export MAILCHECK=0 # on sakura rental server
export EDITOR=vim

fpath=(~/.zsh $fpath)

# should work only on Mac.
if [ `uname` = "Darwin" ]; then
  #ZSHHOME="${HOME}/.zsh"

  ## autoload ~/.zsh/*.zsh file as a part of .zshrc
  #if [ -d $ZSHHOME -a -r $ZSHHOME -a \
  #  -x $ZSHHOME ]; then
  #for i in $ZSHHOME/*; do
  #  [[ ${i##*/} = *.zsh ]] &&
  #    [ \( -f $i -o -h $i \) -a -r $i ] && . $i
  #done
  #fi

  #if [ -f $ZSHHOME/auto-fu.zsh ]; then
  #  zle-line-init () {auto-fu-init;}
  #  zle -N zle-line-init
  #  #  zstyle ':completion:*' completer _oldlist _complete #_history
  #  zstyle ':auto-fu:highlight' completion/one fg=blue
  #  zstyle ':auto-fu:var' postdisplay $'
  #  navi > '
  #  bindkey-advice-before "^G" afu+cancel
  #  bindkey-advice-before "^[" afu+cancel
  #  bindkey-advice-before "^J" afu+cancel afu+accept-line
  #fi
fi

# key-bindings (check by bindkey -L)
# NOTE: I'm using Ctrl+hjkl <=> left,down,up,right key mappings with KeyRemap4Macbook.
bindkey "^O" clear-screen # originally, it's L
bindkey "^Y" kill-line    # originally, it's K

bindkey "\e[Z" reverse-menu-complete # backward action of TAB key complete

# Aliases
setopt aliases
if [ `uname` = "Darwin" ];then
  alias ls='ls -vFG' sl='ls -vFG' la='ls -avFG' ll='ls -lhrtvFG' l='ls -lhrtvFG' alias y='ls -vFG'
  alias gvim='open -a MacVim.app'
  alias sed='gsed'
else
  alias ls='ls -vF --color' sl='ls -vF --color' la='ls -avF --color' ll='ls -lhrtvF --color' l='ls -lhrtvF --color'
fi
alias laa='ls -vA | grep --ignore-case --color=auto ^\\.'
alias lg='ll -va | grep --ignore-case --color=auto'
alias mv='mv -i' cp='cp -i' rm='rm -i'
alias ..='cd ../'
alias his='history'
alias hig='history 500 | grep --color=auto --ignore-case'
alias grep='grep --color=auto --ignore-case'
alias pgrep='pgrep -i' pkill='pkill -i'
#alias v='nocorrect vim' vim='nocorrect vim'
alias vimrc='vim ~/.vimrc'
alias r='rails'
alias tailf='tail -f'
alias scon='vim ~/.ssh/config'
alias ifconfig.me='curl ifconfig.me/ip'
alias svim='sudo -H vim --noplugin'
alias rest='sudo -H vim --noplugin /etc/hosts'

if [ `uname` = "Linux" ];then
  alias tmux='TERM=screen-256color-bce tmux'
fi
alias ta='tmux attach'

if [ -f /etc/issue ] && cat /etc/issue | grep -q Ubuntu; then
  alias ag='sudo apt-get'
  alias ac='sudo apt-cache'
  alias agi='sudo apt-get install -y'
  alias acs='sudo apt-cache search'
  if [ -f /usr/lib/mozc/mozc_tool ]; then
    alias mozc='/usr/lib/mozc/mozc_tool --mode=config_dialog'
    alias mozc-dict='/usr/lib/mozc/mozc_tool --mode=dictionary_tool'
  fi
fi


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
alias gst='git st && g stash list'
alias st='git st'
alias gs='git st'
alias gb='git br -a'
alias gf='git fetch -p'
alias glgg='git logg'
alias glg='git logg | head'
alias gln='git logn | head'
alias ga='git add'
alias gc='git commit -m'
alias gcm='git commit -m'
alias gch='git cherry -v'
alias gd='git diff --no-ext-diff -w "$@" | vim -'
alias gg='git grep -H --heading --break'
alias gull='git pull origin'
alias gush='git push origin'
alias gstd='nocorrect gstd'
alias gsta='nocorrect gsta'
alias gchs='nocorrect gchs'

alias hubb='hub browse'

# Aliases -- for Programming
alias bi='bundle install'
alias be='bundle exec'
alias bs='bundle show'
alias bo='bundle open'

# Set Options
autoload -U compinit
compinit -u
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
  # zstyle ':completion:*' verbose yes
# zstyle ':completion:*:descriptions' format '%B%d%b'
# zstyle ':completion:*:messages' format '%d'
# zstyle ':completion:*:warnings' format 'No matches for: %d'
# zstyle ':completion:*' group-name ''
# zstyle ':completion:*' keep-prefix

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

autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end
bindkey "^A" beginning-of-line
bindkey "^E" end-of-line
bindkey "^H" backward-char
bindkey "^L" forward-char
bindkey "^K" kill-line
bindkey "^U" backward-delete-char
bindkey '^R' history-incremental-search-backward


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
  if [[ "$PWD" =~ '/.git(/.*)?$' ]]; then
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

