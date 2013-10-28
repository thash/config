### Environmant Variables ### {{{2
export LANG=ja_JP.UTF-8
export LESSCHARSET=utf-8
export MAILCHECK=0 # on sakura rental server
export EDITOR=vim

if [[ -s $HOME/.zsh_local ]] ; then
  source $HOME/.zsh_local
fi

export PATH=/opt/local/bin:/opt/local/sbin:/usr/local/bin:/usr/local/sbin:$PATH
export PATH=$HOME/git/local_gems/bin:$PATH
export PATH=$HOME/bin:$PATH

### Prompt settings ### {{{2
#
autoload -U colors; colors
setopt prompt_subst
unsetopt transient_rprompt

# define prompt color
if [ $SSH_CONNECTION ] || [ $REMOTEHOST ]; then
    color_ok="blue"; color_ng="red"
elif [ `hostname | grep 'vagrant'` ]; then
    color_ok="magenta"; color_ng="yellow"
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


### Aliases ### {{{2
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
alias less='less -R' # colorful less
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
alias vag='vagrant'

### suffix
alias -s {haml,slim}=vim

function extract() {
  case $1 in
    *.tar.gz|*.tgz) tar xzvf $1;;
    *.tar.bz2|*.tbz) tar xjvf $1;;
    *.zip) unzip $1;;
    *.tar) tar xvf $1;;
  esac
}
alias -s {gz,tgz,bz2,tbz,zip,tar}=extract

### tmux
if [ `uname` = "Linux" ];then
  alias tmux='TERM=screen-256color-bce tmux'
fi
alias ta='tmux attach'
alias tmux-key-remote='tmux set-option -ag prefix C-w'
alias tmux-key-local='tmux set-option -ag prefix C-t'

### aptitude
if [ -f /etc/issue ] && cat /etc/issue | grep -q Ubuntu; then
  alias ac='sudo apt-cache'
  alias agi='sudo apt-get install -y'
  alias acs='sudo apt-cache search'
  if [ -f /usr/lib/mozc/mozc_tool ]; then
    alias mozc='/usr/lib/mozc/mozc_tool --mode=config_dialog'
    alias mozc-dict='/usr/lib/mozc/mozc_tool --mode=dictionary_tool'
  fi
fi


### Global Aliases {{{3
alias -g G='| egrep --color=auto --ignore-case'
alias -g H='| head'
alias -g T='| tail'
alias -g L='| less'
alias -g N='2> /dev/null'
alias -g TX='tar xvzf'
alias -g TC='tar cvzf'

### Aliases -- for git {{{3
## hub
if [ `which ruby > /dev/null; echo $?` = 0 ] &&
  [ `which hub > /dev/null; echo $?` = 0 ]; then
  eval "$(hub alias -s)"
fi
alias hubb='hub browse'

alias g='git'
alias gst='git st && g stash list'
alias st='git st'
alias gs='git st'
alias gb='git br -avv'
alias gf='git fetch -p'
alias glgg='git logg'
alias glg='git logg | head'
alias gln='git logn | head'
alias ga='git add'
alias gc='git commit -m'
alias gcm='git commit -m'
alias gcam='git commit --amend'
alias gch='git cherry -v'
alias gd='git diff --no-ext-diff -w "$@" | vim -'
alias gg='git grep -H --heading --break'
alias gull='git pull origin'
alias gush='git push origin'
alias gstd='nocorrect gstd'
alias gsta='nocorrect gsta'
alias gchs='nocorrect gchs'


### Aliases -- for Programming {{{3
alias bi='bundle install'
alias be='bundle exec'
alias bs='bundle show'
alias bo='bundle open'



### Set Options ### {{{2
setopt auto_cd
setopt auto_pushd
setopt pushd_ignore_dups
setopt correct
setopt list_packed
setopt nolistbeep

# Autojump
if [ `uname` = "Darwin" ]; then
  [[ -s `brew --prefix`/etc/autojump.sh ]] && . `brew --prefix`/etc/autojump.sh
else
  #TODO: Add Linux settings here
fi

### History ### {{{2
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


### key bindkeys (bindkey -L) ### {{{2
# NOTE: I'm using Ctrl+hjkl <=> left,down,up,right key mappings with KeyRemap4Macbook.
bindkey "^O" clear-screen # originally, it's L
# bindkey "^H" backward-delete-char # backward-char
bindkey "^H" backward-char # backward-delete-char
bindkey "^L" forward-char # clear-screen
bindkey "^K" kill-line

bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end
bindkey "^A" beginning-of-line
bindkey "^E" end-of-line

bindkey "^U" backward-delete-char
bindkey '^R' history-incremental-search-backward
bindkey "\e[Z" reverse-menu-complete # backward action of TAB key complete


### libs, miscs ### {{{2
fpath=(~/.zsh $fpath)

# should work only on Mac.
if [ `uname` = "Darwin" ]; then
  # zsh-completions (brew)
  fpath=(~/.zsh /usr/local/share/zsh-completions $fpath)

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

## completion {{{3
# -u option: ignore "insecure" warning
autoload -U compinit; compinit -u
compdef hub=git

# ignore CASE when complete
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*' menu select=1


### display git branch on the prompt ### {{{3
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


### autojump settings {{{3
if [ -f /usr/share/autojump/autojump.sh ]; then
  . /usr/share/autojump/autojump.sh
fi
