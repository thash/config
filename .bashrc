# export PROMPT_COMMAND="echo\"\";pwd" #executed just before display prompt (bash file)

case "$HOSTNAME" in
  tgg*) # if on TSUBAME
  export PS1="\[\033[030;043m\][\t]TSUBAME:[\w]\n\[\033[030;043m\]\u ->\e[00m \[\033[0m\]";;
  *)
  export PS1="\[\033[037;041m\][\t](\!)[\w]\n\[\033[037;042m\]\u ->\e[00m \[\033[0m\]";;
esac

# \e[04m ... underline
export PATH=/opt/local/bin:/opt/local/sbin:$PATH
export CLICOLOR=1
export LSCOLORS=ExGxcxdxBxegedabagacad
export HISTFILESIZE="1000"

# https://github.com/Homebrew/brew/blob/master/share/doc/homebrew/Analytics.md
export HOMEBREW_NO_ANALYTICS=1

alias ls='ls -F'
alias sl='ls -F'
alias la='ls -aF'
alias ll='ls -lhrtF'
alias l='ls -lhrtF'

export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
