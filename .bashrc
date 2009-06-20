# export PROMPT_COMMAND="echo\"\";pwd" #executed just before display prompt (bash file)
export PS1="\[\033[037;041m\](\!)[\t][\w]\n\[\033[037;042m\]\u ->\e[00m \[\033[0m\]"
# \e[04m ... underline
export PATH=/opt/local/bin/:/opt/local/sbin/:$PATH
export CLICOLOR=1
export LSCOLORS=ExGxcxdxBxegedabagacad
export HISTFILESIZE="1000"

alias ls='ls -F'
alias sl='ls -F'
alias la='ls -aF'
alias ll='ls -lhrtF'
alias l='ls -lhrtF'
alias his='history'
alias hig='history | grep '


# Mac OS X, Open Apps. {{{1
alias firefox="open -a Firefox"
alias safari="open -a Safari"
alias preview="open -a Preview"
#}}}1
# functions {{{1

function cd () { 
  builtin cd $1 
  ls 
}

function .. () { 
  builtin cd .. 
  ls 
}

# }}}1
