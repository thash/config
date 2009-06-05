export PS1="\u::\w$ "
export PATH=/opt/local/bin/:/opt/local/sbin/:$PATH

alias ls='ls -F'
alias sl='ls -F'
alias la='ls -aF'
alias ll='ls -lhrtF'
alias l='ls -lhrtF'
alias his='history'
alias hig='history | grep'

alias fcd='/Users/thash/unix/bin/fcd.sh'
alias tsubame='/Users/thash/unix/bin/tsubame.sh'

alias firefox="open -a Firefox"
alias safari="open -a Safari"
alias preview="open -a Preview"


function cd () { 
  builtin cd $1 
  ls 
}

function .. () { 
  builtin cd .. 
  ls 
}
