# Prompt
autoload -U colors; colors
setopt prompt_subst
unsetopt transient_rprompt

if [ $SSH_CONNECTION ] || [ $REMOTEHOST ]; then
    PROMPT='%{%(!.$bg[default].%(?.$bg[blue].$bg[red]))%}%n@%m:%(5~,%-2~/.../%2~,%~)%#%{$reset_color%} '
    RPROMPT='%{%(!.$bg[default].%(?.$bg[blue].$bg[red]))%}[`date +%Y/%m/%d` %T]%{$reset_color%}'
else
    PROMPT='%{%(!.$bg[default].%(?.$bg[green].$bg[yellow]))%}%n@%m:%(5~,%-2~/.../%2~,%~)%#%{$reset_color%} '
    RPROMPT='%{%(!.$bg[default].%(?.$bg[green].$bg[yellow]))%}[`date +%Y/%m/%d` %T]%{$reset_color%}'
fi


# Aliases
setopt aliases
alias ls='ls -FG'
alias sl='ls -FG'
alias la='ls -aFG'
alias ll='ls -lhrtFG'
alias l='ls -lhrtFG'
alias laa='ls -a | grep ^\\.'
#alias l='gls -F --color=auto'
#alias ls='gls -F --color=auto'
#alias ll='gls -lF --color=auto'
#alias la='gls -aF --color=auto'
#alias lla='gls -laF --color=auto'
#alias x='exit'
alias mv='mv -i'
alias cp='cp -i'
alias rm='rm -i'
alias ..='cd ../'
#alias dirs='dirs -v'
#alias pd='popd'
#alias ud='cd ../'
alias s='screen'
alias v='vim'
alias his='history'
alias hig='history | grep '
alias lg='ll -a | grep '


# History 
 HISTFILE=${HOME}/.zsh_history
 SAVEHIST=10000
 HISTSIZE=10000
 setopt append_history
 setopt hist_ignore_all_dups
 setopt hist_ignore_space
 setopt hist_reduce_blanks
 setopt hist_save_nodups
 setopt share_history
