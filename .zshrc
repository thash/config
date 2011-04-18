# Prompt settings
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

#Environmant Variables
export LANG=ja_JP.UTF-8
export LESSCHARSET=utf-8
export MAILCHECK=0 # on sakura rental server


# Aliases
setopt aliases
alias ls='ls -FG' sl='ls -FG' la='ls -aFG' ll='ls -lhrtFG' l='ls -lhrtFG'
alias laa='ls -A | grep ^\\.'
alias lg='ll -a | grep '
alias mv='mv -i' cp='cp -i' rm='rm -i'
alias ..='cd ../'
alias his='history'
alias hig='history | grep '
alias vi='vim'
alias s='screen -r'
alias cl='clear'
alias g='git'
alias ssh2sakura='ssh takuyahashimoto@112.78.112.93'
alias ssh2relics_pub='ssh hash@www.memerelics.net'

# Local Mac {{{1
if [ `uname` = "Darwin" ]; then
    alias gvim='open -a MacVim.app'
    alias pymol='open -a MacPyMOL.app'
    alias gitx='open -a GitX'
    alias hoge='cd /Users/hash/'
    alias calc='ruby ~/unix/bin/calc.rb'
    alias rest='sudo vim /etc/hosts'
    alias ssh2relics='ssh hash@10.0.1.5'
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


# screen settings {{{1
# ref. http://nijino.homelinux.net/diary/200206.shtml#200206140 

if [ "$TERM" = "screen" ]; then
chpwd () {
#    _set_env_git_current_branch
#    _update_rprompt
    echo -n "_`dirs`\\"
    ls
} 

preexec() {
# see [zsh-workers:13180]
# http://www.zsh.org/mla/workers/2000/msg03993.html
    emulate -L zsh
        local -a cmd; cmd=(${(z)2})
        case $cmd[1] in
        fg)
        if (( $#cmd == 1 )); then
            cmd=(builtin jobs -l %+)
        else
            cmd=(builtin jobs -l $cmd[2])
                fi
                ;;
    %*) 
        cmd=(builtin jobs -l $cmd[1])
        ;;
    cd)
        if (( $#cmd == 2)); then
            cmd[1]=$cmd[2]
                fi
                ;&
                *)
                echo -n "k$cmd[1]:t\\"
                return
                ;;
    esac

        local -A jt; jt=(${(kv)jobtexts})

        $cmd >>(read num rest
                cmd=(${(z)${(e):-\$jt$num}})
                echo -n "k$cmd[1]:t\\") 2>/dev/null
}
chpwd
fi

#}}}1
