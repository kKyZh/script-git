# .bashrc

# Source global definitions
#if [ -f /etc/bashrc ]; then
#. /etc/bashrc
#fi

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions
alias "ls=ls --color=auto"
alias "tmux=tmux -2"
alias "..=cd .."
alias "cd..=cd .."

LS_COLORS='no=00:fi=00;37:di=00;92:ln=01;91:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=41;33;01:ex=00;91:*.cmd=00;32:*.exe=01;32:*.com=01;32:*.bat=01;32:*.btm=01;32:*.dll=01;32:*.tar=00;31:*.tbz=00;31:*.tgz=00;31:*.rpm=00;31:*.deb=00;31:*.arj=00;31:*.taz=00;31:*.lzh=00;31:*.lzma=00;31:*.zip=00;31:*.zoo=00;31:*.z=00;31:*.Z=00;31:*.gz=00;31:*.bz2=00;31:*.tb2=00;31:*.tz2=00;31:*.tbz2=00;31:*.xz=00;31:*.avi=01;35:*.bmp=01;35:*.fli=01;35:*.gif=01;35:*.jpg=01;35:*.jpeg=01;35:*.mng=01;35:*.mov=01;35:*.mpg=01;35:*.pcx=01;35:*.pbm=01;35:*.pgm=01;35:*.png=01;35:*.ppm=01;35:*.tga=01;35:*.tif=01;35:*.xbm=01;35:*.xpm=01;35:*.dl=01;35:*.gl=01;35:*.wmv=01;35:*.aiff=00;32:*.au=00;32:*.mid=00;32:*.mp3=00;32:*.ogg=00;32:*.voc=00;32:*.wav=00;32:'

PATH=$HOME/bin:$HOME/src/gcc/bin:$PATH
LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HOME/src/pythone3/lib:$HOME/src/gcc/lib:$HOME/src/gcc/lib64
LIBRARY_PATH=$LIBRARY_PATH:$HOME/src/pythone3/lib:$HOME/src/gcc/lib:$HOME/src/gcc/lib64

export PATH
export term=xterm-256color
export lscolors
export PS1='\u@\h:\w> '

# permanently disable ctrl-s in terminal
stty -ixon

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!

__conda_setup="$('/uhome/a00916/src/python3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/uhome/a00916/src/python3/etc/profile.d/conda.sh" ]; then
        . "/uhome/a00916/src/python3/etc/profile.d/conda.sh"
    else
        export PATH="/uhome/a00916/src/python3/bin:$PATH"
    fi
fi
unset __conda_setup

# <<< conda initialize <<<

