# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
# force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

export PATH=$PATH:~/bin

# /---------------CUSTOM SETTINGS GO UNDER THIS LINE---------------------------/

# Default text editor:
export EDITOR='vim'

# Startup commands

# This is so I don't miss my appointments!
# Shows the current month and the next one
cal -A 1
echo
echo "Compromissos para os próximos 7 dias:"
echo

# strftime-style formatting, print to screen and copy to briefing.txt
calcurse -r7 --format-apt='- %S -> %E\n\t%m\n%N' | tee ~/.briefing.txt
echo
echo "Você pode olhar os compromissos no arquivo briefing.txt também com o comando 'briefme'."
echo

# Check if .pploweumoney exists

if [ -f ~/.pploweumoney  ]; then
        echo " ** Alguém está devendo dinheiro pra você! Olhe o arquivo .pploweumoney! **"
    fi

# MY ALIASES
alias pomocount='vim ~/pomodorocount2017'
alias editalbumlist='vim ~/Documentos/ExportacaoEvernote/Música/ListaAlbums'
alias journal='vim diario.txt'
alias pomodoro='~/"Bash Scripts"/pomodoro.sh'
alias pomocheck='tail ~/pomodorocount2017'
alias update='sudo apt-get update; sudo apt-get upgrade -y'
alias tuxsay='cowsay -f tux'
alias oblique='python3 ~/oblique-strategies/oblique.py'
alias refresh='source ~/.bashrc'
alias xcopy='python3 ~/Python\ Scripts/custom_xclip.py'
alias vimconfig='vim ~/.vimrc'
alias bashconfig='vim ~/.bashrc'
alias unix='curl -L git.io/unix'
alias weather_full='curl wttr.in/Ponta_grossa'
alias weather='echo -e "--------------------------------------------------------------------------------\n ** If you want the full weather report, type weather_full. **\n--------------------------------------------------------------------------------\n"; weather_full | head -17'
alias lynx='lynx -nofilereferer -noreferer -anonymous -cookies -vikeys'
alias plz='sudo'
alias clean='sudo apt-get clean'
alias commit='git add --all; git commit; git push'
alias wifireload='sudo killall NetworkManager; sudo NetworkManager &'
alias audioreload='pulseaudio --kill'
alias briefme='less ~/.briefing.txt'
alias budget='libreoffice --calc ~/Documentos/Controle\ financeiro.ods'
alias addtodo='./addtodo.sh'
alias todo='calcurse -t --format-todo "(%p) %m\n"'
