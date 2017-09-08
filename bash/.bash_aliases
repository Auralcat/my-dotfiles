# This is my alias file.

# Came with the standard .bashrc:

alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

alias l='ls -CF'
alias la='ls -A'
alias ll='ls -alF'
alias ls='ls --color=auto'

alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias grep='grep --color=auto'

#-----------------MY-ALIASES---------------------------------------------------

# Custom programs:

# Pomodoro related:
alias pomocheck='tail ~/pomodorocount2017'
alias pomocount='vim ~/pomodorocount2017'
alias pomodoro='~/purrcured-scripts/Pomodoro/pomodoro_tmux.py'

# Change dotfiles:
alias bashconfig='vim ~/.bashrc'
alias vimconfig='vim ~/.vimrc'
alias startupconfig='vim ~/.bash_startup'
alias aliasconfig='vim ~/.bash_aliases'

# Reload audio and network adapter:
alias audioreload='pulseaudio --kill'
alias wifireload='sudo killall NetworkManager; sudo NetworkManager &'

# Reload the shell; 7/17/17 -> I always type ref then tab, so why not shorten refresh to ref?
alias ref='source ~/.bashrc'

# Working with a todo list
alias addtodo='vim ~/.todo.md'
alias todo='pandoc ~/.todo.md | lynx -stdin'

# APT shortcuts
alias clean='sudo apt-get clean'
alias update='sudo apt-get update; sudo apt-get upgrade -y'
alias purge='sudo apt-get purge'
alias grab='sudo apt-get install -y'
alias peek='apt show'

# Tmux shortcuts
alias ta='tmux attach'

# Git aliases
alias commit='git add --all; git commit; git push'
alias newrepo='./newrepo.sh'
alias gp = 'git pull'

# Specific vim text files:
alias editalbumlist='vim ~/Documentos/ExportacaoEvernote/Música/ListaAlbums'
alias journal='vim ~/diario.txt'
alias draft='vim /tmp/scratch.md'

# Misc stuff
alias briefme='less ~/.briefing.txt'
alias budget='libreoffice --calc ~/Documentos/Controle\ financeiro.ods'
alias lynx='lynx -nofilereferer -noreferer -anonymous -cookies -vikeys'
alias oblique='python3 ~/oblique-strategies/oblique.py'
alias tuxsay='cowsay -f tux'
alias unix='curl -L git.io/unix'
alias weather='echo -e "--------------------------------------------------------------------------------\n ** If you want the full weather report, type weather_full. **\n--------------------------------------------------------------------------------\n"; weather_full | head -17'
alias weather_full='curl wttr.in/Ponta_grossa'
alias xcopy='python3 ~/Python\ Scripts/custom_xclip.py'
