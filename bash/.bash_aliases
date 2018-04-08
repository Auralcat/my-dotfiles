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

# When typing emacs in the terminal, start the no-window version one:
alias emacs='emacs -nw'

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

# Do a git-pull in all the repos in the system
alias agp='~/my-dotfiles/bash/auto_git_pull.sh'

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
alias gp='git pull'
alias gs='git status'

# Specific vim text files:

alias editalbumlist='vim ~/file-bouncer/ListaAlbums'
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
alias weather_full='curl -s wttr.in/Ponta_grossa'
alias xcopy='python3 ~/Python\ Scripts/custom_xclip.py'
alias clearswap='sudo swapoff -a && sudo swapon -a'

# Read books in the terminal with less!

alias neuromancer='curl http://www.lib.ru/GIBSON/neuromancer.txt | less'
alias count_zero='curl https://www.kataan.org/public/ebook/countzero.txt | less'
