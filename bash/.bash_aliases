# This is my alias file.

# Came with the standard .bashrc:

alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Rails development stuff
alias rs='bundle exec rspec'
alias rdbm='bundle exec rake db:migrate'
alias rdbs='bundle exec rake db:drop db:create db:migrate'
alias b='bundle'

alias t='cd /tmp'
alias l='ls -CF'
alias la='ls -A'
alias ll='ls -alF'
alias ls="TERM=ansi ls --color=always"

alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias grep='grep --color=auto'

#-----------------MY-ALIASES---------------------------------------------------

# Custom programs:

# When typing emacs in the terminal, start the no-window version one:
# alias emacs='emacs -nw'

# Change dotfiles:
alias bashconfig='vim ~/.bashrc'
alias vimconfig='vim ~/.vimrc'
alias startupconfig='vim ~/.bash_startup'
alias aliasconfig='vim ~/.bash_aliases'

# Move quickly to common places
alias mdf='cd ~/my-dotfiles/'
alias fb='cd ~/file-bouncer/'

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
alias g='git'
alias gc='git checkout'
alias gp='git pull origin'
alias gs='git status'
alias gl='git log'
# This could be a complex function, I think.
alias grm='git fetch origin && git rebase origin/master --autosquash'

# Specific vim text files:

alias editalbumlist='vim ~/file-bouncer/ListaAlbums'
alias draft='vim /tmp/scratch.md'

# Check most used commands in bash history
alias freq="history | awk '{a[$2]++} END {for(i in a) { print a[i] \" \" i}}' | sort -rn | head -20"

# Misc stuff
alias budget='libreoffice --calc ~/Documentos/Controle\ financeiro.ods'
alias lynx='lynx -nofilereferer -noreferer -anonymous -cookies -vikeys'
alias tuxsay='cowsay -f tux'
alias unix='curl -L git.io/unix'
alias weather='echo -e "--------------------------------------------------------------------------------\n ** If you want the full weather report, type weather_full. **\n--------------------------------------------------------------------------------\n"; weather_full | head -17'
alias weather_full='curl -s wttr.in/Ponta_grossa'
alias clearswap='sudo swapoff -a && sudo swapon -a'

# Read books in the terminal with less!
alias neuromancer='curl http://www.lib.ru/GIBSON/neuromancer.txt | less'
alias count_zero='curl https://www.kataan.org/public/ebook/countzero.txt | less'

# Elixir stuff
alias md='mix deps.get'
# We can improve this one. The idea is to run mix ecto.setup when available.
alias mes='mix ecto.setup || mix ecto.create && mix ecto.migrate && mix run priv/repo/seeds.exs'
alias med='mix ecto.drop'
alias mt='mix test'
alias mts='mix test --stale'
alias mtf='mix test --failed'
alias mf='mix format'
alias mfc='mix format --check-formatted .'

# Docker aliases
alias dcrb='docker-compose run broker bash'
alias dcu='docker-compose up'
alias dcd='docker-compose down'

# Launch XAMPP for PHP practice
alias xampp='sudo /opt/lampp/xampp'
