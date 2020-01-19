#!/bin/bash

# Bash prompt customization.
# These are the colors we need.
BLUE="\[\033[0;34m\]"
CYAN="\[\033[0;36m\]"
GREEN="\[\033[1;32m\]"
LIGHT_GRAY="\[\033[0;37m\]"
LIGHT_RED="\[\033[1;31m\]"
MAGENTA="\[\033[0;35m\]"
RED="\[\033[0;31m\]"
VIOLET='\[\033[01;35m\]'
WHITE="\[\033[1;37m\]"
YELLOW="\[\033[01;33m\]"
# We need this to end the color sequence
ENDCOLOR="\e[0m";

function color_my_prompt {
  local __user_and_host="$RED★✿ $VIOLET\u@\h$RED ✿★:"
  # capital 'W': current directory, small 'w': full file path
  local __cur_location="$CYAN\w"
  local __git_branch_color="$GREEN"
  local __prompt_tail="\n$GREEN$"
  local __user_input_color="$ENDCOLOR"
  local __git_branch=$(__git_ps1);

  # colour branch name depending on state
  if [[ "${__git_branch}" =~ "*" ]]; then     # if repository is dirty
      __git_branch_color="$RED"
  elif [[ "${__git_branch}" =~ "$" ]]; then   # if there is something stashed
      __git_branch_color="$YELLOW"
  elif [[ "${__git_branch}" =~ "%" ]]; then   # if there are only untracked files
      __git_branch_color="$LIGHT_GRAY"
  elif [[ "${__git_branch}" =~ "+" ]]; then   # if there are staged files
      __git_branch_color="$CYAN"
  fi

  # Build the PS1 (Prompt String)
  PS1="$__user_and_host $__cur_location$__git_branch_color$__git_branch $__prompt_tail$__user_input_color "
}

# configure PROMPT_COMMAND which is executed each time before PS1
export PROMPT_COMMAND=color_my_prompt

# if .git-prompt.sh exists, set options and execute it
if [ locate git-prompt.sh ]; then
  GIT_PS1_SHOWDIRTYSTATE=true
  GIT_PS1_SHOWSTASHSTATE=true
  GIT_PS1_SHOWUNTRACKEDFILES=true
  GIT_PS1_SHOWUPSTREAM="auto"
  GIT_PS1_HIDE_IF_PWD_IGNORED=true
  GIT_PS1_SHOWCOLORHINTS=true
  source $(locate git-prompt.sh)
fi
