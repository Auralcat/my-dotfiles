#!/bin/bash

# Auto-pull all the Git repos in your home folder
echo "Doing a git pull in all the repositories in this computer..."

# Removing old .local_repos.txt if it exists
if [ -f ~/.local_repos.txt ]
then
  rm ~/.local_repos.txt
fi

for base_dir in $(find ~ -type d)
do
    if [ -d "$base_dir/.git" ]
  then
    echo "Git repo found in $base_dir" | tee -a ~/.local_repos.txt
    echo "Pulling repo"
    cd "$base_dir"
    git pull
  fi
done

echo "Finishing... found repo names were saved in ~/.local_repos.txt"
echo "If the shell takes too long to start up, you might want to remove the repos you don't use"
cd "$HOME"
