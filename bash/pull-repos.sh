# You can do it like this: crawl the home folder's directories looking for .git
# and do a git pull if that folder is found.

for base_dir in $(find ~ -type d)
do
      if [ -d "$base_dir/.git" ]
  then
    echo "Git repo found in $base_dir"
    echo "Pulling repo"
    cd "$base_dir"
    git pull
  fi
done

echo "Finishing"
cd
