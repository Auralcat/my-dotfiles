#!/bin/bash

# Batch-add todo items to calcurse

touch tmpaddtodo
echo "# This is a temporary file meant to push todo items to calcurse." >> tmpaddtodo
echo "# Syntax: [priority] Item" >> tmpaddtodo
echo "# Please delete these lines when you're ready to push the items." >> tmpaddtodo
vim tmpaddtodo
echo "Adding items to todo..."
cat tmpaddtodo >> .calcurse/todo
echo "Done!"
rm tmpaddtodo
calcurse -t
