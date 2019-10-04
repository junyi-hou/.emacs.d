#!/bin/sh

maildir=( "${HOME}/.emacs.d/var/maildir/berkeley"
          "${HOME}/.emacs.d/var/maildir/personal" )

# first check whether it is connected to network
if ping -q -c 1 -W 1 8.8.8.8 > /dev/null; then
    for dir in "${maildir[@]}"; do
        cd $dir
        gmi sync > /dev/null
        echo "gmailier: syncing $dir..."
    done
else
    echo "gmailieer: network is down, skipping sync"
fi
