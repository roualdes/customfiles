if [ "$1" = "push" ]; then
    
    rsync -vah -e ssh \
          --exclude="node_modules/" \
          --exclude=".DS_Store" \
          --exclude=".p.kdbx.lock" \
          --exclude=".Rhistory" \
          --exclude=".juliahistory" \
          --exclude="*_files" \
          # --delete --delete-excluded
          ~/dos/ kauai:/mnt/dos/
    
elif [ "$1" = "pull" ]; then
    
    rsync -vah -e ssh \
          kauai:/mnt/dos/ ~/dos/
    
else
    echo "unknown command"
fi
   
