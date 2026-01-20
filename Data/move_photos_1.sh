#!/bin/bash

cd /Volumes/NO\_NAME/DCIM/100DSCIM/
for file in *; do
  # Check if it is a file
  if [ -f "$file" ]; then
    # Get filename without extension
    base="${file%.*}"
    # Get the extension
    ext="${file##*.}"
    
    # Rename
    # If the file has no extension, just append _1
    if [ "$base" == "$file" ]; then
      mv "$file" "/Users/danalapides/Documents/Jupyter/TNC_cameras/Data/${file}_1"
    else
      mv "$file" "/Users/danalapides/Documents/Jupyter/TNC_cameras/Data/${base}_1.${ext}"
    fi
  fi
done
cd ~/Documents/Jupyter/TNC_cameras/Code
