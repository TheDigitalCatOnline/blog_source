#!/bin/bash

if [[ -z $1 ]]; then echo "$0 <input_image>"; exit 1; fi

ls -l $1
cp $1 $1.old
mogrify -sampling-factor 4:2:0 -strip -quality 80 -interlace JPEG -colorspace RGB $1
ls -l $1
echo "Original image saved in $1.old"