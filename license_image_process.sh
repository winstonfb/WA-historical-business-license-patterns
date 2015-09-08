#!/bin/bash
mogrify -colorspace gray -filter cubic -resize 300% -unsharp 7x3+0.0 *.jpg
list="ls *.jpg"
for i in $list; do tesseract $i ${i%.jpg}; done
