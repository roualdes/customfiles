#! /bin/zsh -

setopt extended_glob

for f in *[:alnum:]#.tif; do
    mv "$f" $(padfiledigits "$f")
done

tiffcp *[:alnum:]#.tif tmp.tif
tiff2pdf tmp.tif > $1
rm tmp.tif
