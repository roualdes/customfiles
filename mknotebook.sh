#! /bin/zsh -

setopt extended_glob

for f in *[:alnum:]#.tif; do
    mv "$f" $(padfiledigits "$f")
done

tiffcp *[:alnum:]#.tif tmp.tif
tiff2pdf tmp.tif > tmp.pdf
# https://blog.omgmog.net/post/compressing-pdf-from-your-mac-or-linux-terminal-with-ghostscript/
gs -sDEVICE=pdfwrite -dNOPAUSE -dQUIET -dBATCH -dPDFSETTINGS=/screen -dCompatibilityLevel=1.4 -sOutputFile="$1" tmp.pdf
rm tmp.tif tmp.pdf
