#!/bin/bash

# Script used to automatically generate README.Rmd from index.Rmd

# Launch the script from the main sen2rts directory

# Replace internal links with complete links to the online documentation 
# at https://sen2rts.ranghetti.info
sed -e "s/](\([^\(\)]*\)\.md)/](https:\/\/sen2rts.ranghetti.info\/\1\.html)/g" index.Rmd > temp.Rmd
sed -e "s/https\:\/\/sen2rts\.ranghetti\.info\/\.github\/CODE\-OF\-CONDUCT\.html/blob\/master\/.github\/CODE-OF-CONDUCT.md/g" temp.Rmd > README.Rmd
rm temp.Rmd
