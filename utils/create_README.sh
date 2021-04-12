#!/bin/bash

# Script used to automatically generate README.Rmd from index.Rmd

# Launch the script from the main sen2rts directory

# 1. Replace internal links with complete links to the online documentation 
#    at http://sen2rts.ranghetti.info
sed -e "s/](\([^\(\)]*\)\.md)/](http:\/\/sen2rts.ranghetti.info\/\1\.html)/g" index.Rmd > README.Rmd

# 2. Copy images (for pkgdown)
cp -R man/figures/README docs/reference/figures/
