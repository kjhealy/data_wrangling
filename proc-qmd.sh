#!/usr/bin/env sh

## Convert colors to quarto
perl -pi -e 's/(\.kjh-)(orange|red|green|yellow|blue|lblue|pink)(\[.*?\])/$3\{.fg-$2\}/g' $1


## Convert centered and scaled images to quarto
perl -pi -e 's/\.center\[!\[:scale (\d{1,3}%) "(.*?)"\]\((.*?)\)\]/![$2]($3)\{fig-align="center" width=$1\}/g' $1

## Convert images pulled left or right
perl -pi -e 's/\.pull-(left|right)\[!\[:scale (\d{1,3}%) "(.*?)"\]\((.*?)\)\]/![]($4)\{fig-align="center" width=$2\}/g' $1
