#!/usr/bin/env sh


## Remove slideopts line
perl -pi -e 's/^kjh_set_xaringan_opts\(\)//' $1

## Remove superfluous classes and sizing
perl -pi -e 's/^\.class-info\[//' $1
perl -pi -e 's/^\.class-info\[//' $1
perl -pi -e 's/^\.SMALL\[//' $1

## Convert colors to quarto
perl -pi -e 's/(\.kjh-)(orange|red|green|yellow|blue|lblue|pink)(\[.*?\])/$3\{.fg-$2\}/g' $1

## Convert centered and scaled images to quarto
perl -pi -e 's/\.center\[!\[:scale (\d{1,3}%) "(.*?)"\]\((.*?)\)\]/![$2]($3)\{fig-align="center" width=$1\}/g' $1

## Convert images pulled left or right
perl -pi -e 's/\.pull-(left|right)\[!\[:scale (\d{1,3}%) "(.*?)"\]\((.*?)\)\]/![]($4)\{fig-align="center" width=$2\}/g' $1


## Convert blank slides to huge
perl -pi -e 's/# \.huge\.middle\.squish\d{1}\[(.*)]/:::{.huge}\n $1 \n:::/' $1

## Remove layout: true and class: title title-1

perl -pi -e 's/^layout: true//' $1
perl -pi -e 's/^class: title title-1//' $1

# Remove class line
perl -pi -e 's/class: center middle main-title section-title-1//' $1

# Common image pattern replace
perl -pi -e 's/\.center\[!\[:scale \d{1,3}%\](\(.*?\))\]/![]$1/' $1

# Common aside
perl -pi -e 's/\.smaller\.footnote\[(.*?)\]/::: aside\n $1\n:::/' $1
perl -pi -e 's/\.tiny\.footnote\[(.*?)\]/::: aside\n $1\n:::/' $1


# chunk_reveal to chunq_reveal
perl -pi -e 's/chunk_reveal/chunq_reveal/' $1

perl -pi -e 's/(chunq_reveal.*?)widths = c\((\d{1,2}),(\d{1,2})\)/$1 lcolw="$2", rcolw="$3"/' $1