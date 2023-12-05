SSH_USER = kjhealy@kjhealy.co
DOCUMENT_ROOT = ~/public/kjhealy.co/public_html/dw
PUBLIC_DIR = _site

## Convert single Rmd to HTML slide file
# Wildcard will match e.g. make slides/01-overview.Rmd
%.Rmd: .FORCE
	Rscript build/render_one_slide.R $@

## Convert single HTML slide file to PDF file
# Wildcard will match e.g. make slides/01-overview.html
%.html: .FORCE
	Rscript build/decktape_one_file.R $@

## Make all pdfs
pdfs:
	Rscript -e "suppressMessages(library(knitr));suppressMessages(library(tidyverse)); kjhslides::kjh_decktape_all_slides()"

## Purl all Rmds
code: .FORCE
	Rscript -e "suppressMessages(library(knitr));suppressMessages(library(tidyverse)); kjhslides::kjh_purl_all_slides()"
	find ./code -name '*.R' -type f | xargs gsed -i '1,20d'

## Make all Rmds into HTML slides
slides: .FORCE
	#Rscript -e "suppressMessages(library(knitr));suppressMessages(library(tidyverse)); kjhslides::kjh_render_all_slides()"
	quarto render

clean:
	find slides -type d -name '*_files' -prune -print -exec rm -rf {} +
	find slides -type d -name 'libs' -prune -print -exec rm -rf {} +
	find slides -type f -name '*.html' -prune -print -exec rm -f {} +
	find code -type f -name '*.R' -prune -print -exec rm -f {} +
	find pdf_slides -type f -name '*.pdf' -prune -print -exec rm -f {} +

deploy:
	rsync --exclude='.DS_Store' -Prvzce 'ssh -p 22' --delete-after $(PUBLIC_DIR) $(SSH_USER):$(DOCUMENT_ROOT)


.PHONY: clean

.FORCE:

