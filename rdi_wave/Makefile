all: notes.pdf 04.out 04_chunk_overview.pdf 04_temperature.pdf

%.out: %.R
	Rscript $< > $@
%.pdf: %.Rmd
	R --no-save -e 'library(rmarkdown); render("$<", "pdf_document", encoding="UTF-8")'
