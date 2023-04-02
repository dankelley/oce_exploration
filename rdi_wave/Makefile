all: notes.pdf \
	04.out 04_chunk_overview.pdf 04_temperature.pdf 05.out 05_temperature.pdf \
	06.out 06_HPR.pdf

%.out: %.R
	Rscript $< > $@
%.pdf: %.Rmd
	R --no-save -e 'library(rmarkdown); render("$<", "pdf_document", encoding="UTF-8")'
