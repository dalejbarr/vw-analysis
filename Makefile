SRCDATA := $(wildcard data-raw/*.csv)

default : 
	quarto render
	cp data-raw.zip docs/

table-images/eye-data.png : generate-table-images.R $(SRCDATA)
	Rscript generate-table-images.R

