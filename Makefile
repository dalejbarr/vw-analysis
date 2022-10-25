SRCDATA := $(wildcard data-raw/*.csv)

default :
	quarto render

table-images/eye-data.png : generate-table-images.R $(SRCDATA)
	Rscript generate-table-images.R
