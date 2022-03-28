TITLE  := Whose_Voices
OUTPUT_DIR  := output

all: test-before compile clean count

query-dependencies:
	Rscript -e "source('./misc/functions.R'); \
	install_missing(path = '.', install_dependencies = TRUE); \
  install_missing(path = '.')

# needs littler package
test-before:
	r -i -e 'testthat::test_file("./tests/before.R", reporter = c("Fail", "CompactProgress"))'

compile:
	rm -rf $(OUTPUT_DIR)
	mkdir -p $(OUTPUT_DIR)
	Rscript -e "rmarkdown::render('./sage/sage.Rmd', output_file = '../$(OUTPUT_DIR)/$(TITLE).pdf')"
	Rscript -e "rmarkdown::render('README.Rmd', output_format = rmarkdown::github_document(html_preview = FALSE))"

move:
	mv ./sage/plots $(OUTPUT_DIR)
	cp ./paper/references.bib $(OUTPUT_DIR)/references.bib
	cp ./sage/sagej.cls $(OUTPUT_DIR)
	cp ./sage/sageh.bst $(OUTPUT_DIR)
	sed -i 's/..\/paper\///' ./$(OUTPUT_DIR)/$(TITLE).tex

clean:
	find . \
	-name '*.log' -o \
	-name '*.aux' -o \
	-name '*.toc' | \
	xargs rm -f; 
	
count:
	pdftotext "./$(OUTPUT_DIR)/$(TITLE).pdf" - | wc -w
	