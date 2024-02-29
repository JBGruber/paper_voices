TITLE  := Whose_Voices
OUTPUT_DIR  := output
OUTPUT_FORMAT := aft-pdf
MAIN_QMD := paper/article.qmd

all: test-before compile split clean count

# needs littler package
test-before:
	r -i -e 'testthat::test_file("./tests/before.R", reporter = c("Fail", "CompactProgress"))'

compile:
	rm -rf $(OUTPUT_DIR)
	mkdir -p $(OUTPUT_DIR)
	cd $(OUTPUT_DIR);\
		quarto render ../$(MAIN_QMD) --to $(OUTPUT_FORMAT) --output $(TITLE)_all.pdf
	cd $(OUTPUT_DIR);\
		quarto render ../paper/title_page.qmd --to aft-pdf --output title_page.pdf
split:
	cd $(OUTPUT_DIR)
	pdftk $(OUTPUT_DIR)/$(TITLE)_all.pdf cat 52-end output $(OUTPUT_DIR)/"Supplementary Information file.pdf"
	pdftk $(OUTPUT_DIR)/$(TITLE)_all.pdf cat 1-52 output $(OUTPUT_DIR)/$(TITLE).pdf
	 
clean:
	find . \
	-name '*.log' -o \
	-name '*.aux' -o \
	-name '*.toc' | \
	xargs rm -f;
	
count:
	pdftotext "./$(OUTPUT_DIR)/$(TITLE).pdf" - | wc -w
	
