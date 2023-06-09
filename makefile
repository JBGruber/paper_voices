TITLE  := Whose_Voices
OUTPUT_DIR  := output
OUTPUT_FORMAT := aft-pdf
MAIN_QMD := paper/article.qmd

all: compile clean count

# needs littler package
test-before:
	r -i -e 'testthat::test_file("./tests/before.R", reporter = c("Fail", "CompactProgress"))'

compile:
	rm -rf $(OUTPUT_DIR)
	mkdir -p $(OUTPUT_DIR)
	cd $(OUTPUT_DIR);\
		quarto render ../$(MAIN_QMD) --to $(OUTPUT_FORMAT) --output $(TITLE).pdf

clean:
	find . \
	-name '*.log' -o \
	-name '*.aux' -o \
	-name '*.toc' | \
	xargs rm -f; 
	
count:
	pdftotext "./$(OUTPUT_DIR)/$(TITLE).pdf" - | wc -w
	
