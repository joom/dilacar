SOURCENAME=source
FILENAME=thesis
OUTPUTNAME=output
LATEX=xelatex

default: preview

compile:
	@cp $(SOURCENAME).tex $(FILENAME).tex
	@$(LATEX) $(FILENAME).tex
	@bibtex $(FILENAME) > /dev/null
	@$(LATEX) $(FILENAME).tex 2> /dev/null
	@$(LATEX) $(FILENAME).tex 2> /dev/null
	@$(LATEX) $(FILENAME).tex
	@mv $(FILENAME).pdf $(OUTPUTNAME).pdf

debug:
	cp $(SOURCENAME).tex $(FILENAME).tex
	$(LATEX) $(FILENAME).tex
	bibtex $(FILENAME)
	$(LATEX) $(FILENAME).tex
	$(LATEX) $(FILENAME).tex
	$(LATEX) $(FILENAME).tex
	mv $(FILENAME).pdf $(OUTPUTNAME).pdf

open:
	@open -a /Applications/Preview.app $(OUTPUTNAME).pdf

clean:
	@rm *.aux
	@rm *.log
	@rm $(FILENAME).*

preview: compile open clean
normal: compile clean
