# Makefile for ThuThesis

PACKAGE = thuthesis
# (DCMMC)
THESIS_PROJECT = thuthesis-project

SOURCES = $(PACKAGE).ins $(PACKAGE).dtx
CLSFILE = dtx-style.sty $(PACKAGE).cls

LATEXMK = latexmk
SHELL  := /bin/bash

# make deletion work on Windows
ifdef SystemRoot
	RM = del /Q
else
	RM = rm -f
endif

# (DCMMC)
.PHONY: all all-dev clean distclean thesis-project doc viewdoc cls savepdf FORCE_MAKE %.pdf

# (DCMMC)
thesis-project: $(THESIS_PROJECT).pdf

all: thesis-project

all-dev: doc all

cls: $(CLSFILE)

$(CLSFILE): $(SOURCES)
	xetex $(PACKAGE).ins

doc: $(PACKAGE).pdf

$(THESIS_PROJECT).pdf: cls FORCE_MAKE
	$(LATEXMK) $(THESIS_PROJECT)

$(PACKAGE).pdf: cls FORCE_MAKE
	$(LATEXMK) $(PACKAGE).dtx

viewdoc: doc
	$(LATEXMK) -pv $(PACKAGE).dtx

# (DCMMC)
clean:
	$(LATEXMK) -c $(PACKAGE).dtx $(THESIS) $(SPINE) $(THESIS_PROJECT) *.pdf
	-@$(RM) -rf *~ main-survey.* main-translation.* _markdown_thuthesis* thuthesis.markdown.*  *.ps *.dot *.psbackup

# (DCMMC)
cleanall: clean
	-@$(RM) $(PACKAGE).pdf $(THESIS_PROJECT).pdf

distclean: cleanall
	-@$(RM) $(CLSFILE)
	-@$(RM) -r dist

# (DCMMC)
%.pdf : %.tex cls FORCE_MAKE
# @echo "Test!" $< $@
	$(LATEXMK) $<
