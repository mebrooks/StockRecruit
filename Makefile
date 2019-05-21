R=R
# -> you can do    R=R-devel  make ....

PACKAGE= StockRecruitSET
# get VERSION from StockRecruitSET/DESCRIPTION  
## ("::" = expand only  once, but doesn't work in make <= 3.81)
VERSION := $(shell sed -n '/^Version: /s///p' StockRecruitSET/DESCRIPTION)

TARBALL := $(PACKAGE)_$(VERSION).tar.gz
ZIPFILE := =$(PACKAGE)_$(VERSION).zip

CPP_SRC := $(PACKAGE)/src/*.cpp

all:
	make doc-update
	make build-package
	make install
	make pdf


build-package: $(TARBALL)
$(TARBALL): $(PACKAGE)/NAMESPACE $(CPP_SRC)
	$(R) CMD build --resave-data=no $(PACKAGE)

install: $(TARBALL)
	$(R) CMD INSTALL --preclean $<
	@touch $@

doc-update: $(PACKAGE)/R/*.R
	echo "library(roxygen2);roxygenize(\"$(PACKAGE)\",roclets = c(\"collate\", \"rd\"))" | $(R) --slave
	@touch doc-update

namespace-update :: $(PACKAGE)/NAMESPACE
$(PACKAGE)/NAMESPACE: $(PACKAGE)/R/*.R
	echo "library(roxygen2);roxygenize(\"$(PACKAGE)\",roclets = c(\"namespace\"))" | $(R) --slave

## To enable quick compile, run from R:
##    library(TMB); precompile(flags="-O0 -g")
quick-install: enum-update $(PACKAGE)/src/BevertonHolt.so
	enum-update $(PACKAGE)/src/Ricker.so
	enum-update $(PACKAGE)/src/contHockey.so
	$(R) CMD INSTALL $(PACKAGE)

$(PACKAGE)/src/BevertonHolt.so: $(PACKAGE)/src/BevertonHolt.cpp
	cd $(PACKAGE)/src; echo "library(TMB); compile('BevertonHolt.cpp')" | $(R) --slave
$(PACKAGE)/src/Ricker.so: $(PACKAGE)/src/Ricker.cpp
	cd $(PACKAGE)/src; echo "library(TMB); compile('Ricker.cpp')" | $(R) --slave
$(PACKAGE)/src/contHockey.so: $(PACKAGE)/src/contHockey.cpp
	cd $(PACKAGE)/src; echo "library(TMB); compile('contHockey.cpp')" | $(R) --slave


unexport TEXINPUTS
pdf: $(PACKAGE).pdf
$(PACKAGE).pdf: $(PACKAGE)/man/*.Rd
	rm -f $(PACKAGE).pdf
	$(R) CMD Rd2pdf --no-preview $(PACKAGE)

check:
	$(R) CMD check $(PACKAGE)

## *NOT* using 'R --vanilla' : then cannot find testthat, TMB, etc they are installed into R's "system" library

test:
	echo "devtools::test('StockRecruitSET')" | $(R) --slave

quick-check: quick-install ex-test

ex-test:
	echo "library(StockRecruitSET); example(StockRecruitSET)" | $(R) --slave


unlock:
	\rm -rf `Rscript --vanilla -e 'writeLines(.Library)'`/00LOCK-stockrecruit
#               ------------------------------------------ = R's system library
#	rm -rf ${R_LIBS}/00LOCK-stockrecruit
##               ^^^^^^^ This only works if R_LIBS contains a single directory and the same that 'R CMD INSTALL' uses..

clean:
	\rm -f install doc-update
