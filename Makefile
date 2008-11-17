#------------------------------------------------------------------------------
# To compile boundfit properly, you must follow these three steps:
# make include
# make boundfit
# make clean
#------------------------------------------------------------------------------
# basic definitions
VERSION = 01.0
NDATAMAX= 100000
LENLINEA= 10000
FCOMPIL = gfortran -g -O3 -Wall
# macro definitions
FSOURCE = binsearch.f \
          boundfit.f \
          cleantab.f \
          combpf.f \
          cubspl.f \
          cubsplx.f \
          downhill.f \
          factorialpf.f \
          fextrae.f \
          findmml.f \
          fpoly.f \
          indexr.f \
          iofunctions.f \
          leenewfile.f \
          merge_knots.f \
          pseudofit.f \
          polfit.f \
          ranred.f \
          ranspl.f \
          saveresult.f \
          splfit.f \
          systemfunction.f \
          truebeg.f \
          truelen.f \
          yfunk_pseudo.f \
          yfunk_splfit.f \
          yfunk_splfit1.f \
          yfunk_splfit2.f \
          yfunk_splfit3.f
FOBJECT = $(FSOURCE:.f=.o)
# Default rule to create program
boundfit:  $(FOBJECT)
	$(FCOMPIL) -o $@ $(FOBJECT)
# Target to clean object modules
clean:    $(FOBJECT)
	rm -f $(FOBJECT)
	rm -f lenlinea.inc ndatamax.inc version.inc
# Target to touch source modules
touch:
	touch $(FSOURCE)
# Target to create the file xpgpdir.inc
include:
	rm -f clinea.inc
	echo "        INTEGER LENLINEA" > lenlinea.inc
	echo "        PARAMETER(LENLINEA=$(LENLINEA))" >> lenlinea.inc
	rm -f ndatamax.inc
	echo "        INTEGER NDATAMAX" > ndatamax.inc
	echo "        PARAMETER(NDATAMAX=$(NDATAMAX))" >> ndatamax.inc
	rm -f version.inc
	echo "        CHARACTER*4 VERSION" > version.inc
	echo "        PARAMETER(VERSION='$(VERSION)')" >> version.inc
	touch $(FSOURCE)
# second level dependencies
.f.o: $(FSOURCE)
	$(FCOMPIL) -c $?
# definitions
.PRECIOUS: boundfit
