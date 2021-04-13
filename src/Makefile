# 
# Unix Makefile for WEXLER solute transport programs
# Last modified 04/01/96, rsregan
#
# Usage:
# make [BINDIR=path_to_install_to] [install] [| tee makeout]
#
# Examples of values for path_to_install_to are:
#    make BINDIR=/usr/opt/bin
#    make BINDIR=$HOME/bin
#
# If BINDIR is specified, the executable is linked to the specified
# directory.  The default value of BINDIR is ../bin.  Note, the installer
# must have sufficient access rights to delete and then create a link for
# the executable files in the directory that BINDIR is set to.
# If 'install' is specified only this link is made (executable must already
# exist in the solutetr.1/bin directory).  If 'install' is not specified,
# the source is recompiled according to the make dependency rules in this file.

# this description file uses the Bourne shell
SHELL = /bin/sh

# Disspla graphic library
GraphLib = -lmisalign /usr/opt/disspla11/lib/libint.a \
                /usr/opt/disspla11/lib/libdis77.a -lmalloc -lX11

# Fortran compiler flags (Green Hills):
#   C - check subranges and array bounds
#   u - default data type for undeclareds is undefined
#   g - generate source level symbolic debug information
#   O - optimization
# (Note: set FFLAGS to: OptM to generate optimized code
#                       DbgF to generate debug code)
# for SunOS use OptM = -O
OptM   = -u -O
DbgF   = -C -g
FFLAGS = $(OptM)

# Compiler name
F77    = f77

# Operating system name (DGUX, SGI, Solaris)
OS     = DGUX

#*******************************************************************
#***** You should not need to modify anything below this line. *****
#*******************************************************************

PGM1   = seminf
PGM2   = finite
PGM3   = point2
PGM4   = stripf
PGM5   = stripi
PGM6   = gauss
PGM7   = point3
PGM8   = patchf
PGM9   = patchi
PGM10  = point3_mod
SUBS   = ofile.o title.o dgdiss.o lenchr.o locsubs.$(OS).o
OBJS1  = $(PGM1).o subs1.o plot1d.o exerfc.o $(SUBS)
OBJS2  = $(PGM2).o subs1.o plot1d.o $(SUBS)
OBJS3  = $(PGM3).o subs2.o rdplot.o plot2d.o cntour.o glqpts.o $(SUBS)
OBJS4  = $(PGM4).o subs2.o rdplot.o plot2d.o cntour.o exerfc.o $(SUBS)
OBJS5  = $(PGM5).o subs2.o rdplot.o plot2d.o cntour.o glqpts.o exerfc.o $(SUBS)
OBJS6  = $(PGM6).o subs2.o rdplot.o plot2d.o cntour.o glqpts.o $(SUBS)
OBJS7  = $(PGM7).o subs3.o rdplot.o plot2d.o cntour.o exerfc.o $(SUBS)
OBJS8  = $(PGM8).o subs3.o rdplot.o plot2d.o cntour.o exerfc.o $(SUBS)
OBJS9  = $(PGM9).o subs3.o rdplot.o plot2d.o cntour.o glqpts.o exerfc.o $(SUBS)
OBJS10 = $(PGM10).o subs3.o rdplot.o plot2d.o cntour.o exerfc.o $(SUBS)

# Default location to link executables on installation
BINDIR = ../bin
BDir   = ../bin
EXE1   = $(BDir)/$(PGM1)
EXE2   = $(BDir)/$(PGM2)
EXE3   = $(BDir)/$(PGM3).exe
EXE4   = $(BDir)/$(PGM4)
EXE5   = $(BDir)/$(PGM5).exe
EXE6   = $(BDir)/$(PGM6).exe
EXE7   = $(BDir)/$(PGM7)
EXE8   = $(BDir)/$(PGM8)
EXE9   = $(BDir)/$(PGM9).exe
EXE10  = $(BDir)/$(PGM10)
Exes   = $(EXE1) $(EXE2) $(EXE3) $(EXE4) $(EXE5) $(EXE6) $(EXE7) $(EXE8) $(EXE9) $(EXE10)
BIN1   = $(BINDIR)/$(PGM1)
BIN2   = $(BINDIR)/$(PGM2)
BIN3   = $(BINDIR)/$(PGM3)
BIN4   = $(BINDIR)/$(PGM4)
BIN5   = $(BINDIR)/$(PGM5)
BIN6   = $(BINDIR)/$(PGM6)
BIN7   = $(BINDIR)/$(PGM7)
BIN8   = $(BINDIR)/$(PGM8)
BIN9   = $(BINDIR)/$(PGM9)
BIN10  = $(BINDIR)/$(PGM10)
Bins   = $(BIN1) $(BIN2) $(BIN3) $(BIN4) $(BIN5) $(BIN6) $(BIN7) $(BIN8) $(BIN9) $(BIN10)
CURDIR = `pwd`

# Rules
all: chkdir $(Exes) install
	@echo; echo Done making all, files are now up-to-date.

$(EXE1): $(OBJS1)
	@echo
	$(F77) $(FFLAGS) $(OBJS1) $(GraphLib) -o $@
	strip $@

$(EXE2): $(OBJS2)
	@echo
	$(F77) $(FFLAGS) $(OBJS2) $(GraphLib) -o $@
	strip $@

$(EXE3): $(OBJS3)
	@echo
	$(F77) $(FFLAGS) $(OBJS3) $(GraphLib) -o $@
	strip $@

$(EXE4): $(OBJS4)
	@echo
	$(F77) $(FFLAGS) $(OBJS4) $(GraphLib) -o $@
	strip $@

$(EXE5): $(OBJS5)
	@echo
	$(F77) $(FFLAGS) $(OBJS5) $(GraphLib) -o $@
	strip $@

$(EXE6): $(OBJS6)
	@echo
	$(F77) $(FFLAGS) $(OBJS6) $(GraphLib) -o $@
	strip $@

$(EXE7): $(OBJS7)
	@echo
	$(F77) $(FFLAGS) $(OBJS7) $(GraphLib) -o $@
	strip $@

$(EXE8): $(OBJS8)
	@echo
	$(F77) $(FFLAGS) $(OBJS8) $(GraphLib) -o $@
	strip $@

$(EXE9): $(OBJS9)
	@echo
	$(F77) $(FFLAGS) $(OBJS9) $(GraphLib) -o $@
	strip $@

$(EXE10): $(OBJS10)
	@echo
	$(F77) $(FFLAGS) $(OBJS10) $(GraphLib) -o $@
	strip $@

chkdir:
# Create ../bin directory, if necessary
	@if [ ! -d $(BDir) ]; then \
		mkdir $(BDir); echo Created directory $(BDir); fi

install:
# Create directory for binary, if necessary
	@if [ ! -d $(BINDIR) ]; then \
		mkdir $(BINDIR); echo Created directory $(BINDIR); fi
#
# Put path name of distribution directory into test scripts
	@cd ..;sed "s?TOPDIR=.\{0,80\}?TOPDIR=$(CURDIR)?" test/test.sh > test/t.tmp; \
	sed "s?TOPDIR=.\{0,80\}?TOPDIR=$(CURDIR)?" test/check.sh > test/c.tmp
	@cd ../test; mv -f t.tmp test.sh; mv -f c.tmp check.sh; \
	chmod 755 test.sh check.sh
#
# Put path name of distribution directory into script
	@cd ..;sed "s?TOPDIR=.\{0,80\}?TOPDIR=$(CURDIR)?" bin/$(PGM3) > bin/p3.tmp
	@cd ..;sed "s?TOPDIR=.\{0,80\}?TOPDIR=$(CURDIR)?" bin/$(PGM5) > bin/p5.tmp
	@cd ..;sed "s?TOPDIR=.\{0,80\}?TOPDIR=$(CURDIR)?" bin/$(PGM6) > bin/p6.tmp
	@cd ..;sed "s?TOPDIR=.\{0,80\}?TOPDIR=$(CURDIR)?" bin/$(PGM9) > bin/p9.tmp
	@cd $(BDir); mv -f p3.tmp $(PGM3); mv -f p5.tmp $(PGM5); \
		mv -f p6.tmp $(PGM6); mv -f p9.tmp $(PGM9); \
		chmod 755 $(PGM3) $(PGM5) $(PGM6) $(PGM9)
# Link executable to BINDIR
	@if [ ! -s $(EXE1) ]; then \
		echo Executable $(EXE1) does not exist,; \
		echo use one of the following commands to generate it:; \
		echo "      make"; echo "      make all"; \
	else \
		if [ $(BINDIR) != $(BDir) ]; then \
			rm -f $(Bins); \
			cd ..; ln -s $(CURDIR)/bin/$(PGM1) $(BIN1); \
			ln -s $(CURDIR)/bin/$(PGM2) $(BIN2); \
			ln -s $(CURDIR)/bin/$(PGM3) $(BIN3); \
			ln -s $(CURDIR)/bin/$(PGM4) $(BIN4); \
			ln -s $(CURDIR)/bin/$(PGM5) $(BIN5); \
			ln -s $(CURDIR)/bin/$(PGM6) $(BIN6); \
			ln -s $(CURDIR)/bin/$(PGM7) $(BIN7); \
			ln -s $(CURDIR)/bin/$(PGM8) $(BIN8); \
			ln -s $(CURDIR)/bin/$(PGM9) $(BIN9); \
			ln -s $(CURDIR)/bin/$(PGM10) $(BIN10); \
			echo; echo Executables $(Bins) have been updated.; \
			echo; echo Be sure directory $(BINDIR) is in your PATH.; \
		else \
			cd $(BDir); echo; echo Be sure directory `pwd` is in your PATH.; \
		fi; \
	fi

clean:
	@if [ $(BINDIR) != $(BDir) ]; \
		then rm -f $(Bins); \
	fi
	rm -f $(Exes) *.o
	@echo; echo Removed files generated by make.
#
# source code dependencies
gauss.o:  gauss.f  dimens.inc pltdat.inc units.inc
stripf.o: stripf.f dimens.inc pltdat.inc units.inc
stripi.o: stripi.f dimens.inc pltdat.inc units.inc
point2.o: point2.f dimens.inc pltdat.inc units.inc
point3.o: point3.f dimens.inc pltdat.inc units.inc
patchf.o: patchf.f dimens.inc pltdat.inc units.inc
patchi.o: patchi.f dimens.inc pltdat.inc units.inc
plot2d.o: plot2d.f dimens.inc pltdat.inc units.inc
finite.o: finite.f dimens.inc units.inc
seminf.o: seminf.f dimens.inc units.inc
subs1.o:  subs1.f  dimens.inc units.inc
subs2.o:  subs2.f  dimens.inc pltdat.inc units.inc
subs3.o:  subs3.f  dimens.inc pltdat.inc units.inc
cntour.o: cntour.f dimens.inc pltdat.inc
point3_mod.o: point3_mod.f dimens.inc pltdat.inc units.inc
