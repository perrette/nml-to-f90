#
# This file was adapted from Alex Robinson's nml project
# https://github.com/alex-robinson/nml
#
.SUFFIXES: .f .F .F90 .f90 .o .mod
.SHELL: /bin/sh

.PHONY : usage
usage:
	@echo ""
	@echo "    * USAGE * "
	@echo ""
	@echo " make src      : compile the source code from namelist"
	@echo " make example  : compile the example program"
	@echo " make test     : compile the test program (somewhat more exhaustive)"
	@echo " make test_nml : like example but using nml.f90 library"
	@echo " make test_nml_src : compile ioparams.f90 for test_nml"
	@echo " make clean    : cleans object and executable files"
	@echo ""

# PATH options
objdir = .obj
libdir = ..

# Command-line options at make call
ifort ?= 0
debug ?= 1

ifeq ($(ifort),1)
    FC = ifort 
else
    FC = gfortran
endif 

ifeq ($(ifort)$(debug),11)
    ## IFORT OPTIONS DEBUG ##
    LFLAGS		 = 
    FLAGS        = -module $(objdir) -L$(objdir)
    DFLAGS   = -C -traceback -ftrapuv -fpe0 -check all -vec-report0
	# -w 
endif
ifeq ($(ifort)$(debug),10)
    ## IFORT OPTIONS NO-DEBUG ##
    LFLAGS		 = 
    FLAGS        = -module $(objdir) -L$(objdir)
    DFLAGS   = -vec-report0 -O3
endif
ifeq ($(ifort)$(debug),01)
    ## GFORTRAN OPTIONS ##
    LFLAGS		 = 
    FLAGS        = -I$(objdir) -J$(objdir)
    DFLAGS   = -w -p -ggdb -ffpe-trap=invalid,zero,overflow,underflow \
	       -fbacktrace -fcheck=all -fbackslash
endif
ifeq ($(ifort)$(debug),00)
    LFLAGS		 = 
    FLAGS        = -I$(objdir) -J$(objdir)
    DFLAGS   = -O3 -fbackslash
endif

## Individual libraries or modules ##
$(objdir)/nml.o: lib/nml.f90
	$(FC) $(DFLAGS) $(FLAGS) -c -o $@ $<

$(objdir)/%.o: %.f90
	$(FC) $(DFLAGS) $(FLAGS) -c -o $@ $<

## Complete programs

example: $(objdir)/ioparams.o $(objdir)/example.o
	$(FC) $(DFLAGS) $(FLAGS) -o example.x $^ $(LFLAGS)
	@echo " "
	@echo "    example.x is ready."
	@echo " "

test: $(objdir)/ioparams.o $(objdir)/test.o
	$(FC) $(DFLAGS) $(FLAGS) -o test.x $^ $(LFLAGS)
	@echo " "
	@echo "    test.x is ready."
	@echo " "

test_nml: $(objdir)/nml.o $(objdir)/ioparams.o $(objdir)/test_nml.o
	$(FC) $(DFLAGS) $(FLAGS) -o test_nml.x $^ $(LFLAGS)
	@echo " "
	@echo "    test_nml.x is ready."
	@echo " "

test_nml_src: nml2f90/nml2f90.py
	python -m nml2f90.nml2f90 namelist.nml --io-nml-nml --command-line
	@echo " "
	@echo "  ioparams.f90 is ready."
	@echo " "

src: nml2f90/nml2f90.py
	python -m nml2f90.nml2f90 namelist.nml ioparams --io-nml --command-line --set-get-param -v
	@echo " "
	@echo "  ioparams.f90 is ready."
	@echo " "

clean:
	rm -f *.x $(objdir)/*.o $(objdir)/*.mod namelist.template.nml *.pyc
