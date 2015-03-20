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
	@echo " make test     : compile the test program"
	@echo " make clean    : cleans object and executable files"
	@echo ""

# PATH options
objdir = .obj
libdir = ..

netcdf_inc = /opt/local/include
netcdf_lib = /opt/local/lib
netcdf_inc_ifort = /home/robinson/apps/netcdf/netcdf/include
netcdf_lib_ifort = /home/robinson/apps/netcdf/netcdf/lib
netcdf_inc = /usr/include
netcdf_lib = /usr/lib

# Command-line options at make call
ifort ?= 0
debug ?= 0 

ifeq ($(ifort),1)
    FC = ifort 
else
    FC = gfortran
endif 

ifeq ($(ifort),1)
	## IFORT OPTIONS ##
	FLAGS        = -module $(objdir) -L$(objdir) -I$(netcdf_inc_ifort)
	LFLAGS		 = -L$(netcdf_lib_ifort) -lnetcdf

	ifeq ($(debug), 1)
	    DFLAGS   = -C -traceback -ftrapuv -fpe0 -check all -vec-report0
	    # -w 
	else
	    DFLAGS   = -vec-report0 -O3
	endif
else
	## GFORTRAN OPTIONS ##
	FLAGS        = -I$(objdir) -J$(objdir) -I$(netcdf_inc)
	LFLAGS		 = -L$(netcdf_lib) -lnetcdff -lnetcdf

	ifeq ($(debug), 1)
	    DFLAGS   = -w -p -ggdb -ffpe-trap=invalid,zero,overflow,underflow \
	               -fbacktrace -fcheck=all -fbackslash
	else
	    DFLAGS   = -O3 -fbackslash
	endif
endif

## Individual libraries or modules ##
$(objdir)/%.o: %.f90
	$(FC) $(DFLAGS) $(FLAGS) -c -o $@ $<

## Complete programs

test: $(objdir)/params.o $(objdir)/ioparams.o $(objdir)/test.o
	$(FC) $(DFLAGS) $(FLAGS) -o test.x $^ $(LFLAGS)
	@echo " "
	@echo "    test.x is ready."
	@echo " "

src: $(objdir)/params.o 
	$(FC) $(DFLAGS) $(FLAGS) -o gen_src.x gen_src.f90 $^ $(LFLAGS)
	./gen_src.x
	python gen_src.py
	@echo " "
	@echo "  ioparams.f90 is ready."
	@echo " "

clean:
	rm -f *.x $(objdir)/*.o $(objdir)/*.mod namelist.template.nml *.pyc
