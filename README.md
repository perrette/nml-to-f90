# nml-to-f90
Generate useful fortran source code from a single namelist

Warning:

For now, everything is assumed to come from params.f90, where all relevant 
derived types are defined. This is only one short step away from making things derived from a namelist directly. Work in progress.

preparatory phase (one time step in a project):
- edit gen_src.f90 so that these types are written to a dummy namelist (when a new type is added)
- edit 3 variables gen_src.py to make sure the relevant fortran modules are known, 
  and to modify the naming conventions at your convenience.

create the source code ioparams.f90 :

    make src  

compile the test program

    make nml2
