# nml-to-f90
Generate fortran source code from a single namelist

Start from a namelist file namelist.nml and create ioparams.f90, 
to be used as follow:

    use ioparams, only: group1_t, group2_t, ...
    use ioparams, only: read_nml, write_nml, get_param, set_param

where `<group>_t` are newly defined types created from namelist groups
and the four subroutines are handy I/O and set/get interfaces for the
above defined types.

create the source code ioparams.f90 :

    make src  

compile the test program

    make test

Have a look at ioparams.f90 in the repo to get an impression of the generated code.
