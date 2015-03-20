# nml-to-f90
Generate fortran source code from a single namelist

    python nml2f90.py namelist.nml ioparams

This will create ioparams.f90, with corresponding derived types and I/O functions.
To be imported in any other program as follow (without dependencies).

    use ioparams, only: group1_t, group2_t, ...
    use ioparams, only: read_nml, write_nml, get_param, set_param

where `<group>_t` are newly defined types created from namelist groups
and the four subroutines are handy I/O and set/get interfaces for the
above defined types.

See test.f90 for an example of use.

generate source code and compile the test program in one go:

    make clean src test

    ./test.x  # to try it out

Have a look at ioparams.f90 in the repo to get an impression of the generated code.
