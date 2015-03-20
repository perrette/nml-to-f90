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

It could be used as:

    type(group1_t) :: par1
    double precision :: myparam

    open(88, file="namelist.nml")
    call read_nml(88, par1)
    close(88)

    ! access a specific file in a generic way
    ! equivalent to myparam = par1%myparam
    call get_param(par1, "myparam", myparam)

See test.f90 for a complete example of use.

Generate source code and compile the test program in one go:

    make clean src test

    ./test.x  # to try it out

Have a look at ioparams.f90 in the repo to get an impression of the generated code.

#Credits

Thanks to Alex Robinson and its nml project https://github.com/alex-robinson/nml
for inspiration and its test namelist file. Have a look for an alternative approach
to generic parameters I/O.
