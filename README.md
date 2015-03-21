# nml-to-f90

Generate fortran source code from a single namelist

    python nml2f90.py namelist.nml ioparams

This will create ioparams.f90, with corresponding derived types and I/O functions.
To be imported in any other program as follow (without dependencies).

    use ioparams, only: group1_t, group2_t
    use ioparams, only: read_nml, write_nml, parse_command_argument
    use ioparams, only: set_param, get_param

where `<group>_t` are newly defined types created from namelist groups
and the four subroutines are handy I/O and set/get interfaces for the
above defined types.

It could be used as:

    type(group1_t) :: par1
    double precision :: myparam
    integer :: iostat

    ! read namelist
    open(88, file="namelist.nml")
    call read_nml(88, par1)
    close(88)

    ! parse command-line arguments
    i = 1
    do while(i <= command_argument_count())
        call parse_command_argument(par1, i, iostat)
        if (iostat/=0) call parse_command_argument(par2, i)
        i = i + 2
    enddo

    ! access a type in a generic way
    ! equivalent to myparam = par1%myparam
    call get_param(par1, "myparam", myparam)
    call set_param(par1, "myparam", 3.14)


See test.f90 for a complete example of use.

Generate source code and compile the test program in one go:

    make clean src test

    ./test.x  # to try it out

Have a look at ioparams.f90 in the repo to get an impression of the generated code.

## Install (with administrator rights)

python setup.py install

_NOTE_ : This will install nml2f90.py script and the namelist.py module to read 
namelist into python.

## Caveats

- character strings have default length of 256 (to be edited in nml2f90.py)
- certain types may be lost via conversion to python (e.g. all "real" converted to double precision)
- no derived types can be present in the namelist with the % syntax (this would 
  defy the point of this module...)

## Credits

Thanks to Alex Robinson and its nml project https://github.com/alex-robinson/nml
for inspiration and some pieces of code (e.g. parse vector string) test namelist file). 
Have a look at it for an alternative approach to generic parameters I/O.
