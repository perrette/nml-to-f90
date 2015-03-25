# nml-to-f90

Generate fortran source code for handling parameter I/O from a namelist

    nml2f90 namelist.nml --io-nml --command-line

This will create ioparams.f90, with corresponding derived types, 
namelist I/O and command-line subroutines.
To be imported in any other program as follow (you need to compile 
ioparams.f90 of course, e.g. see **compilation** below)

    use ioparams, only: group1_t, group2_t
    use ioparams, only: read_nml, write_nml, parse_command_argument

where `<group>_t` are newly defined types created from namelist groups
and the four subroutines are handy I/O and set/get interfaces for the
above defined types.

In its simplest form, this would be:

    use ioparams

    implicit none

    type(group1_t) :: par1
    integer :: iounit=88, i

    ! read namelist
    open(iounit, file="namelist.nml")
    call read_nml(iounit, par1)
    close(iounit)

    ! parse command-line arguments and stop in case of error
    i = 1
    do while(i <= command_argument_count())
        call parse_command_argument(par1, i)
        i = i+2
    enddo

See example.f90 for an interactive variant of this snippet, and and test.f90 
for a more complete suite of tests for various functions in ioparams.

Generate source code and compile the example program in one go:

    make clean src example

    ./example.x -h  # to try it out

Have a look at ioparams.f90 in the repo to get an impression of the generated code.

Get all options of nml2f90 by typing:

    nml2f90 --help  

## Install (with administrator rights)

Clone this repository, and at the root execute:

python setup.py install

This will install nml2f90 module (along with handly nml2f90.namelist)
and nml2f90 as an alias for python -m nml2f90.nml2f90

## Compilation

After generating the source code by calling nml2f90, just put ioparam.f90 
into your code directory and compile your main program with:

    gfortran -o example.x ioparams.f90 example.f90

That's it !

## Caveats

- character strings have the same length (default to 256)
- certain types may be lost via conversion to python (e.g. all "real" converted to double precision)
- no derived types can be present in the namelist with the % syntax (this would 
  defy the point of this module...)

## Perspectives, ideas for the development of this project

- Try to make the code as un-invasive and flexible as possible, so that it
  only provides additional functionality with as few tradeoffs as possible:

    - parse source code to re-use existing type definitions (as only - argably
      desirable - tradeoff, enforce "dimension(n)" and "::" notation, to
      ease parsing)

    - use existing object (even without source code) to retrieve the types,
      provided their name, making use of the fortran built-innamelist 
      functionality to write a dummy namelist. Requires generating the 
      following small program, parse the dummy namelist so written, and 
      generate types (and possibly namelist) from that.
      Difficulty: needs compiling, requires additional info about the compiler
      used by the user.
     
            program writedummynamelist

            use mod1, only: group1_t  ! from template
            use mod2, only: group2_t  ! from template 

            type(group1_t) :: group1  ! from template
            type(group2_t) :: group2  ! from template

            integer :: iounit = 88

            namelist /dummynamelist/ group1, group2 ! from template

            open(iounit, file="dummy.nml")
            write(iounit, dummynamelist)
            close(iounit)

            end program

## Credits

Thanks to Alex Robinson and its nml project https://github.com/alex-robinson/nml
for inspiration and some pieces of code (e.g. parse vector string) test namelist file). 
Have a look at it for an alternative approach to generic parameters I/O.
