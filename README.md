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
    integer :: iounit=88, iostat

    ! read namelist
    open(iounit, file="namelist.nml")
    call read_nml(iounit, par1)
    close(iounit)

    ! parse command-line arguments and stop in case of error
    call parse_command_argument(par1, iostat)
    if (iostat == -2) then
        stop  ! help
    elseif (iostat == -1) then
        stop
    elseif (iostat > 0) then
        write(*,*) "Unknown parameter(s)"
        stop
    endif


__NOTE__: the script interface will be simplified soon (see [issue #1](https://github.com/perrette/nml-to-f90/issues/1))

See example.f90 for an interactive variant of this snippet, and and test.f90 
for a more complete suite of tests for various functions in ioparams.

Generate source code and compile the example program in one go:

    make clean src example

    ./example.x -h  # to try it out

Have a look at ioparams.f90 in the repo to get an impression of the generated code.

Get all options of nml2f90 by typing:

    nml2f90 --help  

## Parse fortran source code for more precise results

They are a couple of caveats with the basic namelist to fortran conversion:

- character strings all have the same length (default to 256)
- certain types may be lost via conversion to python (e.g. all "real" converted to double precision)
  or simply more error-prone procedure (say a '.' is missing, but you did mean a float)

So I found for myself more re-insuring to have the variables defined in my regular 
fortran source code, and tell nml2f90 to just look into the source code (parse it) 
to retrieve the actual type, via the `--src` argument:

    nml2f90 namelist.nml --src src/*f90 --io-nml
    
Note the types found in the source code will simply be imported `use <mod>, only:<type>`
and not redefined in ioparams.f90. You will probably need to provide
more information concerning the naming conventions for mapping between
group (namelist blocks) and type names. See options `--type-prefix`, `--type-suffix`
and `--type-map` (`nml2f90 -h` for help).

You may also want to create interfaces for other types not defined 
in the namelist. Use `--include-groups` for that.

## Use nml.f90 to make the code more re-usable and more robust

The --io-nml option creates type-specific interfaces to read from and
write to namelist format using the built-in namelist capability.

A caveat is that it requires determining the type of the various 
variables when creating the routines (have a look at the generated 
ioparams.f90), which may be error prone (see parsing of source code 
for a solution) and generates much boiler-plate code. The second
reason is of greater concern IMO, because it makes the generated
code harder to understand (not too big of an issue as long as 
nml2f90 is around, or as long as there is no bug to be tracked down!).

An elegant solution is offered by Alex Robinson's [nml.f90](https://github.com/alex-robinson/nml)
library, which parses (read-only) the namelist and allows calls like

    call nml_read(filename,"group1","name1",group1%name1)

Passing the option `--io-nml-nml` will make nml2f90 use that
kind of calls internally instead of fortran build-in `namelist` solution.
It assumes nml.f90 is present on the compilation path. That way
nml2f90 becomes no more than a nice helper to avoid writing repetitive code, 
but it involves no commitment in the future...(as long as you keep nml.f90 
around of course !)
And the command-line argument parsing for whole type of course, which 
remains a useful feature.  This feature is still a work-in-progress.

Note this concerns ioparams.f90's internals and does not change the way you use 
ioparams' `read_nml`, `parse_command_argument`, `print_help`. For now, 
the `write_nml` is not available with this option.

If`--io-nml-nml` is passed, there is no need to indicate `--io-nml`.

## Summary of advices for sustainable using

The safer way of using nml2f90 is probably:

    nml2f90 namelist.nml --src src/*f90 --io-nml-nml --command-line

and make sure [nml.f90](https://github.com/alex-robinson/nml) is compiled before ioparams.f90

## Caveats

- each namelist block must be defined in the same fortran type, i.e. one type per block.
- as a corrolary, no derived types nor portions of array can be present in the 
  namelist with the % syntax 

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

## Credits

Thanks to Alex Robinson and its nml project https://github.com/alex-robinson/nml
for inspiration and some pieces of code (e.g. parse vector string) test namelist file). 
Have a look at it for an alternative approach to generic parameters I/O.
