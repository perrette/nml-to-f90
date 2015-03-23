module {io_module_name}
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Automatically generated module by nml2f90.py [source: {source}]
    !
    ! https://github.com/perrette/nml-to-f90
    ! version: {version}
    ! 
    ! Contains read / write subroutines for all derived types imported below.
    ! As well as setter / getter access by field name
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    implicit none

    private
    public :: {list_of_types}
    public :: read_nml, write_nml          ! nml I/O
    public :: set_param, get_param         ! generic set/get
    public :: parse_command_argument       ! parse and assign command-line arg
    public :: print_help                   ! print command-line argument help
    public :: has_param, set_param_string  ! useful fine-grained control on parse_command

    integer, parameter :: dp = kind(0.d0)
    integer, parameter :: clen = {clen}    ! default character length
    logical :: VERBOSE = {verbose}

    {type_definitions}

    interface read_nml
        {read_nml_proc}
    end interface

    interface write_nml
        {write_nml_proc}
    end interface

    interface parse_command_argument
        {parse_command_argument_proc}
    end interface

    interface print_help
        {print_help_proc}
    end interface

    interface has_param
        {has_param_proc}
    end interface

    interface set_param_string
        {set_param_string_proc}
    end interface

    interface set_param
        {set_param_proc}
    end interface

    interface get_param
        {get_param_proc}
    end interface

    interface string_to_array
      module procedure :: string_to_array_integer
      module procedure :: string_to_array_double
      module procedure :: string_to_array_string
      module procedure :: string_to_array_logical
    end interface

contains

    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! IO routines
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

{io_routines}

    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Routines useful to process command-line parameters: 
    ! - has_param
    ! - set_param_string
    ! - parse_command_argument
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

{cmd_routines}

    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! SET / GET routines
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

{setget_routines}

    ! Type conversion

{type_conversion}

end module {io_module_name}


