module {io_module_name}
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Automatically generated module
    ! 
    ! Contains read / write subroutines for all derived types imported below.
    ! As well as setter / getter access by field name
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    implicit none

    private
    public :: read_nml, write_nml, set_param, get_param
    public :: {list_of_types}

    integer, parameter :: dp = kind(0.d0)

    {type_definitions}

    interface read_nml
        {read_nml_proc}
    end interface

    interface write_nml
        {write_nml_proc}
    end interface

    interface set_param
        {set_param_proc}
    end interface

    interface get_param
        {get_param_proc}
    end interface

contains

    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! IO routines
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    {io_routines}

    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! SET / GET routines
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    {setget_routines}

end module {io_module_name}
