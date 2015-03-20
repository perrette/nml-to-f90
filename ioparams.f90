
module ioparams
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Automatically generated module
    ! 
    ! Contains read / write subroutines for all derived types imported below.
    ! As well as setter / getter access by field name
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    use params, only: pars_group1, pars_group2

    implicit none

    private
    public :: read_nml, write_nml, set_param, get_param

    integer, parameter :: dp = kind(0.d0)

    interface read_nml
        module procedure :: read_nml_group1
        module procedure :: read_nml_group2
    end interface

    interface write_nml
        module procedure :: write_nml_group1
        module procedure :: write_nml_group2
    end interface

    interface set_param
        module procedure :: set_param_group1_integer
        module procedure :: set_param_group1_char
        module procedure :: set_param_group1_char_arr
        module procedure :: set_param_group1_logical
        module procedure :: set_param_group2_double_arr
        module procedure :: set_param_group2_char_arr
        module procedure :: set_param_group2_logical
        module procedure :: set_param_group2_char
        module procedure :: set_param_group2_logical_arr
        module procedure :: set_param_group2_double
        module procedure :: set_param_group2_integer
        module procedure :: set_param_group2_integer_arr
    end interface

    interface get_param
        module procedure :: get_param_group1_integer
        module procedure :: get_param_group1_char
        module procedure :: get_param_group1_char_arr
        module procedure :: get_param_group1_logical
        module procedure :: get_param_group2_double_arr
        module procedure :: get_param_group2_char_arr
        module procedure :: get_param_group2_logical
        module procedure :: get_param_group2_char
        module procedure :: get_param_group2_logical_arr
        module procedure :: get_param_group2_double
        module procedure :: get_param_group2_integer
        module procedure :: get_param_group2_integer_arr
    end interface

contains

    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! IO routines
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    
subroutine read_nml_group1 (iounit, params)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Read the group1 block in a namelist file and assign to type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    integer, intent(in) :: iounit
    type(pars_group1), intent(inout) :: params

    character(len=256) :: string1
    character(len=256) :: string2
    character(len=256), dimension(3) :: stringarr1
    logical :: logical1
    integer :: integer1
    integer :: integer2

    namelist / group1 / string1, string2, stringarr1, logical1, integer1, integer2

    ! initialize variables
    string1 = params%string1
    string2 = params%string2
    stringarr1 = params%stringarr1
    logical1 = params%logical1
    integer1 = params%integer1
    integer2 = params%integer2

    ! read all
    read(unit=iounit, nml=group1) 

    ! assign back to type
    params%string1 = string1
    params%string2 = string2
    params%stringarr1 = stringarr1
    params%logical1 = logical1
    params%integer1 = integer1
    params%integer2 = integer2
end subroutine



subroutine write_nml_group1 (iounit, params)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Read the group1 block in a namelist file and assign to type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    integer, intent(in) :: iounit
    type(pars_group1), intent(inout) :: params

    character(len=256) :: string1
    character(len=256) :: string2
    character(len=256), dimension(3) :: stringarr1
    logical :: logical1
    integer :: integer1
    integer :: integer2

    namelist / group1 / string1, string2, stringarr1, logical1, integer1, integer2

    ! initialize variables
    string1 = params%string1
    string2 = params%string2
    stringarr1 = params%stringarr1
    logical1 = params%logical1
    integer1 = params%integer1
    integer2 = params%integer2

    ! write_all
    write(unit=iounit, nml=group1) 
end subroutine



subroutine read_nml_group2 (iounit, params)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Read the group2 block in a namelist file and assign to type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    integer, intent(in) :: iounit
    type(pars_group2), intent(inout) :: params

    character(len=256) :: string1
    character(len=256) :: string2
    character(len=256), dimension(3) :: stringarr1
    logical :: logical1
    integer :: integer1
    integer :: integer2
    integer, dimension(10) :: intarr1
    real(dp) :: double1
    real(dp), dimension(6) :: dblarr1
    logical, dimension(5) :: logarr1

    namelist / group2 / string1, string2, stringarr1, logical1, integer1, integer2, intarr1, double1, dblarr1, logarr1

    ! initialize variables
    string1 = params%string1
    string2 = params%string2
    stringarr1 = params%stringarr1
    logical1 = params%logical1
    integer1 = params%integer1
    integer2 = params%integer2
    intarr1 = params%intarr1
    double1 = params%double1
    dblarr1 = params%dblarr1
    logarr1 = params%logarr1

    ! read all
    read(unit=iounit, nml=group2) 

    ! assign back to type
    params%string1 = string1
    params%string2 = string2
    params%stringarr1 = stringarr1
    params%logical1 = logical1
    params%integer1 = integer1
    params%integer2 = integer2
    params%intarr1 = intarr1
    params%double1 = double1
    params%dblarr1 = dblarr1
    params%logarr1 = logarr1
end subroutine



subroutine write_nml_group2 (iounit, params)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Read the group2 block in a namelist file and assign to type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    integer, intent(in) :: iounit
    type(pars_group2), intent(inout) :: params

    character(len=256) :: string1
    character(len=256) :: string2
    character(len=256), dimension(3) :: stringarr1
    logical :: logical1
    integer :: integer1
    integer :: integer2
    integer, dimension(10) :: intarr1
    real(dp) :: double1
    real(dp), dimension(6) :: dblarr1
    logical, dimension(5) :: logarr1

    namelist / group2 / string1, string2, stringarr1, logical1, integer1, integer2, intarr1, double1, dblarr1, logarr1

    ! initialize variables
    string1 = params%string1
    string2 = params%string2
    stringarr1 = params%stringarr1
    logical1 = params%logical1
    integer1 = params%integer1
    integer2 = params%integer2
    intarr1 = params%intarr1
    double1 = params%double1
    dblarr1 = params%dblarr1
    logarr1 = params%logarr1

    ! write_all
    write(unit=iounit, nml=group2) 
end subroutine


    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! SET / GET routines
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    
subroutine get_param_group1_integer (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group1 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(pars_group1), intent(inout) :: params
    character(len=*), intent(in) :: name
    integer, intent(out) :: value

    select case (name) 
        
case ('integer1')
    value = params%integer1


case ('integer2')
    value = params%integer2

        case default
            write(*,*) "ERROR get_param for group1: unknown type member integer :: ",trim(name)
            stop
    end select
end subroutine



subroutine set_param_group1_integer (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group1 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(pars_group1), intent(inout) :: params
    character(len=*), intent(in) :: name
    integer, intent(in) :: value

    select case (name) 
        
case ('integer1')
    params%integer1 = value


case ('integer2')
    params%integer2 = value

        case default
        write(*,*) "ERROR set_param for group1: unknown type member: integer :: ",trim(name)
            stop
    end select
end subroutine



subroutine get_param_group1_char (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group1 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(pars_group1), intent(inout) :: params
    character(len=*), intent(in) :: name
    character(len=*), intent(out) :: value

    select case (name) 
        
case ('string1')
    value = params%string1


case ('string2')
    value = params%string2

        case default
            write(*,*) "ERROR get_param for group1: unknown type member character(len=*) :: ",trim(name)
            stop
    end select
end subroutine



subroutine set_param_group1_char (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group1 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(pars_group1), intent(inout) :: params
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: value

    select case (name) 
        
case ('string1')
    params%string1 = value


case ('string2')
    params%string2 = value

        case default
        write(*,*) "ERROR set_param for group1: unknown type member: character(len=*) :: ",trim(name)
            stop
    end select
end subroutine



subroutine get_param_group1_char_arr (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group1 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(pars_group1), intent(inout) :: params
    character(len=*), intent(in) :: name
    character(len=256), dimension(3), intent(out) :: value

    select case (name) 
        
case ('stringarr1')
    value = params%stringarr1

        case default
            write(*,*) "ERROR get_param for group1: unknown type member character(len=256), dimension(3) :: ",trim(name)
            stop
    end select
end subroutine



subroutine set_param_group1_char_arr (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group1 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(pars_group1), intent(inout) :: params
    character(len=*), intent(in) :: name
    character(len=256), dimension(3), intent(in) :: value

    select case (name) 
        
case ('stringarr1')
    params%stringarr1 = value

        case default
        write(*,*) "ERROR set_param for group1: unknown type member: character(len=256), dimension(3) :: ",trim(name)
            stop
    end select
end subroutine



subroutine get_param_group1_logical (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group1 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(pars_group1), intent(inout) :: params
    character(len=*), intent(in) :: name
    logical, intent(out) :: value

    select case (name) 
        
case ('logical1')
    value = params%logical1

        case default
            write(*,*) "ERROR get_param for group1: unknown type member logical :: ",trim(name)
            stop
    end select
end subroutine



subroutine set_param_group1_logical (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group1 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(pars_group1), intent(inout) :: params
    character(len=*), intent(in) :: name
    logical, intent(in) :: value

    select case (name) 
        
case ('logical1')
    params%logical1 = value

        case default
        write(*,*) "ERROR set_param for group1: unknown type member: logical :: ",trim(name)
            stop
    end select
end subroutine



subroutine get_param_group2_double_arr (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group2 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(pars_group2), intent(inout) :: params
    character(len=*), intent(in) :: name
    real(dp), dimension(6), intent(out) :: value

    select case (name) 
        
case ('dblarr1')
    value = params%dblarr1

        case default
            write(*,*) "ERROR get_param for group2: unknown type member real(dp), dimension(6) :: ",trim(name)
            stop
    end select
end subroutine



subroutine set_param_group2_double_arr (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group2 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(pars_group2), intent(inout) :: params
    character(len=*), intent(in) :: name
    real(dp), dimension(6), intent(in) :: value

    select case (name) 
        
case ('dblarr1')
    params%dblarr1 = value

        case default
        write(*,*) "ERROR set_param for group2: unknown type member: real(dp), dimension(6) :: ",trim(name)
            stop
    end select
end subroutine



subroutine get_param_group2_char_arr (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group2 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(pars_group2), intent(inout) :: params
    character(len=*), intent(in) :: name
    character(len=256), dimension(3), intent(out) :: value

    select case (name) 
        
case ('stringarr1')
    value = params%stringarr1

        case default
            write(*,*) "ERROR get_param for group2: unknown type member character(len=256), dimension(3) :: ",trim(name)
            stop
    end select
end subroutine



subroutine set_param_group2_char_arr (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group2 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(pars_group2), intent(inout) :: params
    character(len=*), intent(in) :: name
    character(len=256), dimension(3), intent(in) :: value

    select case (name) 
        
case ('stringarr1')
    params%stringarr1 = value

        case default
        write(*,*) "ERROR set_param for group2: unknown type member: character(len=256), dimension(3) :: ",trim(name)
            stop
    end select
end subroutine



subroutine get_param_group2_logical (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group2 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(pars_group2), intent(inout) :: params
    character(len=*), intent(in) :: name
    logical, intent(out) :: value

    select case (name) 
        
case ('logical1')
    value = params%logical1

        case default
            write(*,*) "ERROR get_param for group2: unknown type member logical :: ",trim(name)
            stop
    end select
end subroutine



subroutine set_param_group2_logical (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group2 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(pars_group2), intent(inout) :: params
    character(len=*), intent(in) :: name
    logical, intent(in) :: value

    select case (name) 
        
case ('logical1')
    params%logical1 = value

        case default
        write(*,*) "ERROR set_param for group2: unknown type member: logical :: ",trim(name)
            stop
    end select
end subroutine



subroutine get_param_group2_char (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group2 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(pars_group2), intent(inout) :: params
    character(len=*), intent(in) :: name
    character(len=*), intent(out) :: value

    select case (name) 
        
case ('string1')
    value = params%string1


case ('string2')
    value = params%string2

        case default
            write(*,*) "ERROR get_param for group2: unknown type member character(len=*) :: ",trim(name)
            stop
    end select
end subroutine



subroutine set_param_group2_char (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group2 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(pars_group2), intent(inout) :: params
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: value

    select case (name) 
        
case ('string1')
    params%string1 = value


case ('string2')
    params%string2 = value

        case default
        write(*,*) "ERROR set_param for group2: unknown type member: character(len=*) :: ",trim(name)
            stop
    end select
end subroutine



subroutine get_param_group2_logical_arr (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group2 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(pars_group2), intent(inout) :: params
    character(len=*), intent(in) :: name
    logical, dimension(5), intent(out) :: value

    select case (name) 
        
case ('logarr1')
    value = params%logarr1

        case default
            write(*,*) "ERROR get_param for group2: unknown type member logical, dimension(5) :: ",trim(name)
            stop
    end select
end subroutine



subroutine set_param_group2_logical_arr (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group2 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(pars_group2), intent(inout) :: params
    character(len=*), intent(in) :: name
    logical, dimension(5), intent(in) :: value

    select case (name) 
        
case ('logarr1')
    params%logarr1 = value

        case default
        write(*,*) "ERROR set_param for group2: unknown type member: logical, dimension(5) :: ",trim(name)
            stop
    end select
end subroutine



subroutine get_param_group2_double (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group2 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(pars_group2), intent(inout) :: params
    character(len=*), intent(in) :: name
    real(dp), intent(out) :: value

    select case (name) 
        
case ('double1')
    value = params%double1

        case default
            write(*,*) "ERROR get_param for group2: unknown type member real(dp) :: ",trim(name)
            stop
    end select
end subroutine



subroutine set_param_group2_double (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group2 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(pars_group2), intent(inout) :: params
    character(len=*), intent(in) :: name
    real(dp), intent(in) :: value

    select case (name) 
        
case ('double1')
    params%double1 = value

        case default
        write(*,*) "ERROR set_param for group2: unknown type member: real(dp) :: ",trim(name)
            stop
    end select
end subroutine



subroutine get_param_group2_integer (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group2 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(pars_group2), intent(inout) :: params
    character(len=*), intent(in) :: name
    integer, intent(out) :: value

    select case (name) 
        
case ('integer1')
    value = params%integer1


case ('integer2')
    value = params%integer2

        case default
            write(*,*) "ERROR get_param for group2: unknown type member integer :: ",trim(name)
            stop
    end select
end subroutine



subroutine set_param_group2_integer (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group2 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(pars_group2), intent(inout) :: params
    character(len=*), intent(in) :: name
    integer, intent(in) :: value

    select case (name) 
        
case ('integer1')
    params%integer1 = value


case ('integer2')
    params%integer2 = value

        case default
        write(*,*) "ERROR set_param for group2: unknown type member: integer :: ",trim(name)
            stop
    end select
end subroutine



subroutine get_param_group2_integer_arr (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group2 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(pars_group2), intent(inout) :: params
    character(len=*), intent(in) :: name
    integer, dimension(10), intent(out) :: value

    select case (name) 
        
case ('intarr1')
    value = params%intarr1

        case default
            write(*,*) "ERROR get_param for group2: unknown type member integer, dimension(10) :: ",trim(name)
            stop
    end select
end subroutine



subroutine set_param_group2_integer_arr (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group2 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(pars_group2), intent(inout) :: params
    character(len=*), intent(in) :: name
    integer, dimension(10), intent(in) :: value

    select case (name) 
        
case ('intarr1')
    params%intarr1 = value

        case default
        write(*,*) "ERROR set_param for group2: unknown type member: integer, dimension(10) :: ",trim(name)
            stop
    end select
end subroutine


end module ioparams
