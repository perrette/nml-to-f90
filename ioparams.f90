module ioparams
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Automatically generated module by nml2f90.py [source: namelist.nml]
    !
    ! https://github.com/perrette/nml-to-f90
    ! version: unknown
    ! 
    ! Contains read / write subroutines for all derived types imported below.
    ! As well as setter / getter access by field name
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    implicit none

    private
    public :: group1_t, group2_t
    public :: read_nml, write_nml          ! nml I/O
    public :: set_param, get_param         ! generic set/get
    public :: parse_command_argument       ! parse and assign command-line arg
    public :: print_help                   ! print command-line argument help
    public :: has_param, set_param_string  ! useful fine-grained control on parse_command

    integer, parameter :: dp = kind(0.d0)
    integer, parameter :: clen = 256    ! character length
    logical :: VERBOSE = .true.

    type group1_t
        character(len=clen) :: string1
        character(len=clen), dimension(3) :: stringarr1
        logical :: logical1
        integer :: integer1
        integer :: integer2
        character(len=clen) :: string2
    end type
    type group2_t
        character(len=clen) :: string1
        character(len=clen), dimension(3) :: stringarr1
        logical :: logical1
        integer :: integer1
        integer :: integer2
        character(len=clen) :: string2
        integer, dimension(7) :: intarr1
        real(kind=dp) :: double1
        real(kind=dp), dimension(5) :: dblarr1
        logical, dimension(5) :: logarr1
    end type

    interface read_nml
        module procedure :: read_nml_group1
        module procedure :: read_nml_group2
    end interface

    interface write_nml
        module procedure :: write_nml_group1
        module procedure :: write_nml_group2
    end interface

    interface parse_command_argument
        module procedure :: parse_command_argument_group1
        module procedure :: parse_command_argument_group2
    end interface

    interface print_help
        module procedure :: print_help_group1
        module procedure :: print_help_group2
    end interface

    interface has_param
        module procedure :: has_param_group1
        module procedure :: has_param_group2
    end interface

    interface set_param_string
        module procedure :: set_param_string_group1
        module procedure :: set_param_string_group2
    end interface

    interface set_param
        
    end interface

    interface get_param
        
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

subroutine read_nml_group1 (iounit, params)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Read the group1 block in a namelist file and assign to type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    integer, intent(in) :: iounit
    type(group1_t), intent(inout) :: params

    character(len=clen) :: string1
    character(len=clen), dimension(3) :: stringarr1
    logical :: logical1
    integer :: integer1
    integer :: integer2
    character(len=clen) :: string2

    namelist / group1 / string1, stringarr1, logical1, integer1, integer2, string2

    ! initialize variables
    string1 = params%string1
    stringarr1 = params%stringarr1
    logical1 = params%logical1
    integer1 = params%integer1
    integer2 = params%integer2
    string2 = params%string2

    ! read all
    read(unit=iounit, nml=group1) 

    ! assign back to type
    params%string1 = string1
    params%stringarr1 = stringarr1
    params%logical1 = logical1
    params%integer1 = integer1
    params%integer2 = integer2
    params%string2 = string2
end subroutine

subroutine write_nml_group1 (iounit, params)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Read the group1 block in a namelist file and assign to type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    integer, intent(in) :: iounit
    type(group1_t), intent(inout) :: params

    character(len=clen) :: string1
    character(len=clen), dimension(3) :: stringarr1
    logical :: logical1
    integer :: integer1
    integer :: integer2
    character(len=clen) :: string2

    namelist / group1 / string1, stringarr1, logical1, integer1, integer2, string2

    ! initialize variables
    string1 = params%string1
    stringarr1 = params%stringarr1
    logical1 = params%logical1
    integer1 = params%integer1
    integer2 = params%integer2
    string2 = params%string2

    ! write_all
    write(unit=iounit, nml=group1) 
end subroutine


subroutine read_nml_group2 (iounit, params)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Read the group2 block in a namelist file and assign to type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    integer, intent(in) :: iounit
    type(group2_t), intent(inout) :: params

    character(len=clen) :: string1
    character(len=clen), dimension(3) :: stringarr1
    logical :: logical1
    integer :: integer1
    integer :: integer2
    character(len=clen) :: string2
    integer, dimension(7) :: intarr1
    real(kind=dp) :: double1
    real(kind=dp), dimension(5) :: dblarr1
    logical, dimension(5) :: logarr1

    namelist / group2 / string1, stringarr1, logical1, integer1, integer2, string2, intarr1, &
double1, dblarr1, logarr1

    ! initialize variables
    string1 = params%string1
    stringarr1 = params%stringarr1
    logical1 = params%logical1
    integer1 = params%integer1
    integer2 = params%integer2
    string2 = params%string2
    intarr1 = params%intarr1
    double1 = params%double1
    dblarr1 = params%dblarr1
    logarr1 = params%logarr1

    ! read all
    read(unit=iounit, nml=group2) 

    ! assign back to type
    params%string1 = string1
    params%stringarr1 = stringarr1
    params%logical1 = logical1
    params%integer1 = integer1
    params%integer2 = integer2
    params%string2 = string2
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
    type(group2_t), intent(inout) :: params

    character(len=clen) :: string1
    character(len=clen), dimension(3) :: stringarr1
    logical :: logical1
    integer :: integer1
    integer :: integer2
    character(len=clen) :: string2
    integer, dimension(7) :: intarr1
    real(kind=dp) :: double1
    real(kind=dp), dimension(5) :: dblarr1
    logical, dimension(5) :: logarr1

    namelist / group2 / string1, stringarr1, logical1, integer1, integer2, string2, intarr1, &
double1, dblarr1, logarr1

    ! initialize variables
    string1 = params%string1
    stringarr1 = params%stringarr1
    logical1 = params%logical1
    integer1 = params%integer1
    integer2 = params%integer2
    string2 = params%string2
    intarr1 = params%intarr1
    double1 = params%double1
    dblarr1 = params%dblarr1
    logarr1 = params%logarr1

    ! write_all
    write(unit=iounit, nml=group2) 
end subroutine


    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Routines useful to process command-line parameters: 
    ! - has_param
    ! - set_param_string
    ! - parse_command_argument
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

subroutine parse_command_argument_group1 (params,i, iostat)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Maybe assign ith command line argument to the params type
    ! Input:
    !   params : the paramter type
    !   i : integer, positional argument of the parameter name
    ! Output:
    !   iostat, optional : 0 if success, 1 otherwise (if not
    !       provided, an error is thrown in case of failure)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(group1_t), intent(inout) :: params
    integer, intent(inout) :: i
    integer, optional :: iostat
    character(len=512) :: arg, argv

    call get_command_argument(i, arg)

    ! Print HELP ?
    if (arg == '--help' .or. arg=='-h') then
      call print_help_group1(params)
      return
    endif

    if (has_param_group1(params, trim(arg(3:)))) then
      ! +++++  present 
      call get_command_argument(i+1, argv)
      call set_param_string_group1(params, trim(arg(3:)), trim(argv))
      i = i+1
      if (present(iostat)) then
        iostat = 0
      endif
    else
      ! +++++  no found
      if (present(iostat)) then
        iostat=1
      else
        write(*,*) "ERROR: unknown parameter in group1 : --",trim(arg(3:))
        write(*,*) ""
        write(*,*) "-h or --help for HELP"
        stop
      endif
    endif
end subroutine

subroutine print_help_group1(params, iounit, value)
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Print HELP on a derived type, useful for command-line arg
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  type(group1_t), intent(in) :: params
  integer, optional :: iounit
  integer :: io
  logical, optional :: value
  logical :: def
  if (present(iounit)) then
    io = iounit
  else
    io = 6  ! screen
  endif
  if (present(value)) then
    def =value
  else
    def = .false. ! by default do not show default values
  endif
  write(io, *) " "
  write(io, *) "+++++++++++++++++      group1      ++++++++++++++++++"
  
if (def) then
    write(io, *) "--string1 character(len=clen)  (default: ",params%string1," )"
else
    write(io, *) "--string1 character(len=clen) "
endif

    
if (def) then
    write(io, *) "--stringarr1 character(len=clen), dimension(3)  (default: ",params%stringarr1," )"
else
    write(io, *) "--stringarr1 character(len=clen), dimension(3) "
endif

    
if (def) then
    write(io, *) "--logical1 logical  (default: ",params%logical1," )"
else
    write(io, *) "--logical1 logical "
endif

    
if (def) then
    write(io, *) "--integer1 integer  (default: ",params%integer1," )"
else
    write(io, *) "--integer1 integer "
endif

    
if (def) then
    write(io, *) "--integer2 integer  (default: ",params%integer2," )"
else
    write(io, *) "--integer2 integer "
endif

    
if (def) then
    write(io, *) "--string2 character(len=clen)  (default: ",params%string2," )"
else
    write(io, *) "--string2 character(len=clen) "
endif

end subroutine

subroutine set_param_string_group1 (params, name, string)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group1 type from a string argument
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(group1_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: string
    integer :: iostat

    select case (name) 
    
case ('string1', 'group1%string1')
    read(string, *, iostat=IOSTAT) params%string1
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "group1%string1 = ", params%string1
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --group1%string1"
        else
            write(*,*) "ERROR converting string to character(len=clen): --group1%string1 ",trim(string)
        endif
        stop
    endif

    
case ('stringarr1', 'group1%stringarr1')
    call string_to_array(string, params%stringarr1, iostat=iostat)
    if (iostat /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --group1%stringarr1"
        else
            write(*,*) "ERROR converting string to character(len=clen), dimension(3) array : --group1%stringarr1 ",trim(string)
        endif
        stop
    endif

    
case ('logical1', 'group1%logical1')
    read(string, *, iostat=IOSTAT) params%logical1
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "group1%logical1 = ", params%logical1
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --group1%logical1"
        else
            write(*,*) "ERROR converting string to logical: --group1%logical1 ",trim(string)
        endif
        stop
    endif

    
case ('integer1', 'group1%integer1')
    read(string, *, iostat=IOSTAT) params%integer1
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "group1%integer1 = ", params%integer1
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --group1%integer1"
        else
            write(*,*) "ERROR converting string to integer: --group1%integer1 ",trim(string)
        endif
        stop
    endif

    
case ('integer2', 'group1%integer2')
    read(string, *, iostat=IOSTAT) params%integer2
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "group1%integer2 = ", params%integer2
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --group1%integer2"
        else
            write(*,*) "ERROR converting string to integer: --group1%integer2 ",trim(string)
        endif
        stop
    endif

    
case ('string2', 'group1%string2')
    read(string, *, iostat=IOSTAT) params%string2
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "group1%string2 = ", params%string2
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --group1%string2"
        else
            write(*,*) "ERROR converting string to character(len=clen): --group1%string2 ",trim(string)
        endif
        stop
    endif

    case default
      write(*,*) "ERROR set_param_string for group1: unknown member :: ",trim(name)
      stop
    end select
end subroutine

function has_param_group1 (params, name) result(has_param)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Test whether an attribute name is member of the type group1
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(group1_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    logical :: has_param

    has_param = .true.
    select case (name) 
      
case ('string1', 'group1%string1')

    
case ('stringarr1', 'group1%stringarr1')

    
case ('logical1', 'group1%logical1')

    
case ('integer1', 'group1%integer1')

    
case ('integer2', 'group1%integer2')

    
case ('string2', 'group1%string2')

    case default
      has_param = .false.
    end select
end function



subroutine parse_command_argument_group2 (params,i, iostat)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Maybe assign ith command line argument to the params type
    ! Input:
    !   params : the paramter type
    !   i : integer, positional argument of the parameter name
    ! Output:
    !   iostat, optional : 0 if success, 1 otherwise (if not
    !       provided, an error is thrown in case of failure)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(group2_t), intent(inout) :: params
    integer, intent(inout) :: i
    integer, optional :: iostat
    character(len=512) :: arg, argv

    call get_command_argument(i, arg)

    ! Print HELP ?
    if (arg == '--help' .or. arg=='-h') then
      call print_help_group2(params)
      return
    endif

    if (has_param_group2(params, trim(arg(3:)))) then
      ! +++++  present 
      call get_command_argument(i+1, argv)
      call set_param_string_group2(params, trim(arg(3:)), trim(argv))
      i = i+1
      if (present(iostat)) then
        iostat = 0
      endif
    else
      ! +++++  no found
      if (present(iostat)) then
        iostat=1
      else
        write(*,*) "ERROR: unknown parameter in group2 : --",trim(arg(3:))
        write(*,*) ""
        write(*,*) "-h or --help for HELP"
        stop
      endif
    endif
end subroutine

subroutine print_help_group2(params, iounit, value)
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Print HELP on a derived type, useful for command-line arg
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  type(group2_t), intent(in) :: params
  integer, optional :: iounit
  integer :: io
  logical, optional :: value
  logical :: def
  if (present(iounit)) then
    io = iounit
  else
    io = 6  ! screen
  endif
  if (present(value)) then
    def =value
  else
    def = .false. ! by default do not show default values
  endif
  write(io, *) " "
  write(io, *) "+++++++++++++++++      group2      ++++++++++++++++++"
  
if (def) then
    write(io, *) "--string1 character(len=clen)  (default: ",params%string1," )"
else
    write(io, *) "--string1 character(len=clen) "
endif

    
if (def) then
    write(io, *) "--stringarr1 character(len=clen), dimension(3)  (default: ",params%stringarr1," )"
else
    write(io, *) "--stringarr1 character(len=clen), dimension(3) "
endif

    
if (def) then
    write(io, *) "--logical1 logical  (default: ",params%logical1," )"
else
    write(io, *) "--logical1 logical "
endif

    
if (def) then
    write(io, *) "--integer1 integer  (default: ",params%integer1," )"
else
    write(io, *) "--integer1 integer "
endif

    
if (def) then
    write(io, *) "--integer2 integer  (default: ",params%integer2," )"
else
    write(io, *) "--integer2 integer "
endif

    
if (def) then
    write(io, *) "--string2 character(len=clen)  (default: ",params%string2," )"
else
    write(io, *) "--string2 character(len=clen) "
endif

    
if (def) then
    write(io, *) "--intarr1 integer, dimension(7)  (default: ",params%intarr1," )"
else
    write(io, *) "--intarr1 integer, dimension(7) "
endif

    
if (def) then
    write(io, *) "--double1 real(kind=dp)  (default: ",params%double1," )"
else
    write(io, *) "--double1 real(kind=dp) "
endif

    
if (def) then
    write(io, *) "--dblarr1 real(kind=dp), dimension(5)  (default: ",params%dblarr1," )"
else
    write(io, *) "--dblarr1 real(kind=dp), dimension(5) "
endif

    
if (def) then
    write(io, *) "--logarr1 logical, dimension(5)  (default: ",params%logarr1," )"
else
    write(io, *) "--logarr1 logical, dimension(5) "
endif

end subroutine

subroutine set_param_string_group2 (params, name, string)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group2 type from a string argument
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(group2_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: string
    integer :: iostat

    select case (name) 
    
case ('string1', 'group2%string1')
    read(string, *, iostat=IOSTAT) params%string1
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "group2%string1 = ", params%string1
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --group2%string1"
        else
            write(*,*) "ERROR converting string to character(len=clen): --group2%string1 ",trim(string)
        endif
        stop
    endif

    
case ('stringarr1', 'group2%stringarr1')
    call string_to_array(string, params%stringarr1, iostat=iostat)
    if (iostat /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --group2%stringarr1"
        else
            write(*,*) "ERROR converting string to character(len=clen), dimension(3) array : --group2%stringarr1 ",trim(string)
        endif
        stop
    endif

    
case ('logical1', 'group2%logical1')
    read(string, *, iostat=IOSTAT) params%logical1
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "group2%logical1 = ", params%logical1
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --group2%logical1"
        else
            write(*,*) "ERROR converting string to logical: --group2%logical1 ",trim(string)
        endif
        stop
    endif

    
case ('integer1', 'group2%integer1')
    read(string, *, iostat=IOSTAT) params%integer1
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "group2%integer1 = ", params%integer1
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --group2%integer1"
        else
            write(*,*) "ERROR converting string to integer: --group2%integer1 ",trim(string)
        endif
        stop
    endif

    
case ('integer2', 'group2%integer2')
    read(string, *, iostat=IOSTAT) params%integer2
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "group2%integer2 = ", params%integer2
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --group2%integer2"
        else
            write(*,*) "ERROR converting string to integer: --group2%integer2 ",trim(string)
        endif
        stop
    endif

    
case ('string2', 'group2%string2')
    read(string, *, iostat=IOSTAT) params%string2
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "group2%string2 = ", params%string2
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --group2%string2"
        else
            write(*,*) "ERROR converting string to character(len=clen): --group2%string2 ",trim(string)
        endif
        stop
    endif

    
case ('intarr1', 'group2%intarr1')
    call string_to_array(string, params%intarr1, iostat=iostat)
    if (iostat /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --group2%intarr1"
        else
            write(*,*) "ERROR converting string to integer, dimension(7) array : --group2%intarr1 ",trim(string)
        endif
        stop
    endif

    
case ('double1', 'group2%double1')
    read(string, *, iostat=IOSTAT) params%double1
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "group2%double1 = ", params%double1
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --group2%double1"
        else
            write(*,*) "ERROR converting string to real(kind=dp): --group2%double1 ",trim(string)
        endif
        stop
    endif

    
case ('dblarr1', 'group2%dblarr1')
    call string_to_array(string, params%dblarr1, iostat=iostat)
    if (iostat /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --group2%dblarr1"
        else
            write(*,*) "ERROR converting string to real(kind=dp), dimension(5) array : --group2%dblarr1 ",trim(string)
        endif
        stop
    endif

    
case ('logarr1', 'group2%logarr1')
    call string_to_array(string, params%logarr1, iostat=iostat)
    if (iostat /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --group2%logarr1"
        else
            write(*,*) "ERROR converting string to logical, dimension(5) array : --group2%logarr1 ",trim(string)
        endif
        stop
    endif

    case default
      write(*,*) "ERROR set_param_string for group2: unknown member :: ",trim(name)
      stop
    end select
end subroutine

function has_param_group2 (params, name) result(has_param)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Test whether an attribute name is member of the type group2
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(group2_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    logical :: has_param

    has_param = .true.
    select case (name) 
      
case ('string1', 'group2%string1')

    
case ('stringarr1', 'group2%stringarr1')

    
case ('logical1', 'group2%logical1')

    
case ('integer1', 'group2%integer1')

    
case ('integer2', 'group2%integer2')

    
case ('string2', 'group2%string2')

    
case ('intarr1', 'group2%intarr1')

    
case ('double1', 'group2%double1')

    
case ('dblarr1', 'group2%dblarr1')

    
case ('logarr1', 'group2%logarr1')

    case default
      has_param = .false.
    end select
end function



    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! SET / GET routines
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



    ! Type conversion

    ! =============================================================
    !
    ! Type conversion functions (Courtesy of Alex Robinson's nml module)
    ! ==> useful to read array (lists) from command list argument)
    !
    ! =============================================================

subroutine string_to_array_integer (string, value, iostat)

    implicit none 

    character(len=*), intent(IN) :: string 
    integer :: value(:)
    character(len=256) :: tmpvec(size(value))
    character(len=256) :: tmpstr, fmt 
    integer, optional :: iostat
    integer :: stat, n, q, q1, q2, j 

    tmpstr = trim(adjustl(string))
    n      = len_trim(tmpstr)+2

    tmpvec(:) = "" 

    q1 = 1 
    do q = 1, size(tmpvec)
        q2 = index(tmpstr(q1:n)," ") + q1
        if (q2 .gt. q1 .and. q2 .le. n) then 
            tmpvec(q) = tmpstr(q1:q2-1)
            q1 = q2

            ! Make sure gaps of more than one space are properly handled
            do j = 1, 1000
                if (tmpstr(q1:q1) == " ") q1 = q1+1
                if (q1 .ge. n) exit 
            end do 

            ! Remove quotes around string if they exist 
            call remove_quotes_comma(tmpvec(q))

            read(tmpvec(q), *, iostat=iostat) value(q)
        
        end if 
    end do 
end subroutine

subroutine string_to_array_double (string, value, iostat)

    implicit none 

    character(len=*), intent(IN) :: string 
    real(dp) :: value(:)
    character(len=256) :: tmpvec(size(value))
    character(len=256) :: tmpstr, fmt 
    integer, optional :: iostat
    integer :: stat, n, q, q1, q2, j 

    tmpstr = trim(adjustl(string))
    n      = len_trim(tmpstr)+2

    tmpvec(:) = "" 

    q1 = 1 
    do q = 1, size(tmpvec)
        q2 = index(tmpstr(q1:n)," ") + q1
        if (q2 .gt. q1 .and. q2 .le. n) then 
            tmpvec(q) = tmpstr(q1:q2-1)
            q1 = q2

            ! Make sure gaps of more than one space are properly handled
            do j = 1, 1000
                if (tmpstr(q1:q1) == " ") q1 = q1+1
                if (q1 .ge. n) exit 
            end do 

            ! Remove quotes around string if they exist 
            call remove_quotes_comma(tmpvec(q))

            read(tmpvec(q), *, iostat=iostat) value(q)
        
        end if 
    end do 
end subroutine

subroutine string_to_array_logical (string, value, iostat)

    implicit none 

    character(len=*), intent(IN) :: string 
    logical :: value(:)
    character(len=256) :: tmpvec(size(value))
    character(len=256) :: tmpstr, fmt 
    integer, optional :: iostat
    integer :: stat, n, q, q1, q2, j 

    tmpstr = trim(adjustl(string))
    n      = len_trim(tmpstr)+2

    tmpvec(:) = "" 

    q1 = 1 
    do q = 1, size(tmpvec)
        q2 = index(tmpstr(q1:n)," ") + q1
        if (q2 .gt. q1 .and. q2 .le. n) then 
            tmpvec(q) = tmpstr(q1:q2-1)
            q1 = q2

            ! Make sure gaps of more than one space are properly handled
            do j = 1, 1000
                if (tmpstr(q1:q1) == " ") q1 = q1+1
                if (q1 .ge. n) exit 
            end do 

            ! Remove quotes around string if they exist 
            call remove_quotes_comma(tmpvec(q))

            read(tmpvec(q), *, iostat=iostat) value(q)
        
        end if 
    end do 
end subroutine
subroutine string_to_array_string (string, value, iostat)

    implicit none 

    character(len=*), intent(IN) :: string 
    character(len=*) :: value(:)
    character(len=256) :: tmpvec(size(value))
    character(len=256) :: tmpstr, fmt 
    integer, optional :: iostat
    integer :: stat, n, q, q1, q2, j 

    tmpstr = trim(adjustl(string))
    n      = len_trim(tmpstr)+2

    tmpvec(:) = "" 

    q1 = 1 
    do q = 1, size(tmpvec)
        q2 = index(tmpstr(q1:n)," ") + q1
        if (q2 .gt. q1 .and. q2 .le. n) then 
            tmpvec(q) = tmpstr(q1:q2-1)
            q1 = q2

            ! Make sure gaps of more than one space are properly handled
            do j = 1, 1000
                if (tmpstr(q1:q1) == " ") q1 = q1+1
                if (q1 .ge. n) exit 
            end do 

            ! Remove quotes around string if they exist 
            call remove_quotes_comma(tmpvec(q))

            read(tmpvec(q), *, iostat=iostat) value(q)
        
        end if 
    end do 
end subroutine

subroutine remove_quotes_comma(string)

    implicit none 
    character(len=*), intent(INOUT) :: string 
    integer :: i, n 

    ! Eliminate quotes
    n = len_trim(string)
    do i = 1,n 
        if (string(i:i) == '"' .or. string(i:i) == "'") string(i:i) = " "
    end do 
    string = trim(adjustl(string))

    ! Remove final comma too
    n = len_trim(string)
    if (n > 0) then 
        if (string(n:n) == ",") string(n:n) = " "
        string = trim(adjustl(string))
    end if 
    
    return 

end subroutine remove_quotes_comma




end module ioparams


