! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Automatically generated module by nml2f90
! Create Date : 2015-03-26 22:13:23.697472
! History: /home/perrette/github/nml-to-f90/nml2f90/nml2f90.py
! namelist.nml ioparams --io-nml --command-line --set-get-param -v
!
! https://github.com/perrette/nml-to-f90
! version: 0.0.0.dev-91fcb46
!
! Features included : io_nml, command_line, set_get_param
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

module type_conversion
! =============================================================
!
! Type conversion functions (Courtesy of Alex Robinson's nml module)
! ==> useful to read array (lists) from command list argument)
!
! =============================================================

  integer, parameter :: dp = kind(0.d0)

    interface string_to_array
      module procedure :: string_to_array_integer
      module procedure :: string_to_array_double
      module procedure :: string_to_array_string
      module procedure :: string_to_array_logical
    end interface

  contains


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
        if (string(i:i) == '"' .or. string(i:i) == "'") string(i:i) = & 
" "
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

end module



module ioparams


  use type_conversion, dp_conflict => dp


  implicit none

  private
  public :: group1_t
  public :: group2_t
  public :: control_t
  public :: read_nml
  public :: write_nml
  public :: parse_command_argument
  public :: print_help
  public :: set_param_string
  public :: has_param
  public :: set_param
  public :: get_param


  integer, parameter :: dp = 8
  integer, parameter :: ip = 4
  integer, parameter :: clen = 256
  logical :: VERBOSE = .true.

  type :: group1_t
    character(len=clen) :: string1
    character(len=clen), dimension(3) :: stringarr1
    logical :: logical1
    integer(kind=ip) :: integer1 ! Comment about integer1
    integer(kind=ip) :: integer2 ! Another comment for integer2
    character(len=clen) :: string2
  end type

  type :: group2_t
    character(len=clen) :: string1
    character(len=clen), dimension(3) :: stringarr1
    logical :: logical1
    integer(kind=ip) :: integer1 ! Comment about integer1
    integer(kind=ip) :: integer2 ! Another comment for integer2
    character(len=clen) :: string2 ! nasty " sign in comment
    integer(kind=ip), dimension(7) :: intarr1
    real(kind=dp) :: double1
    real(kind=dp), dimension(5) :: dblarr1
    logical, dimension(5) :: logarr1
  end type

  type :: control_t
    logical :: print_nml ! print read namelist to string?
  end type

  interface read_nml
    module procedure :: read_nml_group1
    module procedure :: read_nml_group2
    module procedure :: read_nml_control
  end interface

  interface write_nml
    module procedure :: write_nml_group1
    module procedure :: write_nml_group2
    module procedure :: write_nml_control
  end interface

  interface parse_command_argument
    module procedure :: parse_command_argument_group1
    module procedure :: parse_command_argument_group2
    module procedure :: parse_command_argument_control
  end interface

  interface print_help
    module procedure :: print_help_group1
    module procedure :: print_help_group2
    module procedure :: print_help_control
  end interface

  interface set_param_string
    module procedure :: set_param_string_group1
    module procedure :: set_param_string_group2
    module procedure :: set_param_string_control
  end interface

  interface has_param
    module procedure :: has_param_group1
    module procedure :: has_param_group2
    module procedure :: has_param_control
  end interface

  interface set_param
    module procedure :: set_param_group1_character
    module procedure :: set_param_group1_character_arr
    module procedure :: set_param_group1_integer
    module procedure :: set_param_group1_logical
    module procedure :: set_param_group2_character
    module procedure :: set_param_group2_character_arr
    module procedure :: set_param_group2_integer
    module procedure :: set_param_group2_integer_arr
    module procedure :: set_param_group2_logical
    module procedure :: set_param_group2_logical_arr
    module procedure :: set_param_group2_real
    module procedure :: set_param_group2_real_arr
    module procedure :: set_param_control_logical
  end interface

  interface get_param
    module procedure :: get_param_group1_character
    module procedure :: get_param_group1_character_arr
    module procedure :: get_param_group1_integer
    module procedure :: get_param_group1_logical
    module procedure :: get_param_group2_character
    module procedure :: get_param_group2_character_arr
    module procedure :: get_param_group2_integer
    module procedure :: get_param_group2_integer_arr
    module procedure :: get_param_group2_logical
    module procedure :: get_param_group2_logical_arr
    module procedure :: get_param_group2_real
    module procedure :: get_param_group2_real_arr
    module procedure :: get_param_control_logical
  end interface



contains

  subroutine read_nml_group1 (iounit, params)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Read the group1 group in a namelist file and assign to type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    integer, intent(in) :: iounit
    type(group1_t), intent(inout) :: params

    character(len=clen) :: string1
    character(len=clen), dimension(3) :: stringarr1
    logical :: logical1
    integer(kind=ip) :: integer1
    integer(kind=ip) :: integer2
    character(len=clen) :: string2

    namelist / group1 / string1, stringarr1, logical1, integer1, & 
integer2, string2

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
    ! Read the group1 group in a namelist file and assign to type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    integer, intent(in) :: iounit
    type(group1_t), intent(inout) :: params

    character(len=clen) :: string1
    character(len=clen), dimension(3) :: stringarr1
    logical :: logical1
    integer(kind=ip) :: integer1
    integer(kind=ip) :: integer2
    character(len=clen) :: string2

    namelist / group1 / string1, stringarr1, logical1, integer1, & 
integer2, string2

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
    ! Read the group2 group in a namelist file and assign to type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    integer, intent(in) :: iounit
    type(group2_t), intent(inout) :: params

    character(len=clen) :: string1
    character(len=clen), dimension(3) :: stringarr1
    logical :: logical1
    integer(kind=ip) :: integer1
    integer(kind=ip) :: integer2
    character(len=clen) :: string2
    integer(kind=ip), dimension(7) :: intarr1
    real(kind=dp) :: double1
    real(kind=dp), dimension(5) :: dblarr1
    logical, dimension(5) :: logarr1

    namelist / group2 / string1, stringarr1, logical1, integer1, & 
integer2, string2, intarr1, double1, dblarr1, logarr1

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
    ! Read the group2 group in a namelist file and assign to type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    integer, intent(in) :: iounit
    type(group2_t), intent(inout) :: params

    character(len=clen) :: string1
    character(len=clen), dimension(3) :: stringarr1
    logical :: logical1
    integer(kind=ip) :: integer1
    integer(kind=ip) :: integer2
    character(len=clen) :: string2
    integer(kind=ip), dimension(7) :: intarr1
    real(kind=dp) :: double1
    real(kind=dp), dimension(5) :: dblarr1
    logical, dimension(5) :: logarr1

    namelist / group2 / string1, stringarr1, logical1, integer1, & 
integer2, string2, intarr1, double1, dblarr1, logarr1

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

subroutine read_nml_control (iounit, params)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Read the control group in a namelist file and assign to type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    integer, intent(in) :: iounit
    type(control_t), intent(inout) :: params

    logical :: print_nml

    namelist / control / print_nml

    ! initialize variables
    print_nml = params%print_nml

    ! read all
    read(unit=iounit, nml=control)

    ! assign back to type
    params%print_nml = print_nml
end subroutine

subroutine write_nml_control (iounit, params)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Read the control group in a namelist file and assign to type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    integer, intent(in) :: iounit
    type(control_t), intent(inout) :: params

    logical :: print_nml

    namelist / control / print_nml

    ! initialize variables
    print_nml = params%print_nml

    ! write_all
    write(unit=iounit, nml=control)
end subroutine


subroutine parse_command_argument_group1 (params,i, iostat, arg)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Maybe assign ith command line argument to the params type
    ! Input:
    !   params : the paramter type
    !   i : integer, positional argument of the parameter name
    ! Output:
    !   iostat : integer, optional : 0 if success, 1 otherwise (if not
    !       provided, an error is thrown in case of failure)
    !   arg : character, optional : the ith command line argument
    !       as returned by native get_command_argument(i, arg)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(group1_t), intent(inout) :: params
    integer, intent(inout) :: i
    integer, optional :: iostat
    character(len=*), optional :: arg
    character(len=512) :: argn, argv

    call get_command_argument(i, argn)

    if (present(arg)) arg = trim(argn)

    ! Print HELP ?
    if (argn == '--help' .or. argn=='-h') then
      call print_help_group1(params)
      if (present(iostat)) iostat = 0
      return
    endif

    if (argn(1:2)  /= "--") then
      stop("ERROR: type-specific command line &
        arguments must start with '--'")
    endif

    if (has_param_group1(params, trim(argn(3:)))) then
      ! +++++  present
      call get_command_argument(i+1, argv)
      call set_param_string_group1(params, trim(argn(3:)), trim(argv))
      i = i+1
      if (present(iostat)) then
        iostat = 0
      endif
    else
      ! +++++  no found
      if (present(iostat)) then
        iostat=1
      else
        write(*,*) "ERROR: unknown parameter in group1 : ",trim(argn)
        write(*,*) ""
        write(*,*) "-h or --help for HELP"
        stop
      endif
    endif
end subroutine

subroutine print_help_group1(params, iounit, default)
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Print HELP on a derived type, useful for command-line arg
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  type(group1_t), intent(in) :: params
  integer, optional :: iounit
  integer :: io
  logical, optional :: default
  logical :: def
  character(len=2000) :: valuestr
  character(len=20) :: valuelen
  if (present(iounit)) then
    io = iounit
  else
    io = 6  ! screen
  endif
  if (present(default)) then
    def = default
  else
    def = .true. ! by default do show default values
  endif
  write(io, *) " "
  write(io, *) "+++++++++++++++++      group1      ++++++++++++++++++"

if (def) then
    write(valuestr, *) params%string1
    write(io, *) "--string1  (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--string1  (type: character(len=clen) :: string1)"
endif

if (def) then
    write(valuestr, *) params%stringarr1(1) ! only first element
    write(valuelen, *) len(trim(adjustl(valuestr)))
    write(io, '("--stringarr1  (default: &
        [",A'//trim(valuelen)//',", ...], size=",I2,")")') &
            trim(adjustl(valuestr)), size(params%stringarr1)
else
    write(io, *) "--stringarr1  (type: character(len=clen), & 
dimension(3) :: stringarr1)"
endif

if (def) then
    write(valuestr, *) params%logical1
    write(io, *) "--logical1  (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--logical1  (type: logical :: logical1)"
endif

if (def) then
    write(valuestr, *) params%integer1
    write(io, *) "--integer1 Comment about integer1 (default: & 
",trim(adjustl(valuestr))," )"
else
    write(io, *) "--integer1 Comment about integer1 (type: & 
integer(kind=ip) :: integer1)"
endif

if (def) then
    write(valuestr, *) params%integer2
    write(io, *) "--integer2 Another comment for integer2 (default: & 
",trim(adjustl(valuestr))," )"
else
    write(io, *) "--integer2 Another comment for integer2 (type: & 
integer(kind=ip) :: integer2)"
endif

if (def) then
    write(valuestr, *) params%string2
    write(io, *) "--string2  (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--string2  (type: character(len=clen) :: string2)"
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
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "group1%string1 = ", & 
params%string1

    if (IOSTAT /= 0) then
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for & 
--group1%string1"
        else
            write(*,*) "ERROR converting string to character(len=clen) & 
:: string1: --group1%string1 ",trim(string)
        endif
        stop
    endif

case ('stringarr1', 'group1%stringarr1')
    call string_to_array(string, params%stringarr1, iostat=iostat)

    if (IOSTAT /= 0) then
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for & 
--group1%stringarr1"
        else
            write(*,*) "ERROR converting string to & 
character(len=clen), dimension(3) :: stringarr1: --group1%stringarr1 & 
",trim(string)
        endif
        stop
    endif

case ('logical1', 'group1%logical1')
    read(string, *, iostat=IOSTAT) params%logical1
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "group1%logical1 = ", & 
params%logical1

    if (IOSTAT /= 0) then
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for & 
--group1%logical1"
        else
            write(*,*) "ERROR converting string to logical :: & 
logical1: --group1%logical1 ",trim(string)
        endif
        stop
    endif

case ('integer1', 'group1%integer1')
    read(string, *, iostat=IOSTAT) params%integer1
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "group1%integer1 = ", & 
params%integer1

    if (IOSTAT /= 0) then
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for & 
--group1%integer1"
        else
            write(*,*) "ERROR converting string to integer(kind=ip) :: & 
integer1: --group1%integer1 ",trim(string)
        endif
        stop
    endif

case ('integer2', 'group1%integer2')
    read(string, *, iostat=IOSTAT) params%integer2
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "group1%integer2 = ", & 
params%integer2

    if (IOSTAT /= 0) then
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for & 
--group1%integer2"
        else
            write(*,*) "ERROR converting string to integer(kind=ip) :: & 
integer2: --group1%integer2 ",trim(string)
        endif
        stop
    endif

case ('string2', 'group1%string2')
    read(string, *, iostat=IOSTAT) params%string2
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "group1%string2 = ", & 
params%string2

    if (IOSTAT /= 0) then
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for & 
--group1%string2"
        else
            write(*,*) "ERROR converting string to character(len=clen) & 
:: string2: --group1%string2 ",trim(string)
        endif
        stop
    endif

    case default
      write(*,*) "ERROR set_param_string for group1: unknown member :: & 
",trim(name)
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


subroutine parse_command_argument_group2 (params,i, iostat, arg)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Maybe assign ith command line argument to the params type
    ! Input:
    !   params : the paramter type
    !   i : integer, positional argument of the parameter name
    ! Output:
    !   iostat : integer, optional : 0 if success, 1 otherwise (if not
    !       provided, an error is thrown in case of failure)
    !   arg : character, optional : the ith command line argument
    !       as returned by native get_command_argument(i, arg)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(group2_t), intent(inout) :: params
    integer, intent(inout) :: i
    integer, optional :: iostat
    character(len=*), optional :: arg
    character(len=512) :: argn, argv

    call get_command_argument(i, argn)

    if (present(arg)) arg = trim(argn)

    ! Print HELP ?
    if (argn == '--help' .or. argn=='-h') then
      call print_help_group2(params)
      if (present(iostat)) iostat = 0
      return
    endif

    if (argn(1:2)  /= "--") then
      stop("ERROR: type-specific command line &
        arguments must start with '--'")
    endif

    if (has_param_group2(params, trim(argn(3:)))) then
      ! +++++  present
      call get_command_argument(i+1, argv)
      call set_param_string_group2(params, trim(argn(3:)), trim(argv))
      i = i+1
      if (present(iostat)) then
        iostat = 0
      endif
    else
      ! +++++  no found
      if (present(iostat)) then
        iostat=1
      else
        write(*,*) "ERROR: unknown parameter in group2 : ",trim(argn)
        write(*,*) ""
        write(*,*) "-h or --help for HELP"
        stop
      endif
    endif
end subroutine

subroutine print_help_group2(params, iounit, default)
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Print HELP on a derived type, useful for command-line arg
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  type(group2_t), intent(in) :: params
  integer, optional :: iounit
  integer :: io
  logical, optional :: default
  logical :: def
  character(len=2000) :: valuestr
  character(len=20) :: valuelen
  if (present(iounit)) then
    io = iounit
  else
    io = 6  ! screen
  endif
  if (present(default)) then
    def = default
  else
    def = .true. ! by default do show default values
  endif
  write(io, *) " "
  write(io, *) "+++++++++++++++++      group2      ++++++++++++++++++"

if (def) then
    write(valuestr, *) params%string1
    write(io, *) "--string1  (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--string1  (type: character(len=clen) :: string1)"
endif

if (def) then
    write(valuestr, *) params%stringarr1(1) ! only first element
    write(valuelen, *) len(trim(adjustl(valuestr)))
    write(io, '("--stringarr1  (default: &
        [",A'//trim(valuelen)//',", ...], size=",I2,")")') &
            trim(adjustl(valuestr)), size(params%stringarr1)
else
    write(io, *) "--stringarr1  (type: character(len=clen), & 
dimension(3) :: stringarr1)"
endif

if (def) then
    write(valuestr, *) params%logical1
    write(io, *) "--logical1  (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--logical1  (type: logical :: logical1)"
endif

if (def) then
    write(valuestr, *) params%integer1
    write(io, *) "--integer1 Comment about integer1 (default: & 
",trim(adjustl(valuestr))," )"
else
    write(io, *) "--integer1 Comment about integer1 (type: & 
integer(kind=ip) :: integer1)"
endif

if (def) then
    write(valuestr, *) params%integer2
    write(io, *) "--integer2 Another comment for integer2 (default: & 
",trim(adjustl(valuestr))," )"
else
    write(io, *) "--integer2 Another comment for integer2 (type: & 
integer(kind=ip) :: integer2)"
endif

if (def) then
    write(valuestr, *) params%string2
    write(io, *) "--string2 nasty ' sign in comment (default: & 
",trim(adjustl(valuestr))," )"
else
    write(io, *) "--string2 nasty ' sign in comment (type: & 
character(len=clen) :: string2)"
endif

if (def) then
    write(valuestr, *) params%intarr1(1) ! only first element
    write(valuelen, *) len(trim(adjustl(valuestr)))
    write(io, '("--intarr1  (default: &
        [",A'//trim(valuelen)//',", ...], size=",I2,")")') &
            trim(adjustl(valuestr)), size(params%intarr1)
else
    write(io, *) "--intarr1  (type: integer(kind=ip), dimension(7) :: & 
intarr1)"
endif

if (def) then
    write(valuestr, *) params%double1
    write(io, *) "--double1  (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--double1  (type: real(kind=dp) :: double1)"
endif

if (def) then
    write(valuestr, *) params%dblarr1(1) ! only first element
    write(valuelen, *) len(trim(adjustl(valuestr)))
    write(io, '("--dblarr1  (default: &
        [",A'//trim(valuelen)//',", ...], size=",I2,")")') &
            trim(adjustl(valuestr)), size(params%dblarr1)
else
    write(io, *) "--dblarr1  (type: real(kind=dp), dimension(5) :: & 
dblarr1)"
endif

if (def) then
    write(valuestr, *) params%logarr1(1) ! only first element
    write(valuelen, *) len(trim(adjustl(valuestr)))
    write(io, '("--logarr1  (default: &
        [",A'//trim(valuelen)//',", ...], size=",I2,")")') &
            trim(adjustl(valuestr)), size(params%logarr1)
else
    write(io, *) "--logarr1  (type: logical, dimension(5) :: logarr1)"
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
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "group2%string1 = ", & 
params%string1

    if (IOSTAT /= 0) then
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for & 
--group2%string1"
        else
            write(*,*) "ERROR converting string to character(len=clen) & 
:: string1: --group2%string1 ",trim(string)
        endif
        stop
    endif

case ('stringarr1', 'group2%stringarr1')
    call string_to_array(string, params%stringarr1, iostat=iostat)

    if (IOSTAT /= 0) then
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for & 
--group2%stringarr1"
        else
            write(*,*) "ERROR converting string to & 
character(len=clen), dimension(3) :: stringarr1: --group2%stringarr1 & 
",trim(string)
        endif
        stop
    endif

case ('logical1', 'group2%logical1')
    read(string, *, iostat=IOSTAT) params%logical1
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "group2%logical1 = ", & 
params%logical1

    if (IOSTAT /= 0) then
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for & 
--group2%logical1"
        else
            write(*,*) "ERROR converting string to logical :: & 
logical1: --group2%logical1 ",trim(string)
        endif
        stop
    endif

case ('integer1', 'group2%integer1')
    read(string, *, iostat=IOSTAT) params%integer1
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "group2%integer1 = ", & 
params%integer1

    if (IOSTAT /= 0) then
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for & 
--group2%integer1"
        else
            write(*,*) "ERROR converting string to integer(kind=ip) :: & 
integer1: --group2%integer1 ",trim(string)
        endif
        stop
    endif

case ('integer2', 'group2%integer2')
    read(string, *, iostat=IOSTAT) params%integer2
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "group2%integer2 = ", & 
params%integer2

    if (IOSTAT /= 0) then
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for & 
--group2%integer2"
        else
            write(*,*) "ERROR converting string to integer(kind=ip) :: & 
integer2: --group2%integer2 ",trim(string)
        endif
        stop
    endif

case ('string2', 'group2%string2')
    read(string, *, iostat=IOSTAT) params%string2
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "group2%string2 = ", & 
params%string2

    if (IOSTAT /= 0) then
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for & 
--group2%string2"
        else
            write(*,*) "ERROR converting string to character(len=clen) & 
:: string2: --group2%string2 ",trim(string)
        endif
        stop
    endif

case ('intarr1', 'group2%intarr1')
    call string_to_array(string, params%intarr1, iostat=iostat)

    if (IOSTAT /= 0) then
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for & 
--group2%intarr1"
        else
            write(*,*) "ERROR converting string to integer(kind=ip), & 
dimension(7) :: intarr1: --group2%intarr1 ",trim(string)
        endif
        stop
    endif

case ('double1', 'group2%double1')
    read(string, *, iostat=IOSTAT) params%double1
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "group2%double1 = ", & 
params%double1

    if (IOSTAT /= 0) then
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for & 
--group2%double1"
        else
            write(*,*) "ERROR converting string to real(kind=dp) :: & 
double1: --group2%double1 ",trim(string)
        endif
        stop
    endif

case ('dblarr1', 'group2%dblarr1')
    call string_to_array(string, params%dblarr1, iostat=iostat)

    if (IOSTAT /= 0) then
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for & 
--group2%dblarr1"
        else
            write(*,*) "ERROR converting string to real(kind=dp), & 
dimension(5) :: dblarr1: --group2%dblarr1 ",trim(string)
        endif
        stop
    endif

case ('logarr1', 'group2%logarr1')
    call string_to_array(string, params%logarr1, iostat=iostat)

    if (IOSTAT /= 0) then
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for & 
--group2%logarr1"
        else
            write(*,*) "ERROR converting string to logical, & 
dimension(5) :: logarr1: --group2%logarr1 ",trim(string)
        endif
        stop
    endif

    case default
      write(*,*) "ERROR set_param_string for group2: unknown member :: & 
",trim(name)
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


subroutine parse_command_argument_control (params,i, iostat, arg)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Maybe assign ith command line argument to the params type
    ! Input:
    !   params : the paramter type
    !   i : integer, positional argument of the parameter name
    ! Output:
    !   iostat : integer, optional : 0 if success, 1 otherwise (if not
    !       provided, an error is thrown in case of failure)
    !   arg : character, optional : the ith command line argument
    !       as returned by native get_command_argument(i, arg)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(control_t), intent(inout) :: params
    integer, intent(inout) :: i
    integer, optional :: iostat
    character(len=*), optional :: arg
    character(len=512) :: argn, argv

    call get_command_argument(i, argn)

    if (present(arg)) arg = trim(argn)

    ! Print HELP ?
    if (argn == '--help' .or. argn=='-h') then
      call print_help_control(params)
      if (present(iostat)) iostat = 0
      return
    endif

    if (argn(1:2)  /= "--") then
      stop("ERROR: type-specific command line &
        arguments must start with '--'")
    endif

    if (has_param_control(params, trim(argn(3:)))) then
      ! +++++  present
      call get_command_argument(i+1, argv)
      call set_param_string_control(params, trim(argn(3:)), & 
trim(argv))
      i = i+1
      if (present(iostat)) then
        iostat = 0
      endif
    else
      ! +++++  no found
      if (present(iostat)) then
        iostat=1
      else
        write(*,*) "ERROR: unknown parameter in control : ",trim(argn)
        write(*,*) ""
        write(*,*) "-h or --help for HELP"
        stop
      endif
    endif
end subroutine

subroutine print_help_control(params, iounit, default)
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Print HELP on a derived type, useful for command-line arg
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  type(control_t), intent(in) :: params
  integer, optional :: iounit
  integer :: io
  logical, optional :: default
  logical :: def
  character(len=2000) :: valuestr
  character(len=20) :: valuelen
  if (present(iounit)) then
    io = iounit
  else
    io = 6  ! screen
  endif
  if (present(default)) then
    def = default
  else
    def = .true. ! by default do show default values
  endif
  write(io, *) " "
  write(io, *) "+++++++++++++++++      control & 
++++++++++++++++++"

if (def) then
    write(valuestr, *) params%print_nml
    write(io, *) "--print_nml print read namelist to string? (default: & 
",trim(adjustl(valuestr))," )"
else
    write(io, *) "--print_nml print read namelist to string? (type: & 
logical :: print_nml)"
endif

end subroutine

subroutine set_param_string_control (params, name, string)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the control type from a string argument
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(control_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: string
    integer :: iostat

    select case (name)

case ('print_nml', 'control%print_nml')
    read(string, *, iostat=IOSTAT) params%print_nml
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "control%print_nml = ", & 
params%print_nml

    if (IOSTAT /= 0) then
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for & 
--control%print_nml"
        else
            write(*,*) "ERROR converting string to logical :: & 
print_nml: --control%print_nml ",trim(string)
        endif
        stop
    endif

    case default
      write(*,*) "ERROR set_param_string for control: unknown member & 
:: ",trim(name)
      stop
    end select
end subroutine

function has_param_control (params, name) result(has_param)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Test whether an attribute name is member of the type control
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(control_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    logical :: has_param

    has_param = .true.
    select case (name)
      case ('print_nml', 'control%print_nml')

    case default
      has_param = .false.
    end select
end function



subroutine set_param_group1_character (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group1 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(group1_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: value

    select case (name)

    case ('string1', 'group1%string1')
        params%string1 = value

    case ('string2', 'group1%string2')
        params%string2 = value

        case default
          write(*,*) "ERROR set_param for group1: unknown type member: & 
character(len=*) :: ",trim(name)
            stop
    end select
end subroutine

subroutine get_param_group1_character (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group1 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(group1_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    character(len=*), intent(out) :: value

    select case (name)

    case ('string1', 'group1%string1')
        value = params%string1

    case ('string2', 'group1%string2')
        value = params%string2

        case default
          write(*,*) "ERROR get_param for group1: unknown type member & 
character(len=*) :: ",trim(name)
            stop
    end select
end subroutine

subroutine set_param_group1_character_arr (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group1 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(group1_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    character(len=*), dimension(:), intent(in) :: value

    select case (name)

    case ('stringarr1', 'group1%stringarr1')
        params%stringarr1 = value

        case default
          write(*,*) "ERROR set_param for group1: unknown type member: & 
character(len=*), dimension(:) :: ",trim(name)
            stop
    end select
end subroutine

subroutine get_param_group1_character_arr (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group1 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(group1_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    character(len=*), dimension(:), intent(out) :: value

    select case (name)

    case ('stringarr1', 'group1%stringarr1')
        value = params%stringarr1

        case default
          write(*,*) "ERROR get_param for group1: unknown type member & 
character(len=*), dimension(:) :: ",trim(name)
            stop
    end select
end subroutine

subroutine set_param_group1_integer (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group1 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(group1_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    integer(kind=ip), intent(in) :: value

    select case (name)

    case ('integer1', 'group1%integer1')
        params%integer1 = value

    case ('integer2', 'group1%integer2')
        params%integer2 = value

        case default
          write(*,*) "ERROR set_param for group1: unknown type member: & 
integer(kind=ip) :: ",trim(name)
            stop
    end select
end subroutine

subroutine get_param_group1_integer (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group1 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(group1_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    integer(kind=ip), intent(out) :: value

    select case (name)

    case ('integer1', 'group1%integer1')
        value = params%integer1

    case ('integer2', 'group1%integer2')
        value = params%integer2

        case default
          write(*,*) "ERROR get_param for group1: unknown type member & 
integer(kind=ip) :: ",trim(name)
            stop
    end select
end subroutine

subroutine set_param_group1_logical (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group1 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(group1_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    logical, intent(in) :: value

    select case (name)

    case ('logical1', 'group1%logical1')
        params%logical1 = value

        case default
          write(*,*) "ERROR set_param for group1: unknown type member: & 
logical :: ",trim(name)
            stop
    end select
end subroutine

subroutine get_param_group1_logical (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group1 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(group1_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    logical, intent(out) :: value

    select case (name)

    case ('logical1', 'group1%logical1')
        value = params%logical1

        case default
          write(*,*) "ERROR get_param for group1: unknown type member & 
logical :: ",trim(name)
            stop
    end select
end subroutine

subroutine set_param_group2_character (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group2 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(group2_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: value

    select case (name)

    case ('string1', 'group2%string1')
        params%string1 = value

    case ('string2', 'group2%string2')
        params%string2 = value

        case default
          write(*,*) "ERROR set_param for group2: unknown type member: & 
character(len=*) :: ",trim(name)
            stop
    end select
end subroutine

subroutine get_param_group2_character (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group2 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(group2_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    character(len=*), intent(out) :: value

    select case (name)

    case ('string1', 'group2%string1')
        value = params%string1

    case ('string2', 'group2%string2')
        value = params%string2

        case default
          write(*,*) "ERROR get_param for group2: unknown type member & 
character(len=*) :: ",trim(name)
            stop
    end select
end subroutine

subroutine set_param_group2_character_arr (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group2 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(group2_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    character(len=*), dimension(:), intent(in) :: value

    select case (name)

    case ('stringarr1', 'group2%stringarr1')
        params%stringarr1 = value

        case default
          write(*,*) "ERROR set_param for group2: unknown type member: & 
character(len=*), dimension(:) :: ",trim(name)
            stop
    end select
end subroutine

subroutine get_param_group2_character_arr (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group2 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(group2_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    character(len=*), dimension(:), intent(out) :: value

    select case (name)

    case ('stringarr1', 'group2%stringarr1')
        value = params%stringarr1

        case default
          write(*,*) "ERROR get_param for group2: unknown type member & 
character(len=*), dimension(:) :: ",trim(name)
            stop
    end select
end subroutine

subroutine set_param_group2_integer (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group2 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(group2_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    integer(kind=ip), intent(in) :: value

    select case (name)

    case ('integer1', 'group2%integer1')
        params%integer1 = value

    case ('integer2', 'group2%integer2')
        params%integer2 = value

        case default
          write(*,*) "ERROR set_param for group2: unknown type member: & 
integer(kind=ip) :: ",trim(name)
            stop
    end select
end subroutine

subroutine get_param_group2_integer (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group2 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(group2_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    integer(kind=ip), intent(out) :: value

    select case (name)

    case ('integer1', 'group2%integer1')
        value = params%integer1

    case ('integer2', 'group2%integer2')
        value = params%integer2

        case default
          write(*,*) "ERROR get_param for group2: unknown type member & 
integer(kind=ip) :: ",trim(name)
            stop
    end select
end subroutine

subroutine set_param_group2_integer_arr (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group2 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(group2_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    integer(kind=ip), dimension(:), intent(in) :: value

    select case (name)

    case ('intarr1', 'group2%intarr1')
        params%intarr1 = value

        case default
          write(*,*) "ERROR set_param for group2: unknown type member: & 
integer(kind=ip), dimension(:) :: ",trim(name)
            stop
    end select
end subroutine

subroutine get_param_group2_integer_arr (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group2 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(group2_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    integer(kind=ip), dimension(:), intent(out) :: value

    select case (name)

    case ('intarr1', 'group2%intarr1')
        value = params%intarr1

        case default
          write(*,*) "ERROR get_param for group2: unknown type member & 
integer(kind=ip), dimension(:) :: ",trim(name)
            stop
    end select
end subroutine

subroutine set_param_group2_logical (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group2 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(group2_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    logical, intent(in) :: value

    select case (name)

    case ('logical1', 'group2%logical1')
        params%logical1 = value

        case default
          write(*,*) "ERROR set_param for group2: unknown type member: & 
logical :: ",trim(name)
            stop
    end select
end subroutine

subroutine get_param_group2_logical (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group2 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(group2_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    logical, intent(out) :: value

    select case (name)

    case ('logical1', 'group2%logical1')
        value = params%logical1

        case default
          write(*,*) "ERROR get_param for group2: unknown type member & 
logical :: ",trim(name)
            stop
    end select
end subroutine

subroutine set_param_group2_logical_arr (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group2 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(group2_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    logical, dimension(:), intent(in) :: value

    select case (name)

    case ('logarr1', 'group2%logarr1')
        params%logarr1 = value

        case default
          write(*,*) "ERROR set_param for group2: unknown type member: & 
logical, dimension(:) :: ",trim(name)
            stop
    end select
end subroutine

subroutine get_param_group2_logical_arr (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group2 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(group2_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    logical, dimension(:), intent(out) :: value

    select case (name)

    case ('logarr1', 'group2%logarr1')
        value = params%logarr1

        case default
          write(*,*) "ERROR get_param for group2: unknown type member & 
logical, dimension(:) :: ",trim(name)
            stop
    end select
end subroutine

subroutine set_param_group2_real (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group2 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(group2_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    real(kind=dp), intent(in) :: value

    select case (name)

    case ('double1', 'group2%double1')
        params%double1 = value

        case default
          write(*,*) "ERROR set_param for group2: unknown type member: & 
real(kind=dp) :: ",trim(name)
            stop
    end select
end subroutine

subroutine get_param_group2_real (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group2 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(group2_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    real(kind=dp), intent(out) :: value

    select case (name)

    case ('double1', 'group2%double1')
        value = params%double1

        case default
          write(*,*) "ERROR get_param for group2: unknown type member & 
real(kind=dp) :: ",trim(name)
            stop
    end select
end subroutine

subroutine set_param_group2_real_arr (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group2 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(group2_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    real(kind=dp), dimension(:), intent(in) :: value

    select case (name)

    case ('dblarr1', 'group2%dblarr1')
        params%dblarr1 = value

        case default
          write(*,*) "ERROR set_param for group2: unknown type member: & 
real(kind=dp), dimension(:) :: ",trim(name)
            stop
    end select
end subroutine

subroutine get_param_group2_real_arr (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group2 type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(group2_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    real(kind=dp), dimension(:), intent(out) :: value

    select case (name)

    case ('dblarr1', 'group2%dblarr1')
        value = params%dblarr1

        case default
          write(*,*) "ERROR get_param for group2: unknown type member & 
real(kind=dp), dimension(:) :: ",trim(name)
            stop
    end select
end subroutine

subroutine set_param_control_logical (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the control type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(control_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    logical, intent(in) :: value

    select case (name)

    case ('print_nml', 'control%print_nml')
        params%print_nml = value

        case default
          write(*,*) "ERROR set_param for control: unknown type & 
member: logical :: ",trim(name)
            stop
    end select
end subroutine

subroutine get_param_control_logical (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the control type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(control_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    logical, intent(out) :: value

    select case (name)

    case ('print_nml', 'control%print_nml')
        value = params%print_nml

        case default
          write(*,*) "ERROR get_param for control: unknown type member & 
logical :: ",trim(name)
            stop
    end select
end subroutine




end module ioparams

