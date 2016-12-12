! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Automatically generated module by nml2f90
! History: nml2f90.py namelist.nml ioparams --io-nml --command-line --set-get-param -v
!
! https://github.com/perrette/nml-to-f90
! version: 0+untagged.118.g3e2c2de.dirty
!  
! Features included : io_nml, command_line, set_get_param
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

module type_conversion
! =============================================================
!
! Type conversion functions (Courtesy of Alex Robinson's nml module)
! ==> useful to read array (lists) from command list argument)
!
! UPDATE M.P. 20161116: string_to_array :: assume comma-separated values
!
! =============================================================
  implicit none

  integer, parameter :: Float = kind(0.d0)

    interface string_to_array
      !! array as comma-separated values
      module procedure :: string_to_array_string
      module procedure :: string_to_array_integer
      module procedure :: string_to_array_double
      module procedure :: string_to_array_logical
    end interface

  contains

    subroutine signal_error(iostat)
      integer, optional :: iostat
      if(present(iostat)) then
        iostat = -1
        return
      else
        stop
      endif
    end subroutine

    pure function strip_brackets(s1) result(s2)
      character(len=*), intent(in) :: s1
      character(len=256) :: s2

      if (len_trim(s1) < 2) return

      ! head
      if (s1(1:2) == "(/" .and. s1(len_trim(s1)-1:) == "/)") then
        s2 = adjustl(s1(3:len_trim(s1)-2))

      elseif (s1(1:1) == "[" .and. s1(len_trim(s1):len_trim(s1)) == "]") then
        s2 = adjustl(s1(2:len_trim(s1)-1))

      elseif (s1(1:1) == "(" .and. s1(len_trim(s1):len_trim(s1)) == ")") then
        s2 = adjustl(s1(2:len_trim(s1)-1))

      else
        s2 = s1

      endif

    end function

    subroutine string_to_array_string (string, value, iostat)

      character(len=*), intent(in) :: string
      character(len=*), intent(out) :: value(:)
      integer, optional :: iostat
      character(len=256) :: tmpstr
      integer :: q, q2

      tmpstr = strip_brackets(trim(adjustl(string)))

      do q=1,size(value)
        q2 = index(tmpstr,',')
        if (q2 == 0) then
          q2 = len(tmpstr)+1

          if (q /= size(value)) then
            write(*,*) "command-line :: expected array of size",size(value),', & 
& got:', q
            call signal_error(iostat)
            return
          endif

        else

          if (q == size(value)) then
            write(*,*) "command-line :: array size exceeded",size(value)
            call signal_error(iostat)
            return
          endif

        endif

        read(tmpstr(:q2-1), *, iostat=iostat) value(q)
        if (present(iostat)) then
          if (iostat /= 0) return
        endif
        tmpstr = tmpstr(q2+1:)
      enddo

    end subroutine

    subroutine string_to_array_integer (string, value, iostat)

      implicit none

      character(len=*), intent(in) :: string
      integer, intent(out) :: value(:)
      integer, optional :: iostat
      character(len=256) :: tmpvec(size(value))
      integer :: q

      call string_to_array_string (string, tmpvec, iostat)
      if (present(iostat))then
        if (iostat /=0) return
      endif

      do q=1,size(tmpvec)
        read(tmpvec(q), *, iostat=iostat) value(q)
        if (present(iostat))then
          if (iostat /=0) return
        endif
      enddo

    end subroutine

    subroutine string_to_array_double (string, value, iostat)

      implicit none

      character(len=*), intent(in) :: string
      real(Float), intent(out) :: value(:)
      integer, optional :: iostat
      character(len=256) :: tmpvec(size(value))
      integer :: q

      call string_to_array_string (string, tmpvec, iostat)
      if (present(iostat))then
        if (iostat /=0) return
      endif

      do q=1,size(tmpvec)
        read(tmpvec(q), *, iostat=iostat) value(q)
        if (present(iostat))then
          if (iostat /=0) return
        endif
      enddo

    end subroutine

    subroutine string_to_array_logical (string, value, iostat)

      implicit none

      character(len=*), intent(in) :: string
      logical, intent(out) :: value(:)
      integer, optional :: iostat
      character(len=256) :: tmpvec(size(value))
      integer :: q

      call string_to_array_string (string, tmpvec, iostat)
      if (present(iostat))then
        if (iostat /=0) return
      endif

      do q=1,size(tmpvec)
        read(tmpvec(q), *, iostat=iostat) value(q)
        if (present(iostat))then
          if (iostat /=0) return
        endif
      enddo

    end subroutine

end module



module ioparams


  use type_conversion


  implicit none

  private
  public :: group1_t
  public :: group2_t
  public :: control_t
  public :: read_nml
  public :: write_nml
  public :: read_nml_file
  public :: parse_command_args
  public :: print_help
  public :: set_param_string
  public :: has_param
  public :: set_param
  public :: get_param

  public :: join_array
  public :: command_argument_as_array

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

  interface read_nml_file
    module procedure :: read_nml_file_group1
    module procedure :: read_nml_file_group2
    module procedure :: read_nml_file_control
  end interface

  interface parse_command_args
    module procedure :: parse_command_args_group1
    module procedure :: parse_command_args_group2
    module procedure :: parse_command_args_control
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

  function join_array(arr, sep, quotes) result(str)
      ! String representation of a character array
      character(len=*), intent(in) :: arr(:)
      character(len=len(arr)*size(arr)) :: str
      integer :: i, n
      logical, intent(in), optional :: quotes
      character(len=*), intent(in), optional :: sep
      character(len=50) :: sep_opt
      character(len=1) :: q
      integer :: len_sep

      q = "'"
      if (present(quotes)) then
        if (.not. quotes) then
          q = ""
        endif
      endif

      if (present(sep)) then
        sep_opt = sep
        len_sep = len(sep)
      else
        sep_opt = ", "
        len_sep = 2
      endif

      str = ""
      do i=1,size(arr)
          str = trim(str)//sep_opt(:len_sep)//q//trim(arr(i))//q
      enddo
      if (size(arr) > 0) then
        str = str(len_sep+1:)
      endif
  end function

  subroutine command_argument_as_array(args)
      character(len=*), intent(out), allocatable :: args(:)
      integer :: i, n
      n = command_argument_count()
      allocate(args(n))
      do i=1,n
          call get_command_argument(i, args(i))
      enddo
  end subroutine

  ! Namelist I/O ******************************************
    subroutine read_nml_group1 (iounit, params, iostat)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Read the group1 group in a namelist file and assign to type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    integer, intent(in) :: iounit
    type(group1_t), intent(inout) :: params
    integer, optional, intent(out) :: iostat

    character(len=clen) :: string1
    character(len=clen), dimension(3) :: stringarr1
    logical :: logical1
    integer(kind=ip) :: integer1
    integer(kind=ip) :: integer2
    character(len=clen) :: string2

    namelist / group1 / string1, stringarr1, logical1, integer1, integer2, & 
& string2

    ! initialize variables
    string1 = params%string1
    stringarr1 = params%stringarr1
    logical1 = params%logical1
    integer1 = params%integer1
    integer2 = params%integer2
    string2 = params%string2

    ! read all
    if (.not. present(iostat)) then
      read(unit=iounit, nml=group1)
    else
      read(unit=iounit, nml=group1, iostat=iostat)
      if (iostat /= 0 .and. VERBOSE) then
        write(*, *) "Failed to read namelist block: group1"
      endif
    endif


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

    namelist / group1 / string1, stringarr1, logical1, integer1, integer2, & 
& string2

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

subroutine read_nml_file_group1 (file, params, iostat)
    character(len=*), intent(in) :: file
    type(group1_t), intent(inout) :: params
    integer, optional, intent(out) :: iostat
    integer :: unit = 4563
    open(file=file, unit=unit, status='OLD')
    call read_nml_group1 (unit, params, iostat)
    close(unit)
end subroutine

subroutine read_nml_group2 (iounit, params, iostat)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Read the group2 group in a namelist file and assign to type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    integer, intent(in) :: iounit
    type(group2_t), intent(inout) :: params
    integer, optional, intent(out) :: iostat

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

    namelist / group2 / string1, stringarr1, logical1, integer1, integer2, & 
& string2, intarr1, double1, dblarr1, logarr1

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
    if (.not. present(iostat)) then
      read(unit=iounit, nml=group2)
    else
      read(unit=iounit, nml=group2, iostat=iostat)
      if (iostat /= 0 .and. VERBOSE) then
        write(*, *) "Failed to read namelist block: group2"
      endif
    endif


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

    namelist / group2 / string1, stringarr1, logical1, integer1, integer2, & 
& string2, intarr1, double1, dblarr1, logarr1

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

subroutine read_nml_file_group2 (file, params, iostat)
    character(len=*), intent(in) :: file
    type(group2_t), intent(inout) :: params
    integer, optional, intent(out) :: iostat
    integer :: unit = 4563
    open(file=file, unit=unit, status='OLD')
    call read_nml_group2 (unit, params, iostat)
    close(unit)
end subroutine

subroutine read_nml_control (iounit, params, iostat)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Read the control group in a namelist file and assign to type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    integer, intent(in) :: iounit
    type(control_t), intent(inout) :: params
    integer, optional, intent(out) :: iostat

    logical :: print_nml

    namelist / control / print_nml

    ! initialize variables
    print_nml = params%print_nml

    ! read all
    if (.not. present(iostat)) then
      read(unit=iounit, nml=control)
    else
      read(unit=iounit, nml=control, iostat=iostat)
      if (iostat /= 0 .and. VERBOSE) then
        write(*, *) "Failed to read namelist block: control"
      endif
    endif


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

subroutine read_nml_file_control (file, params, iostat)
    character(len=*), intent(in) :: file
    type(control_t), intent(inout) :: params
    integer, optional, intent(out) :: iostat
    integer :: unit = 4563
    open(file=file, unit=unit, status='OLD')
    call read_nml_control (unit, params, iostat)
    close(unit)
end subroutine


! Command-line argument passing
    subroutine parse_command_args_group1 (params, args, unmatched, &
    & stop_on_help, iostat)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Maybe assign ith command line argument to the params type
    ! Input:
    !   params : the paramter type
    ! Output:
    !   iostat : integer, optional
    !        0 : all arguments were found
    !       >0 : number of unmatched arguments
    !       -1 : error when reading
    !       -2 : --help was printed
    !       If not provided, execution stop if iostat /= 0
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(group1_t), intent(inout) :: params
    integer, intent(out), optional :: iostat
    character(len=*), dimension(:), intent(in), optional :: args
    character(len=*), dimension(:), allocatable, intent(inout), optional :: & 
& unmatched
    character(len=clen), dimension(:), allocatable :: args_opt, unmatched_opt
    logical, intent(in), optional :: stop_on_help
    character(len=clen) :: argn, argv
    logical :: missing_value, stop_on_help_opt

    integer :: i,j, n, parsed

    if (present(stop_on_help)) then
      stop_on_help_opt = stop_on_help
    else
      stop_on_help_opt = .true.
    endif

    ! Define a list of command line arguments args_opt
    if (present(args)) then
      ! ... provided as subroutine argument
      args_opt = args
    else
      ! ... provided as command-line argument
      call command_argument_as_array(args_opt)
    endif

    n = size(args_opt)

    if (allocated(unmatched_opt)) deallocate(unmatched_opt)
    allocate(unmatched_opt(n))

    parsed = 0

    i = 1
    j = 1
    do while (i <= n)
      argn = args_opt(i)

      ! Print HELP ?
      if (argn == '--help' .or. argn=='-h') then
        call print_help_group1(params)
        if (present(iostat)) then
          iostat = -2
          return
        elseif (stop_on_help_opt) then
          stop('End of help message : exit')
        endif
      endif

      if (argn(1:2)  /= "--") then
        if (.not.present(iostat) .and. .not.present(unmatched)) then
          write(*,*) "i=",i, "; Got: ",trim(argn)
          stop("ERROR::ioparams type-specific command line &
            & arguments must start with '--'")
        else
          unmatched_opt(j) = trim(argn)
          j = j + 1
          i = i + 1  ! check next argument
          cycle
        endif
      endif

      if (has_param_group1(params, trim(argn(3:)))) then
        ! +++++  present

        ! only "--name value" is tolerated
        ! check if value is missing
        missing_value = .false.
        if (i+1 <= n) then
          argv = args_opt(i+1)
          if ( len(argv) > 1) then
            ! ...next argument starts with '--'
            if (argv(1:2)  == "--" ) then
              missing_value = .true.
            endif
          endif
        else
          ! ...end of command line
          missing_value = .true.
        endif

        if (missing_value) then
          write(*,*) "ERROR::ioparams::group1: &
            & missing value for "//trim(argn(3:))
          if (present(iostat)) then
            iostat = -1
            return
          else
            stop
          endif
        endif

        call set_param_string_group1(params, trim(argn(3:)), trim(argv), &
          & iostat=iostat)

        if (present(iostat)) then
          if (iostat /= 0) then
            return  ! error
          endif
        endif

        parsed = parsed + 2
        i = i + 2

      else
        ! +++++  not found

        if (.not. present(iostat) .and. .not. present(unmatched)) then
          write(*,*) "ERROR: unknown parameter in group1 : ",trim(argn)
          write(*,*) ""
          write(*,*) "-h or --help for HELP"
          stop
        endif

        unmatched_opt(j) = trim(argn)
        j = j + 1
        i = i + 1

      endif

    enddo

    ! At this point, any type error or --help message cases are already sorted
! out
    if (present(iostat)) then
      iostat = n - parsed
    endif

    if (present(unmatched)) then
      unmatched = unmatched_opt(:n-parsed)
    endif

    deallocate(args_opt, unmatched_opt)

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
  character(len=2000) :: nameshort
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
  write(io, *) " "

write(nameshort, *) "string1"
if (def) then
    write(valuestr, *) params%string1
    write(valuelen, *) max(1,len(trim(adjustl(valuestr))))
    write(io, '("  --",A20," (default: ",A'//trim(valuelen)//',")")') & 
& adjustl(nameshort), trim(adjustl(valuestr))
else
    write(io, '("  --",A20,"")') adjustl(nameshort)
endif

write(nameshort, *) "stringarr1"
if (def) then
    write(valuestr, *) params%stringarr1(1) ! only first element
    write(valuelen, *) max(1,len(trim(adjustl(valuestr))))
    write(io, '("  --",A20," (default: &
        [",A'//trim(valuelen)//',", ...], size=",I2,")")') adjustl(nameshort), & 
& trim(adjustl(valuestr)), size(params%stringarr1)
else
    write(io, '("  --",A20,"")') adjustl(nameshort)
endif

write(nameshort, *) "logical1"
if (def) then
    write(valuestr, *) params%logical1
    write(valuelen, *) max(1,len(trim(adjustl(valuestr))))
    write(io, '("  --",A20," (default: ",A'//trim(valuelen)//',")")') & 
& adjustl(nameshort), trim(adjustl(valuestr))
else
    write(io, '("  --",A20,"")') adjustl(nameshort)
endif

write(nameshort, *) "integer1"
if (def) then
    write(valuestr, *) params%integer1
    write(valuelen, *) max(1,len(trim(adjustl(valuestr))))
    write(io, '("  --",A20,"Comment about integer1 (default: & 
& ",A'//trim(valuelen)//',")")') adjustl(nameshort), trim(adjustl(valuestr))
else
    write(io, '("  --",A20,"Comment about integer1")') adjustl(nameshort)
endif

write(nameshort, *) "integer2"
if (def) then
    write(valuestr, *) params%integer2
    write(valuelen, *) max(1,len(trim(adjustl(valuestr))))
    write(io, '("  --",A20,"Another comment for integer2 (default: & 
& ",A'//trim(valuelen)//',")")') adjustl(nameshort), trim(adjustl(valuestr))
else
    write(io, '("  --",A20,"Another comment for integer2")') & 
& adjustl(nameshort)
endif

write(nameshort, *) "string2"
if (def) then
    write(valuestr, *) params%string2
    write(valuelen, *) max(1,len(trim(adjustl(valuestr))))
    write(io, '("  --",A20," (default: ",A'//trim(valuelen)//',")")') & 
& adjustl(nameshort), trim(adjustl(valuestr))
else
    write(io, '("  --",A20,"")') adjustl(nameshort)
endif

end subroutine

subroutine set_param_string_group1 (params, name, string, iostat)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group1 type from a string argument
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(group1_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: string
    integer, intent(out), optional :: iostat
    integer :: io !! local...

    if (present(iostat)) then
      iostat = 0
    endif

    select case (name)

case ('string1', 'group1%string1')
    params%string1 = trim(string)
    if (VERBOSE) write(*,*) "group1%string1 = ", params%string1

case ('stringarr1', 'group1%stringarr1')
    call string_to_array(string, params%stringarr1, iostat=io)
    if (VERBOSE) write(*,*) "group1%stringarr1 = ", & 
& trim(params%stringarr1(1)),', ...'

    if (io /= 0) then
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for & 
& --group1%stringarr1"
        else
            write(*,*) "ERROR converting string to character(len=clen), & 
& dimension(3) :: stringarr1: --group1%stringarr1 ",trim(string)
        endif
        stop
    endif

case ('logical1', 'group1%logical1')
    read(string, *, iostat=io) params%logical1
    if (VERBOSE .or. io/=0) write(*,*) "group1%logical1 = ", params%logical1

    if (io /= 0) then
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --group1%logical1"
        else
            write(*,*) "ERROR converting string to logical :: logical1: & 
& --group1%logical1 ",trim(string)
        endif
        stop
    endif

case ('integer1', 'group1%integer1')
    read(string, *, iostat=io) params%integer1
    if (VERBOSE .or. io/=0) write(*,*) "group1%integer1 = ", params%integer1

    if (io /= 0) then
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --group1%integer1"
        else
            write(*,*) "ERROR converting string to integer(kind=ip) :: & 
& integer1: --group1%integer1 ",trim(string)
        endif
        stop
    endif

case ('integer2', 'group1%integer2')
    read(string, *, iostat=io) params%integer2
    if (VERBOSE .or. io/=0) write(*,*) "group1%integer2 = ", params%integer2

    if (io /= 0) then
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --group1%integer2"
        else
            write(*,*) "ERROR converting string to integer(kind=ip) :: & 
& integer2: --group1%integer2 ",trim(string)
        endif
        stop
    endif

case ('string2', 'group1%string2')
    params%string2 = trim(string)
    if (VERBOSE) write(*,*) "group1%string2 = ", params%string2

    case default
      write(*,*) "ERROR::ioparams::group1: &
        & unknown member :: ", trim(name)
      if (present(iostat)) then
        iostat = -1
      else
        stop
      endif
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


subroutine parse_command_args_group2 (params, args, unmatched, &
    & stop_on_help, iostat)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Maybe assign ith command line argument to the params type
    ! Input:
    !   params : the paramter type
    ! Output:
    !   iostat : integer, optional
    !        0 : all arguments were found
    !       >0 : number of unmatched arguments
    !       -1 : error when reading
    !       -2 : --help was printed
    !       If not provided, execution stop if iostat /= 0
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(group2_t), intent(inout) :: params
    integer, intent(out), optional :: iostat
    character(len=*), dimension(:), intent(in), optional :: args
    character(len=*), dimension(:), allocatable, intent(inout), optional :: & 
& unmatched
    character(len=clen), dimension(:), allocatable :: args_opt, unmatched_opt
    logical, intent(in), optional :: stop_on_help
    character(len=clen) :: argn, argv
    logical :: missing_value, stop_on_help_opt

    integer :: i,j, n, parsed

    if (present(stop_on_help)) then
      stop_on_help_opt = stop_on_help
    else
      stop_on_help_opt = .true.
    endif

    ! Define a list of command line arguments args_opt
    if (present(args)) then
      ! ... provided as subroutine argument
      args_opt = args
    else
      ! ... provided as command-line argument
      call command_argument_as_array(args_opt)
    endif

    n = size(args_opt)

    if (allocated(unmatched_opt)) deallocate(unmatched_opt)
    allocate(unmatched_opt(n))

    parsed = 0

    i = 1
    j = 1
    do while (i <= n)
      argn = args_opt(i)

      ! Print HELP ?
      if (argn == '--help' .or. argn=='-h') then
        call print_help_group2(params)
        if (present(iostat)) then
          iostat = -2
          return
        elseif (stop_on_help_opt) then
          stop('End of help message : exit')
        endif
      endif

      if (argn(1:2)  /= "--") then
        if (.not.present(iostat) .and. .not.present(unmatched)) then
          write(*,*) "i=",i, "; Got: ",trim(argn)
          stop("ERROR::ioparams type-specific command line &
            & arguments must start with '--'")
        else
          unmatched_opt(j) = trim(argn)
          j = j + 1
          i = i + 1  ! check next argument
          cycle
        endif
      endif

      if (has_param_group2(params, trim(argn(3:)))) then
        ! +++++  present

        ! only "--name value" is tolerated
        ! check if value is missing
        missing_value = .false.
        if (i+1 <= n) then
          argv = args_opt(i+1)
          if ( len(argv) > 1) then
            ! ...next argument starts with '--'
            if (argv(1:2)  == "--" ) then
              missing_value = .true.
            endif
          endif
        else
          ! ...end of command line
          missing_value = .true.
        endif

        if (missing_value) then
          write(*,*) "ERROR::ioparams::group2: &
            & missing value for "//trim(argn(3:))
          if (present(iostat)) then
            iostat = -1
            return
          else
            stop
          endif
        endif

        call set_param_string_group2(params, trim(argn(3:)), trim(argv), &
          & iostat=iostat)

        if (present(iostat)) then
          if (iostat /= 0) then
            return  ! error
          endif
        endif

        parsed = parsed + 2
        i = i + 2

      else
        ! +++++  not found

        if (.not. present(iostat) .and. .not. present(unmatched)) then
          write(*,*) "ERROR: unknown parameter in group2 : ",trim(argn)
          write(*,*) ""
          write(*,*) "-h or --help for HELP"
          stop
        endif

        unmatched_opt(j) = trim(argn)
        j = j + 1
        i = i + 1

      endif

    enddo

    ! At this point, any type error or --help message cases are already sorted
! out
    if (present(iostat)) then
      iostat = n - parsed
    endif

    if (present(unmatched)) then
      unmatched = unmatched_opt(:n-parsed)
    endif

    deallocate(args_opt, unmatched_opt)

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
  character(len=2000) :: nameshort
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
  write(io, *) " "

write(nameshort, *) "string1"
if (def) then
    write(valuestr, *) params%string1
    write(valuelen, *) max(1,len(trim(adjustl(valuestr))))
    write(io, '("  --",A20," (default: ",A'//trim(valuelen)//',")")') & 
& adjustl(nameshort), trim(adjustl(valuestr))
else
    write(io, '("  --",A20,"")') adjustl(nameshort)
endif

write(nameshort, *) "stringarr1"
if (def) then
    write(valuestr, *) params%stringarr1(1) ! only first element
    write(valuelen, *) max(1,len(trim(adjustl(valuestr))))
    write(io, '("  --",A20," (default: &
        [",A'//trim(valuelen)//',", ...], size=",I2,")")') adjustl(nameshort), & 
& trim(adjustl(valuestr)), size(params%stringarr1)
else
    write(io, '("  --",A20,"")') adjustl(nameshort)
endif

write(nameshort, *) "logical1"
if (def) then
    write(valuestr, *) params%logical1
    write(valuelen, *) max(1,len(trim(adjustl(valuestr))))
    write(io, '("  --",A20," (default: ",A'//trim(valuelen)//',")")') & 
& adjustl(nameshort), trim(adjustl(valuestr))
else
    write(io, '("  --",A20,"")') adjustl(nameshort)
endif

write(nameshort, *) "integer1"
if (def) then
    write(valuestr, *) params%integer1
    write(valuelen, *) max(1,len(trim(adjustl(valuestr))))
    write(io, '("  --",A20,"Comment about integer1 (default: & 
& ",A'//trim(valuelen)//',")")') adjustl(nameshort), trim(adjustl(valuestr))
else
    write(io, '("  --",A20,"Comment about integer1")') adjustl(nameshort)
endif

write(nameshort, *) "integer2"
if (def) then
    write(valuestr, *) params%integer2
    write(valuelen, *) max(1,len(trim(adjustl(valuestr))))
    write(io, '("  --",A20,"Another comment for integer2 (default: & 
& ",A'//trim(valuelen)//',")")') adjustl(nameshort), trim(adjustl(valuestr))
else
    write(io, '("  --",A20,"Another comment for integer2")') & 
& adjustl(nameshort)
endif

write(nameshort, *) "string2"
if (def) then
    write(valuestr, *) params%string2
    write(valuelen, *) max(1,len(trim(adjustl(valuestr))))
    write(io, '("  --",A20,"nasty   sign in comment (default: & 
& ",A'//trim(valuelen)//',")")') adjustl(nameshort), trim(adjustl(valuestr))
else
    write(io, '("  --",A20,"nasty   sign in comment")') adjustl(nameshort)
endif

write(nameshort, *) "intarr1"
if (def) then
    write(valuestr, *) params%intarr1(1) ! only first element
    write(valuelen, *) max(1,len(trim(adjustl(valuestr))))
    write(io, '("  --",A20," (default: &
        [",A'//trim(valuelen)//',", ...], size=",I2,")")') adjustl(nameshort), & 
& trim(adjustl(valuestr)), size(params%intarr1)
else
    write(io, '("  --",A20,"")') adjustl(nameshort)
endif

write(nameshort, *) "double1"
if (def) then
    write(valuestr, *) params%double1
    write(valuelen, *) max(1,len(trim(adjustl(valuestr))))
    write(io, '("  --",A20," (default: ",A'//trim(valuelen)//',")")') & 
& adjustl(nameshort), trim(adjustl(valuestr))
else
    write(io, '("  --",A20,"")') adjustl(nameshort)
endif

write(nameshort, *) "dblarr1"
if (def) then
    write(valuestr, *) params%dblarr1(1) ! only first element
    write(valuelen, *) max(1,len(trim(adjustl(valuestr))))
    write(io, '("  --",A20," (default: &
        [",A'//trim(valuelen)//',", ...], size=",I2,")")') adjustl(nameshort), & 
& trim(adjustl(valuestr)), size(params%dblarr1)
else
    write(io, '("  --",A20,"")') adjustl(nameshort)
endif

write(nameshort, *) "logarr1"
if (def) then
    write(valuestr, *) params%logarr1(1) ! only first element
    write(valuelen, *) max(1,len(trim(adjustl(valuestr))))
    write(io, '("  --",A20," (default: &
        [",A'//trim(valuelen)//',", ...], size=",I2,")")') adjustl(nameshort), & 
& trim(adjustl(valuestr)), size(params%logarr1)
else
    write(io, '("  --",A20,"")') adjustl(nameshort)
endif

end subroutine

subroutine set_param_string_group2 (params, name, string, iostat)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the group2 type from a string argument
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(group2_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: string
    integer, intent(out), optional :: iostat
    integer :: io !! local...

    if (present(iostat)) then
      iostat = 0
    endif

    select case (name)

case ('string1', 'group2%string1')
    params%string1 = trim(string)
    if (VERBOSE) write(*,*) "group2%string1 = ", params%string1

case ('stringarr1', 'group2%stringarr1')
    call string_to_array(string, params%stringarr1, iostat=io)
    if (VERBOSE) write(*,*) "group2%stringarr1 = ", & 
& trim(params%stringarr1(1)),', ...'

    if (io /= 0) then
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for & 
& --group2%stringarr1"
        else
            write(*,*) "ERROR converting string to character(len=clen), & 
& dimension(3) :: stringarr1: --group2%stringarr1 ",trim(string)
        endif
        stop
    endif

case ('logical1', 'group2%logical1')
    read(string, *, iostat=io) params%logical1
    if (VERBOSE .or. io/=0) write(*,*) "group2%logical1 = ", params%logical1

    if (io /= 0) then
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --group2%logical1"
        else
            write(*,*) "ERROR converting string to logical :: logical1: & 
& --group2%logical1 ",trim(string)
        endif
        stop
    endif

case ('integer1', 'group2%integer1')
    read(string, *, iostat=io) params%integer1
    if (VERBOSE .or. io/=0) write(*,*) "group2%integer1 = ", params%integer1

    if (io /= 0) then
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --group2%integer1"
        else
            write(*,*) "ERROR converting string to integer(kind=ip) :: & 
& integer1: --group2%integer1 ",trim(string)
        endif
        stop
    endif

case ('integer2', 'group2%integer2')
    read(string, *, iostat=io) params%integer2
    if (VERBOSE .or. io/=0) write(*,*) "group2%integer2 = ", params%integer2

    if (io /= 0) then
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --group2%integer2"
        else
            write(*,*) "ERROR converting string to integer(kind=ip) :: & 
& integer2: --group2%integer2 ",trim(string)
        endif
        stop
    endif

case ('string2', 'group2%string2')
    params%string2 = trim(string)
    if (VERBOSE) write(*,*) "group2%string2 = ", params%string2

case ('intarr1', 'group2%intarr1')
    call string_to_array(string, params%intarr1, iostat=io)
    if (VERBOSE) write(*,*) "group2%intarr1 = ", params%intarr1(1),', ...'

    if (io /= 0) then
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --group2%intarr1"
        else
            write(*,*) "ERROR converting string to integer(kind=ip), & 
& dimension(7) :: intarr1: --group2%intarr1 ",trim(string)
        endif
        stop
    endif

case ('double1', 'group2%double1')
    read(string, *, iostat=io) params%double1
    if (VERBOSE .or. io/=0) write(*,*) "group2%double1 = ", params%double1

    if (io /= 0) then
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --group2%double1"
        else
            write(*,*) "ERROR converting string to real(kind=dp) :: double1: & 
& --group2%double1 ",trim(string)
        endif
        stop
    endif

case ('dblarr1', 'group2%dblarr1')
    call string_to_array(string, params%dblarr1, iostat=io)
    if (VERBOSE) write(*,*) "group2%dblarr1 = ", params%dblarr1(1),', ...'

    if (io /= 0) then
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --group2%dblarr1"
        else
            write(*,*) "ERROR converting string to real(kind=dp), dimension(5) & 
& :: dblarr1: --group2%dblarr1 ",trim(string)
        endif
        stop
    endif

case ('logarr1', 'group2%logarr1')
    call string_to_array(string, params%logarr1, iostat=io)
    if (VERBOSE) write(*,*) "group2%logarr1 = ", params%logarr1(1),', ...'

    if (io /= 0) then
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --group2%logarr1"
        else
            write(*,*) "ERROR converting string to logical, dimension(5) :: & 
& logarr1: --group2%logarr1 ",trim(string)
        endif
        stop
    endif

    case default
      write(*,*) "ERROR::ioparams::group2: &
        & unknown member :: ", trim(name)
      if (present(iostat)) then
        iostat = -1
      else
        stop
      endif
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


subroutine parse_command_args_control (params, args, unmatched, &
    & stop_on_help, iostat)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Maybe assign ith command line argument to the params type
    ! Input:
    !   params : the paramter type
    ! Output:
    !   iostat : integer, optional
    !        0 : all arguments were found
    !       >0 : number of unmatched arguments
    !       -1 : error when reading
    !       -2 : --help was printed
    !       If not provided, execution stop if iostat /= 0
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(control_t), intent(inout) :: params
    integer, intent(out), optional :: iostat
    character(len=*), dimension(:), intent(in), optional :: args
    character(len=*), dimension(:), allocatable, intent(inout), optional :: & 
& unmatched
    character(len=clen), dimension(:), allocatable :: args_opt, unmatched_opt
    logical, intent(in), optional :: stop_on_help
    character(len=clen) :: argn, argv
    logical :: missing_value, stop_on_help_opt

    integer :: i,j, n, parsed

    if (present(stop_on_help)) then
      stop_on_help_opt = stop_on_help
    else
      stop_on_help_opt = .true.
    endif

    ! Define a list of command line arguments args_opt
    if (present(args)) then
      ! ... provided as subroutine argument
      args_opt = args
    else
      ! ... provided as command-line argument
      call command_argument_as_array(args_opt)
    endif

    n = size(args_opt)

    if (allocated(unmatched_opt)) deallocate(unmatched_opt)
    allocate(unmatched_opt(n))

    parsed = 0

    i = 1
    j = 1
    do while (i <= n)
      argn = args_opt(i)

      ! Print HELP ?
      if (argn == '--help' .or. argn=='-h') then
        call print_help_control(params)
        if (present(iostat)) then
          iostat = -2
          return
        elseif (stop_on_help_opt) then
          stop('End of help message : exit')
        endif
      endif

      if (argn(1:2)  /= "--") then
        if (.not.present(iostat) .and. .not.present(unmatched)) then
          write(*,*) "i=",i, "; Got: ",trim(argn)
          stop("ERROR::ioparams type-specific command line &
            & arguments must start with '--'")
        else
          unmatched_opt(j) = trim(argn)
          j = j + 1
          i = i + 1  ! check next argument
          cycle
        endif
      endif

      if (has_param_control(params, trim(argn(3:)))) then
        ! +++++  present

        ! only "--name value" is tolerated
        ! check if value is missing
        missing_value = .false.
        if (i+1 <= n) then
          argv = args_opt(i+1)
          if ( len(argv) > 1) then
            ! ...next argument starts with '--'
            if (argv(1:2)  == "--" ) then
              missing_value = .true.
            endif
          endif
        else
          ! ...end of command line
          missing_value = .true.
        endif

        if (missing_value) then
          write(*,*) "ERROR::ioparams::control: &
            & missing value for "//trim(argn(3:))
          if (present(iostat)) then
            iostat = -1
            return
          else
            stop
          endif
        endif

        call set_param_string_control(params, trim(argn(3:)), trim(argv), &
          & iostat=iostat)

        if (present(iostat)) then
          if (iostat /= 0) then
            return  ! error
          endif
        endif

        parsed = parsed + 2
        i = i + 2

      else
        ! +++++  not found

        if (.not. present(iostat) .and. .not. present(unmatched)) then
          write(*,*) "ERROR: unknown parameter in control : ",trim(argn)
          write(*,*) ""
          write(*,*) "-h or --help for HELP"
          stop
        endif

        unmatched_opt(j) = trim(argn)
        j = j + 1
        i = i + 1

      endif

    enddo

    ! At this point, any type error or --help message cases are already sorted
! out
    if (present(iostat)) then
      iostat = n - parsed
    endif

    if (present(unmatched)) then
      unmatched = unmatched_opt(:n-parsed)
    endif

    deallocate(args_opt, unmatched_opt)

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
  character(len=2000) :: nameshort
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
  write(io, *) "+++++++++++++++++      control      ++++++++++++++++++"
  write(io, *) " "

write(nameshort, *) "print_nml"
if (def) then
    write(valuestr, *) params%print_nml
    write(valuelen, *) max(1,len(trim(adjustl(valuestr))))
    write(io, '("  --",A20,"print read namelist to string? (default: & 
& ",A'//trim(valuelen)//',")")') adjustl(nameshort), trim(adjustl(valuestr))
else
    write(io, '("  --",A20,"print read namelist to string?")') & 
& adjustl(nameshort)
endif

end subroutine

subroutine set_param_string_control (params, name, string, iostat)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the control type from a string argument
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(control_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: string
    integer, intent(out), optional :: iostat
    integer :: io !! local...

    if (present(iostat)) then
      iostat = 0
    endif

    select case (name)

case ('print_nml', 'control%print_nml')
    read(string, *, iostat=io) params%print_nml
    if (VERBOSE .or. io/=0) write(*,*) "control%print_nml = ", & 
& params%print_nml

    if (io /= 0) then
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for & 
& --control%print_nml"
        else
            write(*,*) "ERROR converting string to logical :: print_nml: & 
& --control%print_nml ",trim(string)
        endif
        stop
    endif

    case default
      write(*,*) "ERROR::ioparams::control: &
        & unknown member :: ", trim(name)
      if (present(iostat)) then
        iostat = -1
      else
        stop
      endif
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



! Interface that covers all derived types and the type
! of contained variables to generically set or get parameter values:
!
! call set_param(params_type, name, value)
! call get_param(params_type, name, value)
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
& character(len=*) :: ",trim(name)
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
& character(len=*) :: ",trim(name)
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
& character(len=*), dimension(:) :: ",trim(name)
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
& character(len=*), dimension(:) :: ",trim(name)
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
& integer(kind=ip) :: ",trim(name)
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
& integer(kind=ip) :: ",trim(name)
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
          write(*,*) "ERROR set_param for group1: unknown type member: logical & 
& :: ",trim(name)
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
          write(*,*) "ERROR get_param for group1: unknown type member logical & 
& :: ",trim(name)
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
& character(len=*) :: ",trim(name)
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
& character(len=*) :: ",trim(name)
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
& character(len=*), dimension(:) :: ",trim(name)
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
& character(len=*), dimension(:) :: ",trim(name)
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
& integer(kind=ip) :: ",trim(name)
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
& integer(kind=ip) :: ",trim(name)
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
& integer(kind=ip), dimension(:) :: ",trim(name)
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
& integer(kind=ip), dimension(:) :: ",trim(name)
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
          write(*,*) "ERROR set_param for group2: unknown type member: logical & 
& :: ",trim(name)
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
          write(*,*) "ERROR get_param for group2: unknown type member logical & 
& :: ",trim(name)
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
& logical, dimension(:) :: ",trim(name)
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
          write(*,*) "ERROR get_param for group2: unknown type member logical, & 
& dimension(:) :: ",trim(name)
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
& real(kind=dp) :: ",trim(name)
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
& real(kind=dp) :: ",trim(name)
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
& real(kind=dp), dimension(:) :: ",trim(name)
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
& real(kind=dp), dimension(:) :: ",trim(name)
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
          write(*,*) "ERROR set_param for control: unknown type member: & 
& logical :: ",trim(name)
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
          write(*,*) "ERROR get_param for control: unknown type member logical & 
& :: ",trim(name)
            stop
    end select
end subroutine




end module ioparams

