subroutine parse_command_argument_{group_name} (params,i, iostat, arg)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Maybe assign ith command line argument to the params type
    ! Input:
    !   params : the paramter type
    !   i : integer, positional argument of the parameter name
    !       is incremented if parameter is found
    ! Output:
    !   iostat : integer, optional
    !       -1 : --help was printed
    !       0  : param found
    !       1  : error when reading
    !       If not provided, execution stop if iostat /= 0
    !   arg : character, optional : the ith command line argument
    !       as returned by native get_command_argument(i, arg)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type({type_name}), intent(inout) :: params
    integer, intent(inout) :: i
    integer, optional :: iostat
    character(len=*), optional :: arg
    character(len=512) :: argn, argv

    call get_command_argument(i, argn)

    if (present(arg)) arg = trim(argn)

    ! Print HELP ?
    if (argn == '--help' .or. argn=='-h') then
      call print_help_{group_name}(params)
      if (present(iostat)) then
        iostat = -1
      else
        stop
      endif
      return
    endif

    if (argn(1:2)  /= "--") then
      print*, "i=",i, "; Got: ",trim(argn)
      stop("ERROR: type-specific command line &
        arguments must start with '--'")
    endif

    if (has_param_{group_name}(params, trim(argn(3:)))) then
      ! +++++  present 
      call get_command_argument(i+1, argv)
      call set_param_string_{group_name}(params, trim(argn(3:)), trim(argv))
      i = i+2
      if (present(iostat)) then
        iostat = 0
      endif
    else
      ! +++++  no found
      if (present(iostat)) then
        iostat = 1
      else
        write(*,*) "ERROR: unknown parameter in {group_name} : ",trim(argn)
        write(*,*) ""
        write(*,*) "-h or --help for HELP"
        stop
      endif
    endif
end subroutine

subroutine print_help_{group_name}(params, iounit, default)
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Print HELP on a derived type, useful for command-line arg
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  type({type_name}), intent(in) :: params
  integer, optional :: iounit
  integer :: io
  logical, optional :: default
  logical :: def
  character(len=2000) :: valuestr
  character(len=20) :: valuelen
  character(len=20) :: nameshort
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
  write(io, *) "+++++++++++++++++      {group_name}      ++++++++++++++++++"
  write(io, *) " "
  {list_help}
end subroutine

subroutine set_param_string_{group_name} (params, name, string)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the {group_name} type from a string argument
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type({type_name}), intent(inout) :: params
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: string
    integer :: iostat

    select case (name) 
    {list_set_cases}
    case default
      write(*,*) "ERROR set_param_string for {group_name}: unknown member :: ",trim(name)
      stop
    end select
end subroutine

function has_param_{group_name} (params, name) result(has_param)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Test whether an attribute name is member of the type {group_name}
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type({type_name}), intent(inout) :: params
    character(len=*), intent(in) :: name
    logical :: has_param

    has_param = .true.
    select case (name) 
      {list_has_cases}
    case default
      has_param = .false.
    end select
end function

