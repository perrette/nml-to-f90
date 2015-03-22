subroutine parse_command_argument_{block_name} (params,i, iostat)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Maybe assign ith command line argument to the params type
    ! Input:
    !   params : the paramter type
    !   i : integer, positional argument of the parameter name
    ! Output:
    !   iostat, optional : 0 if success, 1 otherwise (if not
    !       provided, an error is thrown in case of failure)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type({type_name}), intent(inout) :: params
    integer, intent(inout) :: i
    integer, optional :: iostat
    character(len=512) :: arg, argv

    call get_command_argument(i, arg)

    ! Print HELP ?
    if (arg == '--help' .or. arg=='-h') then
      call print_help_{block_name}(params)
      return
    endif

    if (has_param_{block_name}(params, trim(arg(3:)))) then
      ! +++++  present 
      call get_command_argument(i+1, argv)
      call set_param_string_{block_name}(params, trim(arg(3:)), trim(argv))
      i = i+1
      if (present(iostat)) then
        iostat = 0
      endif
    else
      ! +++++  no found
      if (present(iostat)) then
        iostat=1
      else
        write(*,*) "ERROR: unknown parameter in {block_name} : --",trim(arg(3:))
        write(*,*) ""
        write(*,*) "-h or --help for HELP"
        stop
      endif
    endif
end subroutine

subroutine print_help_{block_name}(params, iounit, value)
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Print HELP on a derived type, useful for command-line arg
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  type({type_name}), intent(in) :: params
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
  write(io, *) "+++++++++++++++++      {block_name}      ++++++++++++++++++"
  {list_help}
end subroutine

subroutine set_param_string_{block_name} (params, name, string)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the {block_name} type from a string argument
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type({type_name}), intent(inout) :: params
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: string
    integer :: iostat

    select case (name) 
    {list_set_cases}
    case default
      write(*,*) "ERROR set_param_string for {block_name}: unknown member :: ",trim(name)
      stop
    end select
end subroutine

function has_param_{block_name} (params, name) result(has_param)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Test whether an attribute name is member of the type {block_name}
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

