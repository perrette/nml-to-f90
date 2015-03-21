subroutine set_param_string_{group} (params, name, string)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the {group} type from a string argument
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type({type_name}), intent(inout) :: params
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: string
    integer :: iostat

    select case (name) 
    {list_set_cases}
    case default
      write(*,*) "ERROR set_param_string for {group}: unknown member :: ",trim(name)
      stop
    end select
end subroutine

function has_param_{group} (params, name) result(has_param)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Test whether an attribute name is member of the type {group}
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

subroutine parse_command_line_{group} (params,i, iostat)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Maybe set command line argument field
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type({type_name}), intent(inout) :: params
    integer, intent(inout) :: i
    integer, optional :: iostat
    character(len=512) :: arg, argv

    call get_command_argument(i, arg)

    if (has_param_{group}(params, trim(arg(3:)))) then
      ! +++++  present 
      call get_command_argument(i+1, argv)
      call set_param_string_{group}(params, trim(arg(3:)), trim(argv))
      i = i+1
      if (present(iostat)) then
        iostat = 0
      endif
    else
      ! +++++  no found
      if (present(iostat)) then
        iostat=1
      else
        write(*,*) "ERROR: unknown parameter in {group} : --",trim(arg(3:))
        stop
      endif
    endif
end subroutine
