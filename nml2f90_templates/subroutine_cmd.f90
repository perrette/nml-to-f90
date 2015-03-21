subroutine parse_command_argument_{group} (params,i, iostat)
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

