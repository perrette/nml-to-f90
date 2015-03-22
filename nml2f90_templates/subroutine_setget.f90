subroutine set_param_{block_name}_{type_interface} (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the {block_name} type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type({type_name}), intent(inout) :: params
    character(len=*), intent(in) :: name
    {type}, intent(in) :: value

    select case (name) 
        {list_set_cases}
        case default
          write(*,*) "ERROR set_param for {block_name}: unknown type member: {type} :: ",trim(name)
            stop
    end select
end subroutine

subroutine get_param_{block_name}_{type_interface} (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the {block_name} type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type({type_name}), intent(inout) :: params
    character(len=*), intent(in) :: name
    {type}, intent(out) :: value

    select case (name) 
        {list_get_cases}
        case default
          write(*,*) "ERROR get_param for {block_name}: unknown type member {type} :: ",trim(name)
            stop
    end select
end subroutine
