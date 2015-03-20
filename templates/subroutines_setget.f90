subroutine set_param_{group}_{vtype_name} (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the {group} type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type({type_name}), intent(inout) :: params
    character(len=*), intent(in) :: name
    {vtype}, intent(in) :: value

    select case (name) 
        {list_set_cases}
        case default
        write(*,*) "ERROR set_param for {group}: unknown type member: {vtype} :: ",trim(name)
            stop
    end select
end subroutine

subroutine get_param_{group}_{vtype_name} (params, name, value)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the {group} type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type({type_name}), intent(inout) :: params
    character(len=*), intent(in) :: name
    {vtype}, intent(out) :: value

    select case (name) 
        {list_get_cases}
        case default
            write(*,*) "ERROR get_param for {group}: unknown type member {vtype} :: ",trim(name)
            stop
    end select
end subroutine
