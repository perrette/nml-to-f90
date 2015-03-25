subroutine read_nml_{group_name} (iounit, params)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Read the {group_name} group in a namelist file and assign to type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    integer, intent(in) :: iounit
    type({type_name}), intent(inout) :: params

    {variable_definitions}

    namelist / {group_name} / {list_of_variables}

    ! initialize variables
    {assign_namelist}

    ! read all
    read(unit=iounit, nml={group_name}) 

    ! assign back to type
    {assign_type}
end subroutine

subroutine write_nml_{group_name} (iounit, params)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Read the {group_name} group in a namelist file and assign to type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    integer, intent(in) :: iounit
    type({type_name}), intent(inout) :: params

    {variable_definitions}

    namelist / {group_name} / {list_of_variables}

    ! initialize variables
    {assign_namelist}

    ! write_all
    write(unit=iounit, nml={group_name}) 
end subroutine