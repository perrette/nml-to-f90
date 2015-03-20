subroutine read_nml_{group} (iounit, params)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Read the {group} block in a namelist file and assign to type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    integer, intent(in) :: iounit
    type({type_name}), intent(inout) :: params

    {variable_definitions}

    namelist / {group} / {list_of_variables}

    ! initialize variables
    {list_of_init}

    ! read all
    read(unit=iounit, nml={group}) 

    ! assign back to type
    {list_of_assign}
end subroutine

subroutine write_nml_{group} (iounit, params)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Read the {group} block in a namelist file and assign to type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    integer, intent(in) :: iounit
    type({type_name}), intent(inout) :: params

    {variable_definitions}

    namelist / {group} / {list_of_variables}

    ! initialize variables
    {list_of_init}

    ! write_all
    write(unit=iounit, nml={group}) 
end subroutine
