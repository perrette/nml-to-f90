subroutine read_nml_{group_name} (iounit, params)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Read the {group_name} group in a namelist file and assign to type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    integer, intent(in) :: iounit
    type({type_name}), intent(inout) :: params

    ! Calls to nml_real
    ! call nml_read("","group1","name1",group1%name1))
{list_of_nml_read_calls}

end subroutine

subroutine write_nml_{group_name} (iounit, params)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Read the {group_name} group in a namelist file and assign to type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    integer, intent(in) :: iounit
    type({type_name}), intent(inout) :: params

    write(iounit,*) "&{group_name}"
{list_of_nml_write_calls}
    write(iounit,*) "/"

end subroutine
