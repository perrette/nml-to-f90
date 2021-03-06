subroutine read_nml_{group_name} (iounit, params, iostat)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Read the {group_name} group in a namelist file and assign to type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    integer, intent(in) :: iounit
    type({type_name}), intent(inout) :: params
    integer, optional, intent(out) :: iostat

    {variable_definitions}

    namelist / {group_name} / {list_of_variables}

    ! initialize variables
    {assign_namelist}

    ! read all
    if (.not. present(iostat)) then
      read(unit=iounit, nml={group_name})
    else
      read(unit=iounit, nml={group_name}, iostat=iostat)
      if (iostat /= 0 .and. VERBOSE) then
        write(*, *) "Failed to read namelist block: {group_name}"
      endif
    endif


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

subroutine read_nml_file_{group_name} (file, params, iostat)
    character(len=*), intent(in) :: file
    type({type_name}), intent(inout) :: params
    integer, optional, intent(out) :: iostat
    integer :: unit = 4563
    open(file=file, unit=unit, status='OLD')
    call read_nml_{group_name} (unit, params, iostat)
    close(unit)
end subroutine
