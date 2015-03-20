program generate_io
  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Write a namelist template to be read-in from python
  ! ++++++++++++++++++++++++++++++++++++++++++++++++++++
  use params, only: pars_group1, pars_group2
  call write_nml_template("namelist.template.nml")

contains

  subroutine write_nml_template(filename)
    ! Dummy namelist, where the group is in front of % instead 
    ! of being in the normal group
    character(len=*), intent(in) :: filename
    type(pars_group1) :: group1
    type(pars_group2) :: group2
    integer :: iounit = 88, i
    namelist /allparams/ group1, group2
    open(iounit, file=filename, action="write")
    write(iounit, allparams)
    close(iounit)
    ! call write_nml_pars_group1(iounit, par1)
    ! call write_nml_pars_group1(iounit, par2)
  end subroutine


end program
