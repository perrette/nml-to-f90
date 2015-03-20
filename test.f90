program test_io_params

  use params, only: pars_group1, pars_group2
  use ioparams, only: read_nml, write_nml, set_param, get_param

  implicit none 

  type(pars_group1) :: group1 
  type(pars_group2) :: group2 
  character(len=256) :: filename, filename2
  integer :: iounit = 88
  double precision :: test_dp
  integer :: test_int, test_int_arr(10)

  filename = "namelist.nml" 

  ! Read parameters from file
  open(iounit, file=filename, status="OLD")
  write(*,*) "READ from ",trim(filename)
  write(*,*) "group1 ==========="
  call read_nml(iounit, group1)
  write(*,*) "group2 ==========="
  call read_nml(iounit, group2)
  close(iounit)

  ! Modify type
  call set_param(group1, 'string1', "this is a new string set via set_param")
  call set_param(group2, 'string2', "another string using set_param")
  call set_param(group2, 'integer1', 777777777)
  call set_param(group2, 'double1', 11111111111.11111111111d0)
  call set_param(group2, 'logical1', .true.)

  ! Print namelist to screen
  call write_nml(6, group1)
  call write_nml(6, group2)

  write(*,*) ""
  write(*,*) " Uses of get_param"
  write(*,*) " +++++++++++++++++"
  write(*,*) " "
  call get_param(group2, "double1", test_dp)
  call get_param(group2, "integer1", test_int)
  call get_param(group2, "intarr1", test_int_arr)
  write(*,*) "double1 (set to 1111..)", test_dp
  write(*,*) "integer1 (set to 77...)", test_int
  write(*,*) "intarr1", test_int_arr

contains
end program
