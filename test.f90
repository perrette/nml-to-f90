program test_io_params

  use ioparams, only: read_nml, write_nml, set_param, get_param, has_param, set_param_string
  use ioparams, only: group1_t, group2_t

  implicit none 

  type(group1_t) :: group1 
  type(group2_t) :: group2 
  character(len=256) :: filename, filename2
  integer :: iounit = 88
  double precision :: test_dp
  logical :: test_l
  integer :: test_int, test_int_arr(10)
  character(len=50) :: test_s

  integer :: i
  character(len=256) :: arg, argv

  filename = "namelist.nml" 

  ! Read parameters from file
  open(iounit, file=filename, status="OLD")
  write(*,*) "READ from ",trim(filename)
  write(*,*) "group1 ==========="
  call read_nml(iounit, group1)
  write(*,*) "group2 ==========="
  call read_nml(iounit, group2)
  close(iounit)

  write(*,*) ""
  write(*,*) " +++++++++++++++++++"
  write(*,*) " Check set/get_param"
  write(*,*) " +++++++++++++++++++"

  call get_param(group2, "string1", test_s)
  call set_param(group2, 'string1', "this is a new string set via set_param")
  write(*,*) "group2, string1:", trim(test_s), " ==> ", trim(group2%string1), " (this is ...)"

  call get_param(group2, "double1", test_dp)
  call set_param(group2, 'double1', 11111111111.11111111111d0)
  write(*,*) "group2, double1:", test_dp, " ==> ", group2%double1, " (111...)"

  call get_param(group2, 'integer1', test_int)
  call set_param(group2, 'integer1', 777777777)
  write(*,*) "group2, integer1:", test_int, " ==> ", group2%integer1, " (777..)"

  call get_param(group2, "intarr1", test_int_arr)
  call set_param(group2, "intarr1", [7,6,5,4,3,2,1])
  write(*,*) "group2, intarr1:", test_int_arr, " ==> ", group2%intarr1, " (7,6,5...)"

  call get_param(group2, "logical1", test_l)
  call set_param(group2, "logical1", .false.)
  write(*,*) "group2, longical1:", test_l, " ==> ", group2%logical1, " (false)"

  write(*,*) " "
  write(*,*) "+++++++++++++++++++++++++++++++++++++++++++"
  write(*,*) " has_param"
  write(*,*) "+++++++++++++++++++++++++++++++++++++++++++"

  write(*,*) " has_param(group1, 'integer1') ? ", has_param(group1, 'integer1')
  write(*,*) " has_param(group1, 'thisdoesnotexists') ? ", has_param(group1, 'thisdoesnotexists')
  write(*,*) 
  write(*,*) "+++++++++++++++++++++++++++++++++++++++++++"
  write(*,*) " set_param_string"
  write(*,*) "+++++++++++++++++++++++++++++++++++++++++++"
  write(*,*) 
  call get_param(group2, "string1", test_s)
  call set_param_string(group2, 'string1', "another string")
  write(*,*) "group2, string1: ", trim(test_s), " ==> ", trim(group2%string1), " (another ...)"

  call get_param(group2, "double1", test_dp)
  call set_param_string(group2, 'double1', "3333.44444")
  write(*,*) "group2, double1: ", test_dp, " ==> ", group2%double1, " (3333.4444)"

  call get_param(group2, 'integer1', test_int)
  call set_param_string(group2, 'integer1', "3333")
  write(*,*) "group2, integer1: ", test_int, " ==> ", group2%integer1, " (3333)"
  if (group2%integer1 /= 3333) write(*,*) "Error set_param_string integer"

  call get_param(group2, "intarr1", test_int_arr)
  call set_param_string(group2, "intarr1", "(/ 7,7,7,7,7,7,7 /)")
  write(*,*) "group2, intarr1:", test_int_arr, " ==> ", group2%intarr1, " (7,7,7...)"

  call get_param(group2, "logical1", test_l)
  call set_param_string(group2, "logical1", ".true.")
  write(*,*) "group2, logical1: ", test_l, " ==> ", group2%logical1, " (true)"

  !
  ! Retrieve command-line arguments
  !
  write(*,*) "+++++++++++++++++++++++++++++++++++++++++++"
  write(*,*) "Now check command line parameters  "
  write(*,*) "+++++++++++++++++++++++++++++++++++++++++++"

  write(*,*) "E.g. call this script with --group1%double1 -99"
  i = 1
  do while(i <= command_argument_count())
    call get_command_argument(i, arg)
    select case (arg)
    case ('-h', '--help')
      print*, "Just pass argument as '--double1 3.14' or, for ambiguous cases, '--group1%integer1 44'"
    case default
      arg = arg(3:) !  remove heading --
      if (has_param(group1, arg)) then
        call get_command_argument(i+1, argv)
        write(*,*) "param "//trim(arg)//" found in group1, set value", argv
        call set_param_string(group1, arg, argv)
        i = i+1
      elseif (has_param(group2, arg)) then
        call get_command_argument(i+1, argv)
        call set_param_string(group2, arg, argv)
        write(*,*) "param "//trim(arg)//" found in group1, set value ", argv
        i = i+1
      else
        write(*,*) "WARNING: unknown parameter ",trim(arg)
      endif
    end select
    i = i+1
  end do

  ! Print namelist to screen
  write(*,*) "Write to namelist_check.nml"
  open(88, file="namelist_check.nml", action="WRITE")
  call write_nml(88, group1)
  call write_nml(88, group2)
  close(88)
  write(*,*) "Done."


contains
end program
