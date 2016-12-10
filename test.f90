program test_io_params

  use ioparams, only: group1_t, group2_t
  use ioparams, only: read_nml, write_nml
  use ioparams, only: set_param, get_param
  use ioparams, only: parse_command_args, print_help
  use ioparams, only: has_param, set_param_string ! low-level

  implicit none 

  type(group1_t) :: group1 
  type(group2_t) :: group2 
  character(len=256) :: filename, filename2
  integer :: iounit = 88
  double precision :: test_dp
  logical :: test_l
  integer :: test_int, test_int_arr(7)
  character(len=50) :: test_s
  character(len=50) :: args(14)
  character(len=50), allocatable :: unmatched(:)

  integer :: i, parsed, io, iostat, iostats(2), stat
  character(len=256) :: arg, argv

  filename = "namelist.nml" 

  write(*,*) " "
  write(*,*) "++++++++++++++++++++++++++++++++++++"
  write(*,*) "Testing nml2f90.py"
  write(*,*) "++++++++++++++++++++++++++++++++++++"
  write(*,*) " "
  write(*,*) "Test reading namelist"
  write(*,*) "---------------------"
  ! Read parameters from file
  write(*,*) "READ from ",trim(filename)
  open(iounit, file=filename, status="OLD")
  write(*,*) "...group1..."
  call read_nml(iounit, group1)
  write(*,*) "...group2..."
  call read_nml(iounit, group2)
  close(iounit)
  write(*,*) "Done. Namelist read successfully."
  write(*,*) " "
  write(*,*) "Test set_param / get_param "
  write(*,*) "---------------------------"
  call get_param(group2, "string1", test_s)
  call set_param(group2, 'string1', "this is a new string set via set_param")
  write(*,*) "group2%string1:", trim(test_s), " ==> ", trim(group2%string1), " (this is ...)"

  call get_param(group2, "double1", test_dp)
  call set_param(group2, 'double1', 11111111111.11111111111d0)
  write(*,*) "group2%double1:", test_dp, " ==> ", group2%double1, " (111...)"

  call get_param(group2, 'integer1', test_int)
  call set_param(group2, 'integer1', 777777777)
  write(*,*) "group2%integer1:", test_int, " ==> ", group2%integer1, " (777..)"

  call get_param(group2, "intarr1", test_int_arr)
  call set_param(group2, "intarr1", [7,6,5,4,3,2,1])
  write(*,*) "group2%intarr1:", test_int_arr, " ==> ", group2%intarr1, " (7,6,5...)"

  call get_param(group2, "logical1", test_l)
  call set_param(group2, "logical1", .false.)
  write(*,*) "group2%longical1:", test_l, " ==> ", group2%logical1, " (false)"

  write(*,*) " "
  write(*,*) "Test has_param"
  write(*,*) "--------------"

  write(*,*) "has_param(group1, 'integer1') ? ", has_param(group1, 'integer1')
  write(*,*) "has_param(group1, 'thisdoesnotexists') ? ", has_param(group1, 'thisdoesnotexists')
  write(*,*) 
  write(*,*) "Test set_param_string"
  write(*,*) "---------------------"
  call set_param_string(group2, 'string1', "another string")
  call set_param_string(group2, 'double1', "3333.44444")
  call set_param_string(group2, 'integer1', "3333")
  call set_param_string(group2, "intarr1", "(/ 7,7,7,7,7,7,7 /)")
  call set_param_string(group2, "logical1", ".true.")

  write(*,*) 
  write(*,*) "Test command-line arguments passed as string array"
  write(*,*) "--------------------------------------------------"
  args(1) = "--string1"
  args(2) = "newtext.txt"
  args(3) = "--logical1"
  args(4) = "F"
  args(5) = "--integer1"
  args(6) = "2"
  args(7) = "--double1"
  args(8) = "777"
  args(9) = "--stringarr1"
  args(10) = "bb,cc,dd"
  args(11) = "--intarr1"
  args(12) = "1, 2, 3, 4, 5, 6, 7"
  args(13) = "--dblarr1"
  args(14) = "5.,4,3,2,1"
  call parse_command_args(group1, stop_on_help=.false., args=args, unmatched=unmatched)
  call parse_command_args(group2, args=unmatched) !, unmatched=unmatched)
  call assert_cmd("group1%string1", "newtext.txt"== group1%string1)
  call assert_cmd("group1%logical1", .false. .eqv. group1%logical1)
  call assert_cmd("group1%integer1", 2== group1%integer1)
  call assert_cmd("group2%double1", 777.d0== group2%double1)
  call assert_cmd("group1%stringarr1(2)", "cc"== group1%stringarr1(2))
  call assert_cmd("group2%intarr1(3)", 3== group2%intarr1(3))
  call assert_cmd("group2%dblarr1(1)", 5.d0== group2%dblarr1(1))

  !
  ! Retrieve command-line arguments
  !
  write(*,*) " "
  write(*,*) "Test command line parameters  "
  write(*,*) "----------------------------- "
  write(*,*) "Type ./test.x -h for help on how to do that."
  i = 1
  io=0
  parsed = 0
  call parse_command_args(group1, unmatched=unmatched, stop_on_help=.false.)
  call parse_command_args(group2, args=unmatched)

  ! Print namelist to screen
  write(*,*) " "
  write(*,*) "Test writing to namelist_check.nml "

  open(88, file="namelist_check.nml", action="WRITE")
  call write_nml(88, group1)
  call write_nml(88, group2)
  close(88)
  write(*,*) "Done. All tests terminated successfully"


contains

  subroutine assert_cmd(nm, test)
    character(len=*), intent(in) :: nm
    logical, intent(in) :: test
    if (.not.test) then
      write(*,*) "ERROR: failed cmd",nm
      stop
    endif
  end subroutine
end program
