program example
  use ioparams, only: group1_t, control_t, &
    read_nml, write_nml, command_argument_as_array, &
    parse_command_args, print_help, join_array

  implicit none
  type(group1_t) :: par1
  type(control_t) :: ctr
  integer :: i, parsed
  character(len=256) :: arg
  character(len=256), allocatable :: args(:)

  ! read namelist
  open(88, file="namelist.nml")
  call read_nml(88, par1)
  call read_nml(88, ctr)
  close(88)

  ! parse command-line arguments
  call command_argument_as_array(args)
  call parse_command_args(par1, args=args, unmatched=args, stop_on_help=.false.)
  call parse_command_args(ctr, args=args, unmatched=args)

  if (size(args) > 0) then
    write(*,*) "Some arguments were not matched: "
    write(*,*) trim(join_array(args))
    stop
  endif

  if (ctr%print_nml) then
    write(*,*) "-------------------------------------"
    write(*,*) "Print namelist : "
    write(*,*) "-------------------------------------"
    call write_nml(6, par1)
    write(*,*) "-------------------------------------"
  endif
  write(*,*) "Example program. Call with -h flag to get help on existing parameters."

end program
