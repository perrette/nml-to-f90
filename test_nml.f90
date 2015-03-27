!+++++++++++++++++++++++++++++++++++++++++++++++++++
! Test programe to make use of Alex' nml.f90 module
!+++++++++++++++++++++++++++++++++++++++++++++++++++
program example
  use ioparams, only: group1_t, control_t, &
    read_nml, write_nml, &
    parse_command_argument, print_help

  implicit none
  type(group1_t) :: par1
  type(control_t) :: ctr
  integer :: i, iostat
  character(len=256) :: arg

  ! read namelist
  open(88, file="namelist.nml")
  call read_nml(88, par1)
  call read_nml(88, ctr)
  close(88)

  ! parse command-line arguments
  i = 1
  do while(i <= command_argument_count())
    call parse_command_argument(par1, i, iostat)
    if (iostat==0) continue
    call parse_command_argument(ctr, i, iostat, arg=arg)
    if (iostat/=0) then
      write(*,'("Parameter not found: ",A20,". Type -h for help")') trim(arg)
      stop
    endif
    if (arg == "--help".or.arg == "-h") stop
    i = i + 2
  enddo

  write(*,*) 
  if (ctr%print_nml) then
    write(*,*) "-------------------------------------"
    write(*,*) "Print namelist : "
    write(*,*) "-------------------------------------"
    call write_nml(6, par1)
    write(*,*) "-------------------------------------"
  endif
  write(*,*) 
  write(*,*) "Nice ! Call with -h flag to get help on existing parameters."
  write(*,*) 

end program
