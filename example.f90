program example
  use ioparams, only: group1_t, control_t, &
    read_nml, write_nml, &
    parse_command_argument, print_help

  implicit none
  type(group1_t) :: par1
  type(control_t) :: ctr
  integer :: i, iostat, parsed
  character(len=256) :: arg

  ! read namelist
  open(88, file="namelist.nml")
  call read_nml(88, par1)
  call read_nml(88, ctr)
  close(88)

  ! parse command-line arguments
  parsed = 0
  call parse_command_argument(par1, iostat)
  parsed = parsed + check_iostat(iostat)
  call parse_command_argument(ctr, iostat)
  parsed = parsed + check_iostat(iostat)

  if (iostat /= -2 .and. parsed < command_argument_count()) then
    write(*,*) "Not all arguments were parsed"
    stop
  endif

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

contains

    integer function check_iostat(iostat)
      integer, intent(in) :: iostat
      if (iostat == -1) then
        write(*,*) "ERROR when parsing command-line param. Try -h or --help"
        stop
      else if (iostat > 0) then
        check_iostat = iostat
      else
        check_iostat = 0
      endif
    end function

end program
