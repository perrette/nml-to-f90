subroutine parse_command_args_{group_name} (params, iostat, unmatched, args, &
    & stop_on_help)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Maybe assign ith command line argument to the params type
    ! Input:
    !   params : the paramter type
    ! Output:
    !   iostat : integer, optional
    !        0 : all arguments were found
    !       >0 : number of unmatched arguments
    !       -1 : error when reading
    !       -2 : --help was printed
    !       If not provided, execution stop if iostat /= 0
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type({type_name}), intent(inout) :: params
    integer, intent(out), optional :: iostat
    character(len=*), dimension(:), intent(in), optional :: args
    character(len=*), dimension(:), allocatable, intent(inout), optional :: unmatched
    character(len=clen), dimension(:), allocatable :: args_opt, unmatched_opt
    logical, intent(in), optional :: stop_on_help
    character(len=clen) :: argn, argv
    logical :: missing_value, stop_on_help_opt

    integer :: i,j, n, parsed

    if (present(stop_on_help)) then
      stop_on_help_opt = stop_on_help
    else
      stop_on_help_opt = .true.
    endif

    ! Define a list of command line arguments args_opt
    if (present(args)) then
      ! ... provided as subroutine argument
      n = size(args)
      args_opt = args
    else
      ! ... provided as command-line argument
      n = command_argument_count()
      if (allocated(args_opt)) deallocate(args_opt) 
      allocate(args_opt(n))
      do i=1,n
          call get_command_argument(i, args_opt(i))
      enddo
    endif

    if (allocated(unmatched_opt)) deallocate(unmatched_opt)
    allocate(unmatched_opt(n))

    parsed = 0

    i = 1
    j = 1
    do while (i <= n)
      argn = args_opt(i)

      ! Print HELP ?
      if (argn == '--help' .or. argn=='-h') then
        call print_help_{group_name}(params)
        if (present(iostat)) then
          iostat = -2
          return
        elseif (stop_on_help_opt) then
          stop
        endif
      endif

      if (argn(1:2)  /= "--") then
        if (.not.present(iostat) .and. .not.present(unmatched)) then
          write(*,*) "i=",i, "; Got: ",trim(argn)
          stop("ERROR::{module_name} type-specific command line &
            & arguments must start with '--'")
        else
          unmatched_opt(j) = trim(argn)
          j = j + 1
          i = i + 1  ! check next argument
          cycle
        endif
      endif

      if (has_param_{group_name}(params, trim(argn(3:)))) then
        ! +++++  present 

        ! only "--name value" is tolerated
        ! check if value is missing
        missing_value = .false.
        if (i+1 <= n) then
          argv = args_opt(i+1)
          if ( len(argv) > 1) then
            ! ...next argument starts with '--'
            if (argv(1:2)  == "--" ) then
              missing_value = .true.
            endif
          endif
        else 
          ! ...end of command line
          missing_value = .true.
        endif
        
        if (missing_value) then
          write(*,*) "ERROR::{module_name}::{group_name}: &
            & missing value for "//trim(argn(3:))
          if (present(iostat)) then
            iostat = -1
            return
          else
            stop
          endif
        endif

        call set_param_string_{group_name}(params, trim(argn(3:)), trim(argv), &
          & iostat=iostat)

        if (present(iostat)) then
          if (iostat /= 0) then
            return  ! error
          endif
        endif

        parsed = parsed + 2
        i = i + 2

      else
        ! +++++  not found

        if (.not. present(iostat) .and. .not. present(unmatched)) then
          write(*,*) "ERROR: unknown parameter in {group_name} : ",trim(argn)
          write(*,*) ""
          write(*,*) "-h or --help for HELP"
          stop
        endif

        unmatched_opt(j) = trim(argn)
        j = j + 1
        i = i + 1 

      endif

    enddo

    ! At this point, any type error or --help message cases are already sorted out
    if (present(iostat)) then
      iostat = n - parsed
    endif

    if (present(unmatched)) then
      unmatched = unmatched_opt(:n-parsed)
    endif

    deallocate(args_opt, unmatched_opt)

end subroutine

subroutine print_help_{group_name}(params, iounit, default)
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Print HELP on a derived type, useful for command-line arg
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  type({type_name}), intent(in) :: params
  integer, optional :: iounit
  integer :: io
  logical, optional :: default
  logical :: def
  character(len=2000) :: valuestr
  character(len=20) :: valuelen
  character(len=2000) :: nameshort
  if (present(iounit)) then
    io = iounit
  else
    io = 6  ! screen
  endif
  if (present(default)) then
    def = default
  else
    def = .true. ! by default do show default values
  endif
  write(io, *) " "
  write(io, *) "+++++++++++++++++      {group_name}      ++++++++++++++++++"
  write(io, *) " "
  {list_help}
end subroutine

subroutine set_param_string_{group_name} (params, name, string, iostat)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the {group_name} type from a string argument
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type({type_name}), intent(inout) :: params
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: string
    integer, intent(out), optional :: iostat
    integer :: io !! local...

    if (present(iostat)) then
      iostat = 0
    endif

    select case (name) 
    {list_set_cases}
    case default
      write(*,*) "ERROR::{module_name}::{group_name}: &
        & unknown member :: ", trim(name)
      if (present(iostat)) then
        iostat = -1
      else
        stop
      endif
    end select
end subroutine

function has_param_{group_name} (params, name) result(has_param)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Test whether an attribute name is member of the type {group_name}
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type({type_name}), intent(inout) :: params
    character(len=*), intent(in) :: name
    logical :: has_param

    has_param = .true.
    select case (name) 
      {list_has_cases}
    case default
      has_param = .false.
    end select
end function

