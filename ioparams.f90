module ioparams
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Automatically generated module by nml2f90.py [source: /home/perrette/GreenlandOutletGlaciers/branches/tuning/params.nml]
    !
    ! https://github.com/perrette/nml-to-f90
    ! version: 0.0.0.dev-ab81874
    ! 
    ! Contains read / write subroutines for all derived types imported below.
    ! As well as setter / getter access by field name
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    implicit none

    private
    public :: geometry_t, dynamics_t, calving_t, smb  ! surface mass balance_t, &
fjordmelt   ! submarine melting_t, basalmelt ! basal melting_t
    public :: read_nml, write_nml          ! nml I/O
    public :: set_param, get_param         ! generic set/get
    public :: parse_command_argument       ! parse and assign command-line arg
    public :: print_help                   ! print command-line argument help
    public :: has_param, set_param_string  ! useful fine-grained control on parse_command

    integer, parameter :: dp = kind(0.d0)
    integer, parameter :: clen = 256    ! default character length
    logical :: VERBOSE = .true.

    type geometry_t
        real(kind=dp) :: L
        real(kind=dp) :: dx0
        character(len=256) :: grid_mode
        character(len=256) :: functype
        real(kind=dp) :: dx1
        real(kind=dp) :: D
        character(len=256) :: width_mode
        character(len=256) :: bedrock_mode
        real(kind=dp) :: x_scale
        real(kind=dp), dimension(3) :: w
        real(kind=dp), dimension(7) :: z
        real(kind=dp) :: slr
        real(kind=dp) :: H_max
        character(len=256) :: filename
    end type
    type dynamics_t
        real(kind=dp) :: rho_i
        real(kind=dp) :: rho_sw
        real(kind=dp) :: rho_fw
        real(kind=dp) :: g
        integer :: n
        real(kind=dp), dimension(2) :: A
        real(kind=dp) :: E
        character(len=256) :: basal_mode
        real(kind=dp) :: beta
        integer :: m
        logical :: use_EP
        real(kind=dp) :: EP_max
        character(len=256) :: upstream_U
        real(kind=dp) :: U0
        real(kind=dp) :: dUdx0
        character(len=256) :: upstream_H
        real(kind=dp) :: H0
        real(kind=dp) :: dhdx0
        character(len=256) :: filename
        integer :: maxiter
        real(kind=dp) :: vel_increment
        real(kind=dp) :: stress_res
        real(kind=dp) :: stress_res_max
        logical :: verbose
    end type
    type calving_t
        character(len=256) :: mode
        integer :: fwd
        integer :: H_min
        integer :: debug
    end type
    type smb  ! surface mass balance_t
        character(len=256) :: mode
        real(kind=dp) :: a
        real(kind=dp) :: ELA
        real(kind=dp) :: lapserate
        real(kind=dp) :: smb_offset
        real(kind=dp) :: c1
        real(kind=dp) :: c2
        character(len=256) :: filename
    end type
    type fjordmelt   ! submarine melting_t
        character(len=256) :: mode
        real(kind=dp) :: scal
        real(kind=dp) :: cal_melt
        real(kind=dp) :: sub_rate
        character(len=256) :: filename
    end type
    type basalmelt ! basal melting_t
        real(kind=dp) :: bmelt
        character(len=256) :: filename
    end type

    interface read_nml
        module procedure :: read_nml_geometry
        module procedure :: read_nml_dynamics
        module procedure :: read_nml_calving
        module procedure :: read_nml_smb  ! surface mass balance
        module procedure :: read_nml_fjordmelt   ! submarine melting
        module procedure :: read_nml_basalmelt ! basal melting
    end interface

    interface write_nml
        module procedure :: write_nml_geometry
        module procedure :: write_nml_dynamics
        module procedure :: write_nml_calving
        module procedure :: write_nml_smb  ! surface mass balance
        module procedure :: write_nml_fjordmelt   ! submarine melting
        module procedure :: write_nml_basalmelt ! basal melting
    end interface

    interface parse_command_argument
        module procedure :: parse_command_argument_geometry
        module procedure :: parse_command_argument_dynamics
        module procedure :: parse_command_argument_calving
        module procedure :: parse_command_argument_smb  ! surface mass balance
        module procedure :: parse_command_argument_fjordmelt   ! submarine melting
        module procedure :: parse_command_argument_basalmelt ! basal melting
    end interface

    interface print_help
        module procedure :: print_help_geometry
        module procedure :: print_help_dynamics
        module procedure :: print_help_calving
        module procedure :: print_help_smb  ! surface mass balance
        module procedure :: print_help_fjordmelt   ! submarine melting
        module procedure :: print_help_basalmelt ! basal melting
    end interface

    interface has_param
        module procedure :: has_param_geometry
        module procedure :: has_param_dynamics
        module procedure :: has_param_calving
        module procedure :: has_param_smb  ! surface mass balance
        module procedure :: has_param_fjordmelt   ! submarine melting
        module procedure :: has_param_basalmelt ! basal melting
    end interface

    interface set_param_string
        module procedure :: set_param_string_geometry
        module procedure :: set_param_string_dynamics
        module procedure :: set_param_string_calving
        module procedure :: set_param_string_smb  ! surface mass balance
        module procedure :: set_param_string_fjordmelt   ! submarine melting
        module procedure :: set_param_string_basalmelt ! basal melting
    end interface

    interface set_param
        
    end interface

    interface get_param
        
    end interface

    interface string_to_array
      module procedure :: string_to_array_integer
      module procedure :: string_to_array_double
      module procedure :: string_to_array_string
      module procedure :: string_to_array_logical
    end interface

contains

    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! IO routines
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

subroutine read_nml_geometry (iounit, params)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Read the geometry group in a namelist file and assign to type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    integer, intent(in) :: iounit
    type(geometry_t), intent(inout) :: params

    real(kind=dp) :: L
    real(kind=dp) :: dx0
    character(len=256) :: grid_mode
    character(len=256) :: functype
    real(kind=dp) :: dx1
    real(kind=dp) :: D
    character(len=256) :: width_mode
    character(len=256) :: bedrock_mode
    real(kind=dp) :: x_scale
    real(kind=dp), dimension(3) :: w
    real(kind=dp), dimension(7) :: z
    real(kind=dp) :: slr
    real(kind=dp) :: H_max
    character(len=256) :: filename

    namelist / geometry / L, dx0, grid_mode, functype, dx1, D, width_mode, bedrock_mode, &
x_scale, w, z, slr, H_max, filename

    ! initialize variables
    L = params%L
    dx0 = params%dx0
    grid_mode = params%grid_mode
    functype = params%functype
    dx1 = params%dx1
    D = params%D
    width_mode = params%width_mode
    bedrock_mode = params%bedrock_mode
    x_scale = params%x_scale
    w = params%w
    z = params%z
    slr = params%slr
    H_max = params%H_max
    filename = params%filename

    ! read all
    read(unit=iounit, nml=geometry) 

    ! assign back to type
    params%L = L
    params%dx0 = dx0
    params%grid_mode = grid_mode
    params%functype = functype
    params%dx1 = dx1
    params%D = D
    params%width_mode = width_mode
    params%bedrock_mode = bedrock_mode
    params%x_scale = x_scale
    params%w = w
    params%z = z
    params%slr = slr
    params%H_max = H_max
    params%filename = filename
end subroutine

subroutine write_nml_geometry (iounit, params)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Read the geometry group in a namelist file and assign to type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    integer, intent(in) :: iounit
    type(geometry_t), intent(inout) :: params

    real(kind=dp) :: L
    real(kind=dp) :: dx0
    character(len=256) :: grid_mode
    character(len=256) :: functype
    real(kind=dp) :: dx1
    real(kind=dp) :: D
    character(len=256) :: width_mode
    character(len=256) :: bedrock_mode
    real(kind=dp) :: x_scale
    real(kind=dp), dimension(3) :: w
    real(kind=dp), dimension(7) :: z
    real(kind=dp) :: slr
    real(kind=dp) :: H_max
    character(len=256) :: filename

    namelist / geometry / L, dx0, grid_mode, functype, dx1, D, width_mode, bedrock_mode, &
x_scale, w, z, slr, H_max, filename

    ! initialize variables
    L = params%L
    dx0 = params%dx0
    grid_mode = params%grid_mode
    functype = params%functype
    dx1 = params%dx1
    D = params%D
    width_mode = params%width_mode
    bedrock_mode = params%bedrock_mode
    x_scale = params%x_scale
    w = params%w
    z = params%z
    slr = params%slr
    H_max = params%H_max
    filename = params%filename

    ! write_all
    write(unit=iounit, nml=geometry) 
end subroutine


subroutine read_nml_dynamics (iounit, params)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Read the dynamics group in a namelist file and assign to type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    integer, intent(in) :: iounit
    type(dynamics_t), intent(inout) :: params

    real(kind=dp) :: rho_i
    real(kind=dp) :: rho_sw
    real(kind=dp) :: rho_fw
    real(kind=dp) :: g
    integer :: n
    real(kind=dp), dimension(2) :: A
    real(kind=dp) :: E
    character(len=256) :: basal_mode
    real(kind=dp) :: beta
    integer :: m
    logical :: use_EP
    real(kind=dp) :: EP_max
    character(len=256) :: upstream_U
    real(kind=dp) :: U0
    real(kind=dp) :: dUdx0
    character(len=256) :: upstream_H
    real(kind=dp) :: H0
    real(kind=dp) :: dhdx0
    character(len=256) :: filename
    integer :: maxiter
    real(kind=dp) :: vel_increment
    real(kind=dp) :: stress_res
    real(kind=dp) :: stress_res_max
    logical :: verbose

    namelist / dynamics / rho_i, rho_sw, rho_fw, g, n, A, E, basal_mode, beta, m, use_EP, &
EP_max, upstream_U, U0, dUdx0, upstream_H, H0, dhdx0, filename, &
maxiter, vel_increment, stress_res, stress_res_max, verbose

    ! initialize variables
    rho_i = params%rho_i
    rho_sw = params%rho_sw
    rho_fw = params%rho_fw
    g = params%g
    n = params%n
    A = params%A
    E = params%E
    basal_mode = params%basal_mode
    beta = params%beta
    m = params%m
    use_EP = params%use_EP
    EP_max = params%EP_max
    upstream_U = params%upstream_U
    U0 = params%U0
    dUdx0 = params%dUdx0
    upstream_H = params%upstream_H
    H0 = params%H0
    dhdx0 = params%dhdx0
    filename = params%filename
    maxiter = params%maxiter
    vel_increment = params%vel_increment
    stress_res = params%stress_res
    stress_res_max = params%stress_res_max
    verbose = params%verbose

    ! read all
    read(unit=iounit, nml=dynamics) 

    ! assign back to type
    params%rho_i = rho_i
    params%rho_sw = rho_sw
    params%rho_fw = rho_fw
    params%g = g
    params%n = n
    params%A = A
    params%E = E
    params%basal_mode = basal_mode
    params%beta = beta
    params%m = m
    params%use_EP = use_EP
    params%EP_max = EP_max
    params%upstream_U = upstream_U
    params%U0 = U0
    params%dUdx0 = dUdx0
    params%upstream_H = upstream_H
    params%H0 = H0
    params%dhdx0 = dhdx0
    params%filename = filename
    params%maxiter = maxiter
    params%vel_increment = vel_increment
    params%stress_res = stress_res
    params%stress_res_max = stress_res_max
    params%verbose = verbose
end subroutine

subroutine write_nml_dynamics (iounit, params)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Read the dynamics group in a namelist file and assign to type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    integer, intent(in) :: iounit
    type(dynamics_t), intent(inout) :: params

    real(kind=dp) :: rho_i
    real(kind=dp) :: rho_sw
    real(kind=dp) :: rho_fw
    real(kind=dp) :: g
    integer :: n
    real(kind=dp), dimension(2) :: A
    real(kind=dp) :: E
    character(len=256) :: basal_mode
    real(kind=dp) :: beta
    integer :: m
    logical :: use_EP
    real(kind=dp) :: EP_max
    character(len=256) :: upstream_U
    real(kind=dp) :: U0
    real(kind=dp) :: dUdx0
    character(len=256) :: upstream_H
    real(kind=dp) :: H0
    real(kind=dp) :: dhdx0
    character(len=256) :: filename
    integer :: maxiter
    real(kind=dp) :: vel_increment
    real(kind=dp) :: stress_res
    real(kind=dp) :: stress_res_max
    logical :: verbose

    namelist / dynamics / rho_i, rho_sw, rho_fw, g, n, A, E, basal_mode, beta, m, use_EP, &
EP_max, upstream_U, U0, dUdx0, upstream_H, H0, dhdx0, filename, &
maxiter, vel_increment, stress_res, stress_res_max, verbose

    ! initialize variables
    rho_i = params%rho_i
    rho_sw = params%rho_sw
    rho_fw = params%rho_fw
    g = params%g
    n = params%n
    A = params%A
    E = params%E
    basal_mode = params%basal_mode
    beta = params%beta
    m = params%m
    use_EP = params%use_EP
    EP_max = params%EP_max
    upstream_U = params%upstream_U
    U0 = params%U0
    dUdx0 = params%dUdx0
    upstream_H = params%upstream_H
    H0 = params%H0
    dhdx0 = params%dhdx0
    filename = params%filename
    maxiter = params%maxiter
    vel_increment = params%vel_increment
    stress_res = params%stress_res
    stress_res_max = params%stress_res_max
    verbose = params%verbose

    ! write_all
    write(unit=iounit, nml=dynamics) 
end subroutine


subroutine read_nml_calving (iounit, params)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Read the calving group in a namelist file and assign to type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    integer, intent(in) :: iounit
    type(calving_t), intent(inout) :: params

    character(len=256) :: mode
    integer :: fwd
    integer :: H_min
    integer :: debug

    namelist / calving / mode, fwd, H_min, debug

    ! initialize variables
    mode = params%mode
    fwd = params%fwd
    H_min = params%H_min
    debug = params%debug

    ! read all
    read(unit=iounit, nml=calving) 

    ! assign back to type
    params%mode = mode
    params%fwd = fwd
    params%H_min = H_min
    params%debug = debug
end subroutine

subroutine write_nml_calving (iounit, params)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Read the calving group in a namelist file and assign to type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    integer, intent(in) :: iounit
    type(calving_t), intent(inout) :: params

    character(len=256) :: mode
    integer :: fwd
    integer :: H_min
    integer :: debug

    namelist / calving / mode, fwd, H_min, debug

    ! initialize variables
    mode = params%mode
    fwd = params%fwd
    H_min = params%H_min
    debug = params%debug

    ! write_all
    write(unit=iounit, nml=calving) 
end subroutine


subroutine read_nml_smb  ! surface mass balance (iounit, params)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Read the smb  ! surface mass balance group in a namelist file and assign to type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    integer, intent(in) :: iounit
    type(smb  ! surface mass balance_t), intent(inout) :: params

    character(len=256) :: mode
    real(kind=dp) :: a
    real(kind=dp) :: ELA
    real(kind=dp) :: lapserate
    real(kind=dp) :: smb_offset
    real(kind=dp) :: c1
    real(kind=dp) :: c2
    character(len=256) :: filename

    namelist / smb  ! surface mass balance / mode, a, ELA, lapserate, smb_offset, c1, c2, filename

    ! initialize variables
    mode = params%mode
    a = params%a
    ELA = params%ELA
    lapserate = params%lapserate
    smb_offset = params%smb_offset
    c1 = params%c1
    c2 = params%c2
    filename = params%filename

    ! read all
    read(unit=iounit, nml=smb  ! surface mass balance) 

    ! assign back to type
    params%mode = mode
    params%a = a
    params%ELA = ELA
    params%lapserate = lapserate
    params%smb_offset = smb_offset
    params%c1 = c1
    params%c2 = c2
    params%filename = filename
end subroutine

subroutine write_nml_smb  ! surface mass balance (iounit, params)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Read the smb  ! surface mass balance group in a namelist file and assign to type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    integer, intent(in) :: iounit
    type(smb  ! surface mass balance_t), intent(inout) :: params

    character(len=256) :: mode
    real(kind=dp) :: a
    real(kind=dp) :: ELA
    real(kind=dp) :: lapserate
    real(kind=dp) :: smb_offset
    real(kind=dp) :: c1
    real(kind=dp) :: c2
    character(len=256) :: filename

    namelist / smb  ! surface mass balance / mode, a, ELA, lapserate, smb_offset, c1, c2, filename

    ! initialize variables
    mode = params%mode
    a = params%a
    ELA = params%ELA
    lapserate = params%lapserate
    smb_offset = params%smb_offset
    c1 = params%c1
    c2 = params%c2
    filename = params%filename

    ! write_all
    write(unit=iounit, nml=smb  ! surface mass balance) 
end subroutine


subroutine read_nml_fjordmelt   ! submarine melting (iounit, params)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Read the fjordmelt   ! submarine melting group in a namelist file and assign to type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    integer, intent(in) :: iounit
    type(fjordmelt   ! submarine melting_t), intent(inout) :: params

    character(len=256) :: mode
    real(kind=dp) :: scal
    real(kind=dp) :: cal_melt
    real(kind=dp) :: sub_rate
    character(len=256) :: filename

    namelist / fjordmelt   ! submarine melting / mode, scal, cal_melt, sub_rate, filename

    ! initialize variables
    mode = params%mode
    scal = params%scal
    cal_melt = params%cal_melt
    sub_rate = params%sub_rate
    filename = params%filename

    ! read all
    read(unit=iounit, nml=fjordmelt   ! submarine melting) 

    ! assign back to type
    params%mode = mode
    params%scal = scal
    params%cal_melt = cal_melt
    params%sub_rate = sub_rate
    params%filename = filename
end subroutine

subroutine write_nml_fjordmelt   ! submarine melting (iounit, params)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Read the fjordmelt   ! submarine melting group in a namelist file and assign to type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    integer, intent(in) :: iounit
    type(fjordmelt   ! submarine melting_t), intent(inout) :: params

    character(len=256) :: mode
    real(kind=dp) :: scal
    real(kind=dp) :: cal_melt
    real(kind=dp) :: sub_rate
    character(len=256) :: filename

    namelist / fjordmelt   ! submarine melting / mode, scal, cal_melt, sub_rate, filename

    ! initialize variables
    mode = params%mode
    scal = params%scal
    cal_melt = params%cal_melt
    sub_rate = params%sub_rate
    filename = params%filename

    ! write_all
    write(unit=iounit, nml=fjordmelt   ! submarine melting) 
end subroutine


subroutine read_nml_basalmelt ! basal melting (iounit, params)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Read the basalmelt ! basal melting group in a namelist file and assign to type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    integer, intent(in) :: iounit
    type(basalmelt ! basal melting_t), intent(inout) :: params

    real(kind=dp) :: bmelt
    character(len=256) :: filename

    namelist / basalmelt ! basal melting / bmelt, filename

    ! initialize variables
    bmelt = params%bmelt
    filename = params%filename

    ! read all
    read(unit=iounit, nml=basalmelt ! basal melting) 

    ! assign back to type
    params%bmelt = bmelt
    params%filename = filename
end subroutine

subroutine write_nml_basalmelt ! basal melting (iounit, params)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Read the basalmelt ! basal melting group in a namelist file and assign to type
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    integer, intent(in) :: iounit
    type(basalmelt ! basal melting_t), intent(inout) :: params

    real(kind=dp) :: bmelt
    character(len=256) :: filename

    namelist / basalmelt ! basal melting / bmelt, filename

    ! initialize variables
    bmelt = params%bmelt
    filename = params%filename

    ! write_all
    write(unit=iounit, nml=basalmelt ! basal melting) 
end subroutine


    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Routines useful to process command-line parameters: 
    ! - has_param
    ! - set_param_string
    ! - parse_command_argument
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

subroutine parse_command_argument_geometry (params,i, iostat)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Maybe assign ith command line argument to the params type
    ! Input:
    !   params : the paramter type
    !   i : integer, positional argument of the parameter name
    ! Output:
    !   iostat, optional : 0 if success, 1 otherwise (if not
    !       provided, an error is thrown in case of failure)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(geometry_t), intent(inout) :: params
    integer, intent(inout) :: i
    integer, optional :: iostat
    character(len=512) :: arg, argv

    call get_command_argument(i, arg)

    ! Print HELP ?
    if (arg == '--help' .or. arg=='-h') then
      call print_help_geometry(params)
      return
    endif

    if (has_param_geometry(params, trim(arg(3:)))) then
      ! +++++  present 
      call get_command_argument(i+1, argv)
      call set_param_string_geometry(params, trim(arg(3:)), trim(argv))
      i = i+1
      if (present(iostat)) then
        iostat = 0
      endif
    else
      ! +++++  no found
      if (present(iostat)) then
        iostat=1
      else
        write(*,*) "ERROR: unknown parameter in geometry : --",trim(arg(3:))
        write(*,*) ""
        write(*,*) "-h or --help for HELP"
        stop
      endif
    endif
end subroutine

subroutine print_help_geometry(params, iounit, default)
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Print HELP on a derived type, useful for command-line arg
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  type(geometry_t), intent(in) :: params
  integer, optional :: iounit
  integer :: io
  logical, optional :: default
  logical :: def
  character(len=2000) :: valuestr
  character(len=20) :: valuelen
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
  write(io, *) "+++++++++++++++++      geometry      ++++++++++++++++++"
  
if (def) then
    write(valuestr, *) params%L
    write(io, *) "--L domain length (m): set to -1 to adapt to input data (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--L domain length (m): set to -1 to adapt to input data (type: real(kind=dp))"
endif

    
if (def) then
    write(valuestr, *) params%dx0
    write(io, *) "--dx0 desired grid spacing (poly or regridding at restart time) (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--dx0 desired grid spacing (poly or regridding at restart time) (type: real(kind=dp))"
endif

    
if (def) then
    write(valuestr, *) params%grid_mode
    write(io, *) "--grid_mode "refined", "stretched" (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--grid_mode "refined", "stretched" (type: character(len=256))"
endif

    
if (def) then
    write(valuestr, *) params%functype
    write(io, *) "--functype "sigmoid", "hyperbolic", "gaussian", "linear","arctan" (in "refined mode") (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--functype "sigmoid", "hyperbolic", "gaussian", "linear","arctan" (in "refined mode") (type: character(len=256))"
endif

    
if (def) then
    write(valuestr, *) params%dx1
    write(io, *) "--dx1 grid refinement near the grounding line (refined mode) (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--dx1 grid refinement near the grounding line (refined mode) (type: real(kind=dp))"
endif

    
if (def) then
    write(valuestr, *) params%D
    write(io, *) "--D refined zone G.L. +/- D around grounding line (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--D refined zone G.L. +/- D around grounding line (type: real(kind=dp))"
endif

    
if (def) then
    write(valuestr, *) params%width_mode
    write(io, *) "--width_mode "from_file", "polynomial" (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--width_mode "from_file", "polynomial" (type: character(len=256))"
endif

    
if (def) then
    write(valuestr, *) params%bedrock_mode
    write(io, *) "--bedrock_mode "from_file", "polynomial" (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--bedrock_mode "from_file", "polynomial" (type: character(len=256))"
endif

    
if (def) then
    write(valuestr, *) params%x_scale
    write(io, *) "--x_scale (m) normalize x axis prior to the calculation of polynomials (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--x_scale (m) normalize x axis prior to the calculation of polynomials (type: real(kind=dp))"
endif

    
if (def) then
    write(valuestr, *) params%w(1) ! only first element is shown
    write(valuelen, *) len(trim(adjustl(valuestr)))
    write(io, '("--w polynomial coef to describe glacier width (m, m/m, m/m2) (default: &
        [",A'//trim(valuelen)//',", ...], size=",I2,")")') &
            trim(adjustl(valuestr)), size(params%w)
else
    write(io, *) "--w polynomial coef to describe glacier width (m, m/m, m/m2) (type: real(kind=dp), dimension(3))"
endif

    
if (def) then
    write(valuestr, *) params%z(1) ! only first element is shown
    write(valuelen, *) len(trim(adjustl(valuestr)))
    write(io, '("--z polynomial coef for bedrock topography (m, m/m, ...) (default: &
        [",A'//trim(valuelen)//',", ...], size=",I2,")")') &
            trim(adjustl(valuestr)), size(params%z)
else
    write(io, *) "--z polynomial coef for bedrock topography (m, m/m, ...) (type: real(kind=dp), dimension(7))"
endif

    
if (def) then
    write(valuestr, *) params%slr
    write(io, *) "--slr (m) sea level rise above reference level: bedrock topo (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--slr (m) sea level rise above reference level: bedrock topo (type: real(kind=dp))"
endif

    
if (def) then
    write(valuestr, *) params%H_max
    write(io, *) "--H_max stop if glacier thicker than that (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--H_max stop if glacier thicker than that (type: real(kind=dp))"
endif

    
if (def) then
    write(valuestr, *) params%filename
    write(io, *) "--filename to read from (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--filename to read from (type: character(len=256))"
endif

end subroutine

subroutine set_param_string_geometry (params, name, string)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the geometry type from a string argument
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(geometry_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: string
    integer :: iostat

    select case (name) 
    
case ('L', 'geometry%L')
    read(string, *, iostat=IOSTAT) params%L
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "geometry%L = ", params%L
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --geometry%L"
        else
            write(*,*) "ERROR converting string to real(kind=dp): --geometry%L ",trim(string)
        endif
        stop
    endif

    
case ('dx0', 'geometry%dx0')
    read(string, *, iostat=IOSTAT) params%dx0
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "geometry%dx0 = ", params%dx0
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --geometry%dx0"
        else
            write(*,*) "ERROR converting string to real(kind=dp): --geometry%dx0 ",trim(string)
        endif
        stop
    endif

    
case ('grid_mode', 'geometry%grid_mode')
    read(string, *, iostat=IOSTAT) params%grid_mode
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "geometry%grid_mode = ", params%grid_mode
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --geometry%grid_mode"
        else
            write(*,*) "ERROR converting string to character(len=256): --geometry%grid_mode ",trim(string)
        endif
        stop
    endif

    
case ('functype', 'geometry%functype')
    read(string, *, iostat=IOSTAT) params%functype
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "geometry%functype = ", params%functype
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --geometry%functype"
        else
            write(*,*) "ERROR converting string to character(len=256): --geometry%functype ",trim(string)
        endif
        stop
    endif

    
case ('dx1', 'geometry%dx1')
    read(string, *, iostat=IOSTAT) params%dx1
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "geometry%dx1 = ", params%dx1
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --geometry%dx1"
        else
            write(*,*) "ERROR converting string to real(kind=dp): --geometry%dx1 ",trim(string)
        endif
        stop
    endif

    
case ('D', 'geometry%D')
    read(string, *, iostat=IOSTAT) params%D
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "geometry%D = ", params%D
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --geometry%D"
        else
            write(*,*) "ERROR converting string to real(kind=dp): --geometry%D ",trim(string)
        endif
        stop
    endif

    
case ('width_mode', 'geometry%width_mode')
    read(string, *, iostat=IOSTAT) params%width_mode
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "geometry%width_mode = ", params%width_mode
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --geometry%width_mode"
        else
            write(*,*) "ERROR converting string to character(len=256): --geometry%width_mode ",trim(string)
        endif
        stop
    endif

    
case ('bedrock_mode', 'geometry%bedrock_mode')
    read(string, *, iostat=IOSTAT) params%bedrock_mode
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "geometry%bedrock_mode = ", params%bedrock_mode
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --geometry%bedrock_mode"
        else
            write(*,*) "ERROR converting string to character(len=256): --geometry%bedrock_mode ",trim(string)
        endif
        stop
    endif

    
case ('x_scale', 'geometry%x_scale')
    read(string, *, iostat=IOSTAT) params%x_scale
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "geometry%x_scale = ", params%x_scale
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --geometry%x_scale"
        else
            write(*,*) "ERROR converting string to real(kind=dp): --geometry%x_scale ",trim(string)
        endif
        stop
    endif

    
case ('w', 'geometry%w')
    call string_to_array(string, params%w, iostat=iostat)
    if (iostat /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --geometry%w"
        else
            write(*,*) "ERROR converting string to real(kind=dp), dimension(3) array : --geometry%w ",trim(string)
        endif
        stop
    endif

    
case ('z', 'geometry%z')
    call string_to_array(string, params%z, iostat=iostat)
    if (iostat /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --geometry%z"
        else
            write(*,*) "ERROR converting string to real(kind=dp), dimension(7) array : --geometry%z ",trim(string)
        endif
        stop
    endif

    
case ('slr', 'geometry%slr')
    read(string, *, iostat=IOSTAT) params%slr
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "geometry%slr = ", params%slr
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --geometry%slr"
        else
            write(*,*) "ERROR converting string to real(kind=dp): --geometry%slr ",trim(string)
        endif
        stop
    endif

    
case ('H_max', 'geometry%H_max')
    read(string, *, iostat=IOSTAT) params%H_max
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "geometry%H_max = ", params%H_max
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --geometry%H_max"
        else
            write(*,*) "ERROR converting string to real(kind=dp): --geometry%H_max ",trim(string)
        endif
        stop
    endif

    
case ('filename', 'geometry%filename')
    read(string, *, iostat=IOSTAT) params%filename
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "geometry%filename = ", params%filename
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --geometry%filename"
        else
            write(*,*) "ERROR converting string to character(len=256): --geometry%filename ",trim(string)
        endif
        stop
    endif

    case default
      write(*,*) "ERROR set_param_string for geometry: unknown member :: ",trim(name)
      stop
    end select
end subroutine

function has_param_geometry (params, name) result(has_param)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Test whether an attribute name is member of the type geometry
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(geometry_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    logical :: has_param

    has_param = .true.
    select case (name) 
      
case ('L', 'geometry%L')

    
case ('dx0', 'geometry%dx0')

    
case ('grid_mode', 'geometry%grid_mode')

    
case ('functype', 'geometry%functype')

    
case ('dx1', 'geometry%dx1')

    
case ('D', 'geometry%D')

    
case ('width_mode', 'geometry%width_mode')

    
case ('bedrock_mode', 'geometry%bedrock_mode')

    
case ('x_scale', 'geometry%x_scale')

    
case ('w', 'geometry%w')

    
case ('z', 'geometry%z')

    
case ('slr', 'geometry%slr')

    
case ('H_max', 'geometry%H_max')

    
case ('filename', 'geometry%filename')

    case default
      has_param = .false.
    end select
end function



subroutine parse_command_argument_dynamics (params,i, iostat)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Maybe assign ith command line argument to the params type
    ! Input:
    !   params : the paramter type
    !   i : integer, positional argument of the parameter name
    ! Output:
    !   iostat, optional : 0 if success, 1 otherwise (if not
    !       provided, an error is thrown in case of failure)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(dynamics_t), intent(inout) :: params
    integer, intent(inout) :: i
    integer, optional :: iostat
    character(len=512) :: arg, argv

    call get_command_argument(i, arg)

    ! Print HELP ?
    if (arg == '--help' .or. arg=='-h') then
      call print_help_dynamics(params)
      return
    endif

    if (has_param_dynamics(params, trim(arg(3:)))) then
      ! +++++  present 
      call get_command_argument(i+1, argv)
      call set_param_string_dynamics(params, trim(arg(3:)), trim(argv))
      i = i+1
      if (present(iostat)) then
        iostat = 0
      endif
    else
      ! +++++  no found
      if (present(iostat)) then
        iostat=1
      else
        write(*,*) "ERROR: unknown parameter in dynamics : --",trim(arg(3:))
        write(*,*) ""
        write(*,*) "-h or --help for HELP"
        stop
      endif
    endif
end subroutine

subroutine print_help_dynamics(params, iounit, default)
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Print HELP on a derived type, useful for command-line arg
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  type(dynamics_t), intent(in) :: params
  integer, optional :: iounit
  integer :: io
  logical, optional :: default
  logical :: def
  character(len=2000) :: valuestr
  character(len=20) :: valuelen
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
  write(io, *) "+++++++++++++++++      dynamics      ++++++++++++++++++"
  
if (def) then
    write(valuestr, *) params%rho_i
    write(io, *) "--rho_i ice density (kg m^-3) (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--rho_i ice density (kg m^-3) (type: real(kind=dp))"
endif

    
if (def) then
    write(valuestr, *) params%rho_sw
    write(io, *) "--rho_sw ocean water density (kg m^-3) (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--rho_sw ocean water density (kg m^-3) (type: real(kind=dp))"
endif

    
if (def) then
    write(valuestr, *) params%rho_fw
    write(io, *) "--rho_fw fresh water density (kg m^-3) (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--rho_fw fresh water density (kg m^-3) (type: real(kind=dp))"
endif

    
if (def) then
    write(valuestr, *) params%g
    write(io, *) "--g acceleration (m s^-2) (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--g acceleration (m s^-2) (type: real(kind=dp))"
endif

    
if (def) then
    write(valuestr, *) params%n
    write(io, *) "--n flow law exponent (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--n flow law exponent (type: integer)"
endif

    
if (def) then
    write(valuestr, *) params%A(1) ! only first element is shown
    write(valuelen, *) len(trim(adjustl(valuestr)))
    write(io, '("--A rate factor (linear polynomial: Pa^-n s^-1, Pa^-n s^-1 m^-1) (default: &
        [",A'//trim(valuelen)//',", ...], size=",I2,")")') &
            trim(adjustl(valuestr)), size(params%A)
else
    write(io, *) "--A rate factor (linear polynomial: Pa^-n s^-1, Pa^-n s^-1 m^-1) (type: real(kind=dp), dimension(2))"
endif

    
if (def) then
    write(valuestr, *) params%E
    write(io, *) "--E enhancement factor (accounts for fabric development) (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--E enhancement factor (accounts for fabric development) (type: real(kind=dp))"
endif

    
if (def) then
    write(valuestr, *) params%basal_mode
    write(io, *) "--basal_mode "constant",or "prescribed" for values in basal.nc (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--basal_mode "constant",or "prescribed" for values in basal.nc (type: character(len=256))"
endif

    
if (def) then
    write(valuestr, *) params%beta
    write(io, *) "--beta Pa (m^1/m s^-1/m) (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--beta Pa (m^1/m s^-1/m) (type: real(kind=dp))"
endif

    
if (def) then
    write(valuestr, *) params%m
    write(io, *) "--m basal sliding exponent (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--m basal sliding exponent (type: integer)"
endif

    
if (def) then
    write(valuestr, *) params%use_EP
    write(io, *) "--use_EP if T, scale beta by EP/EP_max when EP < EP_max and zb < 0 (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--use_EP if T, scale beta by EP/EP_max when EP < EP_max and zb < 0 (type: logical)"
endif

    
if (def) then
    write(valuestr, *) params%EP_max
    write(io, *) "--EP_max (kPa) effective pressure cap, above which beta=beta_coef (above) (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--EP_max (kPa) effective pressure cap, above which beta=beta_coef (above) (type: real(kind=dp))"
endif

    
if (def) then
    write(valuestr, *) params%upstream_U
    write(io, *) "--upstream_U  (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--upstream_U  (type: character(len=256))"
endif

    
if (def) then
    write(valuestr, *) params%U0
    write(io, *) "--U0 (m/yr) U boundary condition at x=0 (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--U0 (m/yr) U boundary condition at x=0 (type: real(kind=dp))"
endif

    
if (def) then
    write(valuestr, *) params%dUdx0
    write(io, *) "--dUdx0 (m/s/m) longitudinal velocity gradient at x = 0 (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--dUdx0 (m/s/m) longitudinal velocity gradient at x = 0 (type: real(kind=dp))"
endif

    
if (def) then
    write(valuestr, *) params%upstream_H
    write(io, *) "--upstream_H  (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--upstream_H  (type: character(len=256))"
endif

    
if (def) then
    write(valuestr, *) params%H0
    write(io, *) "--H0 (m) H0 at x = 0, if applicable, or at t=0 for all x (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--H0 (m) H0 at x = 0, if applicable, or at t=0 for all x (type: real(kind=dp))"
endif

    
if (def) then
    write(valuestr, *) params%dhdx0
    write(io, *) "--dhdx0 surface gradient at x = 0 (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--dhdx0 surface gradient at x = 0 (type: real(kind=dp))"
endif

    
if (def) then
    write(valuestr, *) params%filename
    write(io, *) "--filename to read from, in "prescribed" mode (relative to the input) (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--filename to read from, in "prescribed" mode (relative to the input) (type: character(len=256))"
endif

    
if (def) then
    write(valuestr, *) params%maxiter
    write(io, *) "--maxiter maximum number of iterations (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--maxiter maximum number of iterations (type: integer)"
endif

    
if (def) then
    write(valuestr, *) params%vel_increment
    write(io, *) "--vel_increment convergence criterion for velocity, (m / yr) (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--vel_increment convergence criterion for velocity, (m / yr) (type: real(kind=dp))"
endif

    
if (def) then
    write(valuestr, *) params%stress_res
    write(io, *) "--stress_res Pa  (stop iteration if stress residual less than that) (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--stress_res Pa  (stop iteration if stress residual less than that) (type: real(kind=dp))"
endif

    
if (def) then
    write(valuestr, *) params%stress_res_max
    write(io, *) "--stress_res_max Pa  (raise error if stress residual more than that) (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--stress_res_max Pa  (raise error if stress residual more than that) (type: real(kind=dp))"
endif

    
if (def) then
    write(valuestr, *) params%verbose
    write(io, *) "--verbose display message on convergence (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--verbose display message on convergence (type: logical)"
endif

end subroutine

subroutine set_param_string_dynamics (params, name, string)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the dynamics type from a string argument
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(dynamics_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: string
    integer :: iostat

    select case (name) 
    
case ('rho_i', 'dynamics%rho_i')
    read(string, *, iostat=IOSTAT) params%rho_i
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "dynamics%rho_i = ", params%rho_i
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --dynamics%rho_i"
        else
            write(*,*) "ERROR converting string to real(kind=dp): --dynamics%rho_i ",trim(string)
        endif
        stop
    endif

    
case ('rho_sw', 'dynamics%rho_sw')
    read(string, *, iostat=IOSTAT) params%rho_sw
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "dynamics%rho_sw = ", params%rho_sw
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --dynamics%rho_sw"
        else
            write(*,*) "ERROR converting string to real(kind=dp): --dynamics%rho_sw ",trim(string)
        endif
        stop
    endif

    
case ('rho_fw', 'dynamics%rho_fw')
    read(string, *, iostat=IOSTAT) params%rho_fw
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "dynamics%rho_fw = ", params%rho_fw
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --dynamics%rho_fw"
        else
            write(*,*) "ERROR converting string to real(kind=dp): --dynamics%rho_fw ",trim(string)
        endif
        stop
    endif

    
case ('g', 'dynamics%g')
    read(string, *, iostat=IOSTAT) params%g
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "dynamics%g = ", params%g
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --dynamics%g"
        else
            write(*,*) "ERROR converting string to real(kind=dp): --dynamics%g ",trim(string)
        endif
        stop
    endif

    
case ('n', 'dynamics%n')
    read(string, *, iostat=IOSTAT) params%n
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "dynamics%n = ", params%n
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --dynamics%n"
        else
            write(*,*) "ERROR converting string to integer: --dynamics%n ",trim(string)
        endif
        stop
    endif

    
case ('A', 'dynamics%A')
    call string_to_array(string, params%A, iostat=iostat)
    if (iostat /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --dynamics%A"
        else
            write(*,*) "ERROR converting string to real(kind=dp), dimension(2) array : --dynamics%A ",trim(string)
        endif
        stop
    endif

    
case ('E', 'dynamics%E')
    read(string, *, iostat=IOSTAT) params%E
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "dynamics%E = ", params%E
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --dynamics%E"
        else
            write(*,*) "ERROR converting string to real(kind=dp): --dynamics%E ",trim(string)
        endif
        stop
    endif

    
case ('basal_mode', 'dynamics%basal_mode')
    read(string, *, iostat=IOSTAT) params%basal_mode
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "dynamics%basal_mode = ", params%basal_mode
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --dynamics%basal_mode"
        else
            write(*,*) "ERROR converting string to character(len=256): --dynamics%basal_mode ",trim(string)
        endif
        stop
    endif

    
case ('beta', 'dynamics%beta')
    read(string, *, iostat=IOSTAT) params%beta
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "dynamics%beta = ", params%beta
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --dynamics%beta"
        else
            write(*,*) "ERROR converting string to real(kind=dp): --dynamics%beta ",trim(string)
        endif
        stop
    endif

    
case ('m', 'dynamics%m')
    read(string, *, iostat=IOSTAT) params%m
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "dynamics%m = ", params%m
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --dynamics%m"
        else
            write(*,*) "ERROR converting string to integer: --dynamics%m ",trim(string)
        endif
        stop
    endif

    
case ('use_EP', 'dynamics%use_EP')
    read(string, *, iostat=IOSTAT) params%use_EP
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "dynamics%use_EP = ", params%use_EP
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --dynamics%use_EP"
        else
            write(*,*) "ERROR converting string to logical: --dynamics%use_EP ",trim(string)
        endif
        stop
    endif

    
case ('EP_max', 'dynamics%EP_max')
    read(string, *, iostat=IOSTAT) params%EP_max
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "dynamics%EP_max = ", params%EP_max
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --dynamics%EP_max"
        else
            write(*,*) "ERROR converting string to real(kind=dp): --dynamics%EP_max ",trim(string)
        endif
        stop
    endif

    
case ('upstream_U', 'dynamics%upstream_U')
    read(string, *, iostat=IOSTAT) params%upstream_U
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "dynamics%upstream_U = ", params%upstream_U
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --dynamics%upstream_U"
        else
            write(*,*) "ERROR converting string to character(len=256): --dynamics%upstream_U ",trim(string)
        endif
        stop
    endif

    
case ('U0', 'dynamics%U0')
    read(string, *, iostat=IOSTAT) params%U0
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "dynamics%U0 = ", params%U0
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --dynamics%U0"
        else
            write(*,*) "ERROR converting string to real(kind=dp): --dynamics%U0 ",trim(string)
        endif
        stop
    endif

    
case ('dUdx0', 'dynamics%dUdx0')
    read(string, *, iostat=IOSTAT) params%dUdx0
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "dynamics%dUdx0 = ", params%dUdx0
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --dynamics%dUdx0"
        else
            write(*,*) "ERROR converting string to real(kind=dp): --dynamics%dUdx0 ",trim(string)
        endif
        stop
    endif

    
case ('upstream_H', 'dynamics%upstream_H')
    read(string, *, iostat=IOSTAT) params%upstream_H
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "dynamics%upstream_H = ", params%upstream_H
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --dynamics%upstream_H"
        else
            write(*,*) "ERROR converting string to character(len=256): --dynamics%upstream_H ",trim(string)
        endif
        stop
    endif

    
case ('H0', 'dynamics%H0')
    read(string, *, iostat=IOSTAT) params%H0
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "dynamics%H0 = ", params%H0
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --dynamics%H0"
        else
            write(*,*) "ERROR converting string to real(kind=dp): --dynamics%H0 ",trim(string)
        endif
        stop
    endif

    
case ('dhdx0', 'dynamics%dhdx0')
    read(string, *, iostat=IOSTAT) params%dhdx0
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "dynamics%dhdx0 = ", params%dhdx0
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --dynamics%dhdx0"
        else
            write(*,*) "ERROR converting string to real(kind=dp): --dynamics%dhdx0 ",trim(string)
        endif
        stop
    endif

    
case ('filename', 'dynamics%filename')
    read(string, *, iostat=IOSTAT) params%filename
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "dynamics%filename = ", params%filename
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --dynamics%filename"
        else
            write(*,*) "ERROR converting string to character(len=256): --dynamics%filename ",trim(string)
        endif
        stop
    endif

    
case ('maxiter', 'dynamics%maxiter')
    read(string, *, iostat=IOSTAT) params%maxiter
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "dynamics%maxiter = ", params%maxiter
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --dynamics%maxiter"
        else
            write(*,*) "ERROR converting string to integer: --dynamics%maxiter ",trim(string)
        endif
        stop
    endif

    
case ('vel_increment', 'dynamics%vel_increment')
    read(string, *, iostat=IOSTAT) params%vel_increment
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "dynamics%vel_increment = ", params%vel_increment
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --dynamics%vel_increment"
        else
            write(*,*) "ERROR converting string to real(kind=dp): --dynamics%vel_increment ",trim(string)
        endif
        stop
    endif

    
case ('stress_res', 'dynamics%stress_res')
    read(string, *, iostat=IOSTAT) params%stress_res
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "dynamics%stress_res = ", params%stress_res
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --dynamics%stress_res"
        else
            write(*,*) "ERROR converting string to real(kind=dp): --dynamics%stress_res ",trim(string)
        endif
        stop
    endif

    
case ('stress_res_max', 'dynamics%stress_res_max')
    read(string, *, iostat=IOSTAT) params%stress_res_max
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "dynamics%stress_res_max = ", params%stress_res_max
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --dynamics%stress_res_max"
        else
            write(*,*) "ERROR converting string to real(kind=dp): --dynamics%stress_res_max ",trim(string)
        endif
        stop
    endif

    
case ('verbose', 'dynamics%verbose')
    read(string, *, iostat=IOSTAT) params%verbose
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "dynamics%verbose = ", params%verbose
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --dynamics%verbose"
        else
            write(*,*) "ERROR converting string to logical: --dynamics%verbose ",trim(string)
        endif
        stop
    endif

    case default
      write(*,*) "ERROR set_param_string for dynamics: unknown member :: ",trim(name)
      stop
    end select
end subroutine

function has_param_dynamics (params, name) result(has_param)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Test whether an attribute name is member of the type dynamics
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(dynamics_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    logical :: has_param

    has_param = .true.
    select case (name) 
      
case ('rho_i', 'dynamics%rho_i')

    
case ('rho_sw', 'dynamics%rho_sw')

    
case ('rho_fw', 'dynamics%rho_fw')

    
case ('g', 'dynamics%g')

    
case ('n', 'dynamics%n')

    
case ('A', 'dynamics%A')

    
case ('E', 'dynamics%E')

    
case ('basal_mode', 'dynamics%basal_mode')

    
case ('beta', 'dynamics%beta')

    
case ('m', 'dynamics%m')

    
case ('use_EP', 'dynamics%use_EP')

    
case ('EP_max', 'dynamics%EP_max')

    
case ('upstream_U', 'dynamics%upstream_U')

    
case ('U0', 'dynamics%U0')

    
case ('dUdx0', 'dynamics%dUdx0')

    
case ('upstream_H', 'dynamics%upstream_H')

    
case ('H0', 'dynamics%H0')

    
case ('dhdx0', 'dynamics%dhdx0')

    
case ('filename', 'dynamics%filename')

    
case ('maxiter', 'dynamics%maxiter')

    
case ('vel_increment', 'dynamics%vel_increment')

    
case ('stress_res', 'dynamics%stress_res')

    
case ('stress_res_max', 'dynamics%stress_res_max')

    
case ('verbose', 'dynamics%verbose')

    case default
      has_param = .false.
    end select
end function



subroutine parse_command_argument_calving (params,i, iostat)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Maybe assign ith command line argument to the params type
    ! Input:
    !   params : the paramter type
    !   i : integer, positional argument of the parameter name
    ! Output:
    !   iostat, optional : 0 if success, 1 otherwise (if not
    !       provided, an error is thrown in case of failure)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(calving_t), intent(inout) :: params
    integer, intent(inout) :: i
    integer, optional :: iostat
    character(len=512) :: arg, argv

    call get_command_argument(i, arg)

    ! Print HELP ?
    if (arg == '--help' .or. arg=='-h') then
      call print_help_calving(params)
      return
    endif

    if (has_param_calving(params, trim(arg(3:)))) then
      ! +++++  present 
      call get_command_argument(i+1, argv)
      call set_param_string_calving(params, trim(arg(3:)), trim(argv))
      i = i+1
      if (present(iostat)) then
        iostat = 0
      endif
    else
      ! +++++  no found
      if (present(iostat)) then
        iostat=1
      else
        write(*,*) "ERROR: unknown parameter in calving : --",trim(arg(3:))
        write(*,*) ""
        write(*,*) "-h or --help for HELP"
        stop
      endif
    endif
end subroutine

subroutine print_help_calving(params, iounit, default)
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Print HELP on a derived type, useful for command-line arg
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  type(calving_t), intent(in) :: params
  integer, optional :: iounit
  integer :: io
  logical, optional :: default
  logical :: def
  character(len=2000) :: valuestr
  character(len=20) :: valuelen
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
  write(io, *) "+++++++++++++++++      calving      ++++++++++++++++++"
  
if (def) then
    write(valuestr, *) params%mode
    write(io, *) "--mode off, CD, CDw (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--mode off, CD, CDw (type: character(len=256))"
endif

    
if (def) then
    write(valuestr, *) params%fwd
    write(io, *) "--fwd (m) fresh water depth in crevasses (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--fwd (m) fresh water depth in crevasses (type: integer)"
endif

    
if (def) then
    write(valuestr, *) params%H_min
    write(io, *) "--H_min (m) minimum thickness for calving if crevasses never reach the sea level height (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--H_min (m) minimum thickness for calving if crevasses never reach the sea level height (type: integer)"
endif

    
if (def) then
    write(valuestr, *) params%debug
    write(io, *) "--debug debug level for the calving module (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--debug debug level for the calving module (type: integer)"
endif

end subroutine

subroutine set_param_string_calving (params, name, string)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the calving type from a string argument
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(calving_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: string
    integer :: iostat

    select case (name) 
    
case ('mode', 'calving%mode')
    read(string, *, iostat=IOSTAT) params%mode
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "calving%mode = ", params%mode
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --calving%mode"
        else
            write(*,*) "ERROR converting string to character(len=256): --calving%mode ",trim(string)
        endif
        stop
    endif

    
case ('fwd', 'calving%fwd')
    read(string, *, iostat=IOSTAT) params%fwd
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "calving%fwd = ", params%fwd
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --calving%fwd"
        else
            write(*,*) "ERROR converting string to integer: --calving%fwd ",trim(string)
        endif
        stop
    endif

    
case ('H_min', 'calving%H_min')
    read(string, *, iostat=IOSTAT) params%H_min
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "calving%H_min = ", params%H_min
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --calving%H_min"
        else
            write(*,*) "ERROR converting string to integer: --calving%H_min ",trim(string)
        endif
        stop
    endif

    
case ('debug', 'calving%debug')
    read(string, *, iostat=IOSTAT) params%debug
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "calving%debug = ", params%debug
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --calving%debug"
        else
            write(*,*) "ERROR converting string to integer: --calving%debug ",trim(string)
        endif
        stop
    endif

    case default
      write(*,*) "ERROR set_param_string for calving: unknown member :: ",trim(name)
      stop
    end select
end subroutine

function has_param_calving (params, name) result(has_param)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Test whether an attribute name is member of the type calving
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(calving_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    logical :: has_param

    has_param = .true.
    select case (name) 
      
case ('mode', 'calving%mode')

    
case ('fwd', 'calving%fwd')

    
case ('H_min', 'calving%H_min')

    
case ('debug', 'calving%debug')

    case default
      has_param = .false.
    end select
end function



subroutine parse_command_argument_smb  ! surface mass balance (params,i, iostat)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Maybe assign ith command line argument to the params type
    ! Input:
    !   params : the paramter type
    !   i : integer, positional argument of the parameter name
    ! Output:
    !   iostat, optional : 0 if success, 1 otherwise (if not
    !       provided, an error is thrown in case of failure)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(smb  ! surface mass balance_t), intent(inout) :: params
    integer, intent(inout) :: i
    integer, optional :: iostat
    character(len=512) :: arg, argv

    call get_command_argument(i, arg)

    ! Print HELP ?
    if (arg == '--help' .or. arg=='-h') then
      call print_help_smb  ! surface mass balance(params)
      return
    endif

    if (has_param_smb  ! surface mass balance(params, trim(arg(3:)))) then
      ! +++++  present 
      call get_command_argument(i+1, argv)
      call set_param_string_smb  ! surface mass balance(params, trim(arg(3:)), trim(argv))
      i = i+1
      if (present(iostat)) then
        iostat = 0
      endif
    else
      ! +++++  no found
      if (present(iostat)) then
        iostat=1
      else
        write(*,*) "ERROR: unknown parameter in smb  ! surface mass balance : --",trim(arg(3:))
        write(*,*) ""
        write(*,*) "-h or --help for HELP"
        stop
      endif
    endif
end subroutine

subroutine print_help_smb  ! surface mass balance(params, iounit, default)
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Print HELP on a derived type, useful for command-line arg
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  type(smb  ! surface mass balance_t), intent(in) :: params
  integer, optional :: iounit
  integer :: io
  logical, optional :: default
  logical :: def
  character(len=2000) :: valuestr
  character(len=20) :: valuelen
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
  write(io, *) "+++++++++++++++++      smb  ! surface mass balance      ++++++++++++++++++"
  
if (def) then
    write(valuestr, *) params%mode
    write(io, *) "--mode "ELA",or "prescribed" to use smb data, to be implemented: "atmos" (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--mode "ELA",or "prescribed" to use smb data, to be implemented: "atmos" (type: character(len=256))"
endif

    
if (def) then
    write(valuestr, *) params%a
    write(io, *) "--a m/yr (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--a m/yr (type: real(kind=dp))"
endif

    
if (def) then
    write(valuestr, *) params%ELA
    write(io, *) "--ELA equilibrium line altitude (m) (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--ELA equilibrium line altitude (m) (type: real(kind=dp))"
endif

    
if (def) then
    write(valuestr, *) params%lapserate
    write(io, *) "--lapserate air temperature lapse rate (0.71^{o}C/100m) (deg/m) (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--lapserate air temperature lapse rate (0.71^{o}C/100m) (deg/m) (type: real(kind=dp))"
endif

    
if (def) then
    write(valuestr, *) params%smb_offset
    write(io, *) "--smb_offset SMB offset, valid in prescribed mode only (m / year) (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--smb_offset SMB offset, valid in prescribed mode only (m / year) (type: real(kind=dp))"
endif

    
if (def) then
    write(valuestr, *) params%c1
    write(io, *) "--c1 m/yr/K (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--c1 m/yr/K (type: real(kind=dp))"
endif

    
if (def) then
    write(valuestr, *) params%c2
    write(io, *) "--c2 m/yr/K (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--c2 m/yr/K (type: real(kind=dp))"
endif

    
if (def) then
    write(valuestr, *) params%filename
    write(io, *) "--filename to read from, in "prescribed" mode (relative to the input) (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--filename to read from, in "prescribed" mode (relative to the input) (type: character(len=256))"
endif

end subroutine

subroutine set_param_string_smb  ! surface mass balance (params, name, string)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the smb  ! surface mass balance type from a string argument
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(smb  ! surface mass balance_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: string
    integer :: iostat

    select case (name) 
    
case ('mode', 'smb  ! surface mass balance%mode')
    read(string, *, iostat=IOSTAT) params%mode
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "smb  ! surface mass balance%mode = ", params%mode
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --smb  ! surface mass balance%mode"
        else
            write(*,*) "ERROR converting string to character(len=256): --smb  ! surface mass balance%mode ",trim(string)
        endif
        stop
    endif

    
case ('a', 'smb  ! surface mass balance%a')
    read(string, *, iostat=IOSTAT) params%a
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "smb  ! surface mass balance%a = ", params%a
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --smb  ! surface mass balance%a"
        else
            write(*,*) "ERROR converting string to real(kind=dp): --smb  ! surface mass balance%a ",trim(string)
        endif
        stop
    endif

    
case ('ELA', 'smb  ! surface mass balance%ELA')
    read(string, *, iostat=IOSTAT) params%ELA
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "smb  ! surface mass balance%ELA = ", params%ELA
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --smb  ! surface mass balance%ELA"
        else
            write(*,*) "ERROR converting string to real(kind=dp): --smb  ! surface mass balance%ELA ",trim(string)
        endif
        stop
    endif

    
case ('lapserate', 'smb  ! surface mass balance%lapserate')
    read(string, *, iostat=IOSTAT) params%lapserate
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "smb  ! surface mass balance%lapserate = ", params%lapserate
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --smb  ! surface mass balance%lapserate"
        else
            write(*,*) "ERROR converting string to real(kind=dp): --smb  ! surface mass balance%lapserate ",trim(string)
        endif
        stop
    endif

    
case ('smb_offset', 'smb  ! surface mass balance%smb_offset')
    read(string, *, iostat=IOSTAT) params%smb_offset
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "smb  ! surface mass balance%smb_offset = ", params%smb_offset
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --smb  ! surface mass balance%smb_offset"
        else
            write(*,*) "ERROR converting string to real(kind=dp): --smb  ! surface mass balance%smb_offset ",trim(string)
        endif
        stop
    endif

    
case ('c1', 'smb  ! surface mass balance%c1')
    read(string, *, iostat=IOSTAT) params%c1
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "smb  ! surface mass balance%c1 = ", params%c1
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --smb  ! surface mass balance%c1"
        else
            write(*,*) "ERROR converting string to real(kind=dp): --smb  ! surface mass balance%c1 ",trim(string)
        endif
        stop
    endif

    
case ('c2', 'smb  ! surface mass balance%c2')
    read(string, *, iostat=IOSTAT) params%c2
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "smb  ! surface mass balance%c2 = ", params%c2
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --smb  ! surface mass balance%c2"
        else
            write(*,*) "ERROR converting string to real(kind=dp): --smb  ! surface mass balance%c2 ",trim(string)
        endif
        stop
    endif

    
case ('filename', 'smb  ! surface mass balance%filename')
    read(string, *, iostat=IOSTAT) params%filename
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "smb  ! surface mass balance%filename = ", params%filename
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --smb  ! surface mass balance%filename"
        else
            write(*,*) "ERROR converting string to character(len=256): --smb  ! surface mass balance%filename ",trim(string)
        endif
        stop
    endif

    case default
      write(*,*) "ERROR set_param_string for smb  ! surface mass balance: unknown member :: ",trim(name)
      stop
    end select
end subroutine

function has_param_smb  ! surface mass balance (params, name) result(has_param)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Test whether an attribute name is member of the type smb  ! surface mass balance
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(smb  ! surface mass balance_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    logical :: has_param

    has_param = .true.
    select case (name) 
      
case ('mode', 'smb  ! surface mass balance%mode')

    
case ('a', 'smb  ! surface mass balance%a')

    
case ('ELA', 'smb  ! surface mass balance%ELA')

    
case ('lapserate', 'smb  ! surface mass balance%lapserate')

    
case ('smb_offset', 'smb  ! surface mass balance%smb_offset')

    
case ('c1', 'smb  ! surface mass balance%c1')

    
case ('c2', 'smb  ! surface mass balance%c2')

    
case ('filename', 'smb  ! surface mass balance%filename')

    case default
      has_param = .false.
    end select
end function



subroutine parse_command_argument_fjordmelt   ! submarine melting (params,i, iostat)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Maybe assign ith command line argument to the params type
    ! Input:
    !   params : the paramter type
    !   i : integer, positional argument of the parameter name
    ! Output:
    !   iostat, optional : 0 if success, 1 otherwise (if not
    !       provided, an error is thrown in case of failure)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(fjordmelt   ! submarine melting_t), intent(inout) :: params
    integer, intent(inout) :: i
    integer, optional :: iostat
    character(len=512) :: arg, argv

    call get_command_argument(i, arg)

    ! Print HELP ?
    if (arg == '--help' .or. arg=='-h') then
      call print_help_fjordmelt   ! submarine melting(params)
      return
    endif

    if (has_param_fjordmelt   ! submarine melting(params, trim(arg(3:)))) then
      ! +++++  present 
      call get_command_argument(i+1, argv)
      call set_param_string_fjordmelt   ! submarine melting(params, trim(arg(3:)), trim(argv))
      i = i+1
      if (present(iostat)) then
        iostat = 0
      endif
    else
      ! +++++  no found
      if (present(iostat)) then
        iostat=1
      else
        write(*,*) "ERROR: unknown parameter in fjordmelt   ! submarine melting : --",trim(arg(3:))
        write(*,*) ""
        write(*,*) "-h or --help for HELP"
        stop
      endif
    endif
end subroutine

subroutine print_help_fjordmelt   ! submarine melting(params, iounit, default)
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Print HELP on a derived type, useful for command-line arg
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  type(fjordmelt   ! submarine melting_t), intent(in) :: params
  integer, optional :: iounit
  integer :: io
  logical, optional :: default
  logical :: def
  character(len=2000) :: valuestr
  character(len=20) :: valuelen
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
  write(io, *) "+++++++++++++++++      fjordmelt   ! submarine melting      ++++++++++++++++++"
  
if (def) then
    write(valuestr, *) params%mode
    write(io, *) "--mode "prescribed", "off","constant" (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--mode "prescribed", "off","constant" (type: character(len=256))"
endif

    
if (def) then
    write(valuestr, *) params%scal
    write(io, *) "--scal experimental, to scale fjomarine melting (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--scal experimental, to scale fjomarine melting (type: real(kind=dp))"
endif

    
if (def) then
    write(valuestr, *) params%cal_melt
    write(io, *) "--cal_melt (m / yr) (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--cal_melt (m / yr) (type: real(kind=dp))"
endif

    
if (def) then
    write(valuestr, *) params%sub_rate
    write(io, *) "--sub_rate (m / yr) melt parameter in constant mode (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--sub_rate (m / yr) melt parameter in constant mode (type: real(kind=dp))"
endif

    
if (def) then
    write(valuestr, *) params%filename
    write(io, *) "--filename to read from, in "prescribed" mode (relative to the input) (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--filename to read from, in "prescribed" mode (relative to the input) (type: character(len=256))"
endif

end subroutine

subroutine set_param_string_fjordmelt   ! submarine melting (params, name, string)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the fjordmelt   ! submarine melting type from a string argument
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(fjordmelt   ! submarine melting_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: string
    integer :: iostat

    select case (name) 
    
case ('mode', 'fjordmelt   ! submarine melting%mode')
    read(string, *, iostat=IOSTAT) params%mode
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "fjordmelt   ! submarine melting%mode = ", params%mode
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --fjordmelt   ! submarine melting%mode"
        else
            write(*,*) "ERROR converting string to character(len=256): --fjordmelt   ! submarine melting%mode ",trim(string)
        endif
        stop
    endif

    
case ('scal', 'fjordmelt   ! submarine melting%scal')
    read(string, *, iostat=IOSTAT) params%scal
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "fjordmelt   ! submarine melting%scal = ", params%scal
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --fjordmelt   ! submarine melting%scal"
        else
            write(*,*) "ERROR converting string to real(kind=dp): --fjordmelt   ! submarine melting%scal ",trim(string)
        endif
        stop
    endif

    
case ('cal_melt', 'fjordmelt   ! submarine melting%cal_melt')
    read(string, *, iostat=IOSTAT) params%cal_melt
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "fjordmelt   ! submarine melting%cal_melt = ", params%cal_melt
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --fjordmelt   ! submarine melting%cal_melt"
        else
            write(*,*) "ERROR converting string to real(kind=dp): --fjordmelt   ! submarine melting%cal_melt ",trim(string)
        endif
        stop
    endif

    
case ('sub_rate', 'fjordmelt   ! submarine melting%sub_rate')
    read(string, *, iostat=IOSTAT) params%sub_rate
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "fjordmelt   ! submarine melting%sub_rate = ", params%sub_rate
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --fjordmelt   ! submarine melting%sub_rate"
        else
            write(*,*) "ERROR converting string to real(kind=dp): --fjordmelt   ! submarine melting%sub_rate ",trim(string)
        endif
        stop
    endif

    
case ('filename', 'fjordmelt   ! submarine melting%filename')
    read(string, *, iostat=IOSTAT) params%filename
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "fjordmelt   ! submarine melting%filename = ", params%filename
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --fjordmelt   ! submarine melting%filename"
        else
            write(*,*) "ERROR converting string to character(len=256): --fjordmelt   ! submarine melting%filename ",trim(string)
        endif
        stop
    endif

    case default
      write(*,*) "ERROR set_param_string for fjordmelt   ! submarine melting: unknown member :: ",trim(name)
      stop
    end select
end subroutine

function has_param_fjordmelt   ! submarine melting (params, name) result(has_param)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Test whether an attribute name is member of the type fjordmelt   ! submarine melting
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(fjordmelt   ! submarine melting_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    logical :: has_param

    has_param = .true.
    select case (name) 
      
case ('mode', 'fjordmelt   ! submarine melting%mode')

    
case ('scal', 'fjordmelt   ! submarine melting%scal')

    
case ('cal_melt', 'fjordmelt   ! submarine melting%cal_melt')

    
case ('sub_rate', 'fjordmelt   ! submarine melting%sub_rate')

    
case ('filename', 'fjordmelt   ! submarine melting%filename')

    case default
      has_param = .false.
    end select
end function



subroutine parse_command_argument_basalmelt ! basal melting (params,i, iostat)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Maybe assign ith command line argument to the params type
    ! Input:
    !   params : the paramter type
    !   i : integer, positional argument of the parameter name
    ! Output:
    !   iostat, optional : 0 if success, 1 otherwise (if not
    !       provided, an error is thrown in case of failure)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(basalmelt ! basal melting_t), intent(inout) :: params
    integer, intent(inout) :: i
    integer, optional :: iostat
    character(len=512) :: arg, argv

    call get_command_argument(i, arg)

    ! Print HELP ?
    if (arg == '--help' .or. arg=='-h') then
      call print_help_basalmelt ! basal melting(params)
      return
    endif

    if (has_param_basalmelt ! basal melting(params, trim(arg(3:)))) then
      ! +++++  present 
      call get_command_argument(i+1, argv)
      call set_param_string_basalmelt ! basal melting(params, trim(arg(3:)), trim(argv))
      i = i+1
      if (present(iostat)) then
        iostat = 0
      endif
    else
      ! +++++  no found
      if (present(iostat)) then
        iostat=1
      else
        write(*,*) "ERROR: unknown parameter in basalmelt ! basal melting : --",trim(arg(3:))
        write(*,*) ""
        write(*,*) "-h or --help for HELP"
        stop
      endif
    endif
end subroutine

subroutine print_help_basalmelt ! basal melting(params, iounit, default)
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! Print HELP on a derived type, useful for command-line arg
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  type(basalmelt ! basal melting_t), intent(in) :: params
  integer, optional :: iounit
  integer :: io
  logical, optional :: default
  logical :: def
  character(len=2000) :: valuestr
  character(len=20) :: valuelen
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
  write(io, *) "+++++++++++++++++      basalmelt ! basal melting      ++++++++++++++++++"
  
if (def) then
    write(valuestr, *) params%bmelt
    write(io, *) "--bmelt (m/yr) basal melting (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--bmelt (m/yr) basal melting (type: real(kind=dp))"
endif

    
if (def) then
    write(valuestr, *) params%filename
    write(io, *) "--filename input data file, for later (default: ",trim(adjustl(valuestr))," )"
else
    write(io, *) "--filename input data file, for later (type: character(len=256))"
endif

end subroutine

subroutine set_param_string_basalmelt ! basal melting (params, name, string)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Set one field of the basalmelt ! basal melting type from a string argument
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(basalmelt ! basal melting_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: string
    integer :: iostat

    select case (name) 
    
case ('bmelt', 'basalmelt ! basal melting%bmelt')
    read(string, *, iostat=IOSTAT) params%bmelt
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "basalmelt ! basal melting%bmelt = ", params%bmelt
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --basalmelt ! basal melting%bmelt"
        else
            write(*,*) "ERROR converting string to real(kind=dp): --basalmelt ! basal melting%bmelt ",trim(string)
        endif
        stop
    endif

    
case ('filename', 'basalmelt ! basal melting%filename')
    read(string, *, iostat=IOSTAT) params%filename
    if (VERBOSE .or. IOSTAT/=0) write(*,*) "basalmelt ! basal melting%filename = ", params%filename
    if (IOSTAT /= 0) then 
        if (trim(string) == "") then
            write(*,*) "ERROR: missing parameter value for --basalmelt ! basal melting%filename"
        else
            write(*,*) "ERROR converting string to character(len=256): --basalmelt ! basal melting%filename ",trim(string)
        endif
        stop
    endif

    case default
      write(*,*) "ERROR set_param_string for basalmelt ! basal melting: unknown member :: ",trim(name)
      stop
    end select
end subroutine

function has_param_basalmelt ! basal melting (params, name) result(has_param)
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Test whether an attribute name is member of the type basalmelt ! basal melting
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    type(basalmelt ! basal melting_t), intent(inout) :: params
    character(len=*), intent(in) :: name
    logical :: has_param

    has_param = .true.
    select case (name) 
      
case ('bmelt', 'basalmelt ! basal melting%bmelt')

    
case ('filename', 'basalmelt ! basal melting%filename')

    case default
      has_param = .false.
    end select
end function



    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! SET / GET routines
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



    ! Type conversion

    ! =============================================================
    !
    ! Type conversion functions (Courtesy of Alex Robinson's nml module)
    ! ==> useful to read array (lists) from command list argument)
    !
    ! =============================================================

subroutine string_to_array_integer (string, value, iostat)

    implicit none 

    character(len=*), intent(IN) :: string 
    integer :: value(:)
    character(len=256) :: tmpvec(size(value))
    character(len=256) :: tmpstr, fmt 
    integer, optional :: iostat
    integer :: stat, n, q, q1, q2, j 

    tmpstr = trim(adjustl(string))
    n      = len_trim(tmpstr)+2

    tmpvec(:) = "" 

    q1 = 1 
    do q = 1, size(tmpvec)
        q2 = index(tmpstr(q1:n)," ") + q1
        if (q2 .gt. q1 .and. q2 .le. n) then 
            tmpvec(q) = tmpstr(q1:q2-1)
            q1 = q2

            ! Make sure gaps of more than one space are properly handled
            do j = 1, 1000
                if (tmpstr(q1:q1) == " ") q1 = q1+1
                if (q1 .ge. n) exit 
            end do 

            ! Remove quotes around string if they exist 
            call remove_quotes_comma(tmpvec(q))

            read(tmpvec(q), *, iostat=iostat) value(q)
        
        end if 
    end do 
end subroutine

subroutine string_to_array_double (string, value, iostat)

    implicit none 

    character(len=*), intent(IN) :: string 
    real(dp) :: value(:)
    character(len=256) :: tmpvec(size(value))
    character(len=256) :: tmpstr, fmt 
    integer, optional :: iostat
    integer :: stat, n, q, q1, q2, j 

    tmpstr = trim(adjustl(string))
    n      = len_trim(tmpstr)+2

    tmpvec(:) = "" 

    q1 = 1 
    do q = 1, size(tmpvec)
        q2 = index(tmpstr(q1:n)," ") + q1
        if (q2 .gt. q1 .and. q2 .le. n) then 
            tmpvec(q) = tmpstr(q1:q2-1)
            q1 = q2

            ! Make sure gaps of more than one space are properly handled
            do j = 1, 1000
                if (tmpstr(q1:q1) == " ") q1 = q1+1
                if (q1 .ge. n) exit 
            end do 

            ! Remove quotes around string if they exist 
            call remove_quotes_comma(tmpvec(q))

            read(tmpvec(q), *, iostat=iostat) value(q)
        
        end if 
    end do 
end subroutine

subroutine string_to_array_logical (string, value, iostat)

    implicit none 

    character(len=*), intent(IN) :: string 
    logical :: value(:)
    character(len=256) :: tmpvec(size(value))
    character(len=256) :: tmpstr, fmt 
    integer, optional :: iostat
    integer :: stat, n, q, q1, q2, j 

    tmpstr = trim(adjustl(string))
    n      = len_trim(tmpstr)+2

    tmpvec(:) = "" 

    q1 = 1 
    do q = 1, size(tmpvec)
        q2 = index(tmpstr(q1:n)," ") + q1
        if (q2 .gt. q1 .and. q2 .le. n) then 
            tmpvec(q) = tmpstr(q1:q2-1)
            q1 = q2

            ! Make sure gaps of more than one space are properly handled
            do j = 1, 1000
                if (tmpstr(q1:q1) == " ") q1 = q1+1
                if (q1 .ge. n) exit 
            end do 

            ! Remove quotes around string if they exist 
            call remove_quotes_comma(tmpvec(q))

            read(tmpvec(q), *, iostat=iostat) value(q)
        
        end if 
    end do 
end subroutine
subroutine string_to_array_string (string, value, iostat)

    implicit none 

    character(len=*), intent(IN) :: string 
    character(len=*) :: value(:)
    character(len=256) :: tmpvec(size(value))
    character(len=256) :: tmpstr, fmt 
    integer, optional :: iostat
    integer :: stat, n, q, q1, q2, j 

    tmpstr = trim(adjustl(string))
    n      = len_trim(tmpstr)+2

    tmpvec(:) = "" 

    q1 = 1 
    do q = 1, size(tmpvec)
        q2 = index(tmpstr(q1:n)," ") + q1
        if (q2 .gt. q1 .and. q2 .le. n) then 
            tmpvec(q) = tmpstr(q1:q2-1)
            q1 = q2

            ! Make sure gaps of more than one space are properly handled
            do j = 1, 1000
                if (tmpstr(q1:q1) == " ") q1 = q1+1
                if (q1 .ge. n) exit 
            end do 

            ! Remove quotes around string if they exist 
            call remove_quotes_comma(tmpvec(q))

            read(tmpvec(q), *, iostat=iostat) value(q)
        
        end if 
    end do 
end subroutine

subroutine remove_quotes_comma(string)

    implicit none 
    character(len=*), intent(INOUT) :: string 
    integer :: i, n 

    ! Eliminate quotes
    n = len_trim(string)
    do i = 1,n 
        if (string(i:i) == '"' .or. string(i:i) == "'") string(i:i) = " "
    end do 
    string = trim(adjustl(string))

    ! Remove final comma too
    n = len_trim(string)
    if (n > 0) then 
        if (string(n:n) == ",") string(n:n) = " "
        string = trim(adjustl(string))
    end if 
    
    return 

end subroutine remove_quotes_comma




end module ioparams


