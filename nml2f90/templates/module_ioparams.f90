! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Automatically generated module by nml2f90
! History: {command_call}
!
! https://github.com/perrette/nml-to-f90
! version: {version}
!  
! Features included : {features}
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

{libraries}

module {module_name}
  {description}

  {imports}

  implicit none

  private
  {public}
  public :: join_array
  public :: command_argument_as_array

  integer, parameter :: dp = {real_kind}
  integer, parameter :: ip = {int_kind}
  integer, parameter :: clen = {char_len}
  logical :: VERBOSE = {verbose}

  {definition}

contains

  function join_array(arr, sep, quotes) result(str)
      ! String representation of a character array
      character(len=*), intent(in) :: arr(:)
      character(len=len(arr)*size(arr)) :: str
      integer :: i, n
      logical, intent(in), optional :: quotes 
      character(len=*), intent(in), optional :: sep
      character(len=50) :: sep_opt
      character(len=1) :: q
      integer :: len_sep

      q = "'"
      if (present(quotes)) then
        if (.not. quotes) then
          q = ""
        endif
      endif

      if (present(sep)) then
        sep_opt = sep
        len_sep = len(sep)
      else
        sep_opt = ", "
        len_sep = 2
      endif

      str = ""
      do i=1,size(arr)
          str = trim(str)//sep_opt(:len_sep)//q//trim(arr(i))//q
      enddo
      if (size(arr) > 0) then
        str = str(len_sep+1:)
      endif
  end function

  subroutine command_argument_as_array(args)
      character(len=*), intent(out), allocatable :: args(:)
      integer :: i, n
      n = command_argument_count()
      allocate(args(n))
      do i=1,n
          call get_command_argument(i, args(i))
      enddo
  end subroutine

  {content}

end module {module_name}
