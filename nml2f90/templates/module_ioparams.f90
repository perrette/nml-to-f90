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
  public :: trim_array
  public :: command_argument_as_array

  integer, parameter :: dp = {real_kind}
  integer, parameter :: ip = {int_kind}
  integer, parameter :: clen = {char_len}
  logical :: VERBOSE = {verbose}

  {definition}

contains

  function trim_array(arr) result(str)
      character(len=*), intent(in) :: arr(:)
      character(len=len(arr)*size(arr)) :: str
      integer :: i, n
      str = ""
      do i=1,size(arr)
          str = trim(str)//" "//trim(arr(i))
      enddo
      str = adjustl(str)
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
