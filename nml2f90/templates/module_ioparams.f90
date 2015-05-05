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

  integer, parameter :: dp = {real_kind}
  integer, parameter :: ip = {int_kind}
  integer, parameter :: clen = {char_len}
  logical :: VERBOSE = {verbose}

  {definition}

contains

  {content}

end module {module_name}
