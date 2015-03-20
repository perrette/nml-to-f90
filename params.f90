module params

    implicit none 

    type pars_group1 
        character(len=256) :: string1="", string2="", stringarr1(3)=""
        logical :: logical1 
        integer :: integer1, integer2 
    end type 

    type pars_group2 
        character(len=256) :: string1="", string2="", stringarr1(3)=""
        logical :: logical1 
        integer :: integer1, integer2, intarr1(10)
        double precision :: double1
        double precision :: dblarr1(6)
        logical :: logarr1(5)
    end type 

  contains

    ! subroutine write_nml_pars_group1(iounit, par1)
    !   type(pars_group1) :: par1
    !   integer, intent(in) :: iounit
    !   namelist /pars_group1/ par1
    !   write(iounit, pars_group1)
    ! end subroutine
    !
    ! subroutine write_nml_pars_group2(iounit, par2)
    !   type(pars_group2) :: par2
    !   integer, intent(in) :: iounit
    !   namelist /pars_group2/ par2
    !   write(iounit, pars_group2)
    ! end subroutine

end module
