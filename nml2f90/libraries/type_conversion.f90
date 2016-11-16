module type_conversion
! =============================================================
!
! Type conversion functions (Courtesy of Alex Robinson's nml module)
! ==> useful to read array (lists) from command list argument)
!
! UPDATE M.P. 20161116: string_to_array :: assume comma-separated values
!
! =============================================================

  integer, parameter :: Float = kind(0.d0)

    interface string_to_array
      !! array as comma-separated values
      module procedure :: string_to_array_string
      module procedure :: string_to_array_integer
      module procedure :: string_to_array_double
      module procedure :: string_to_array_logical
    end interface

  contains

    subroutine signal_error(iostat)
      integer, optional :: iostat
      if(present(iostat)) then
        iostat = -1
        return
      else
        stop
      endif
    end subroutine

    pure function strip_brackets(s1) result(s2)
      character(len=*), intent(in) :: s1
      character(len=256) :: s2

      if (len_trim(s1) < 2) return

      ! head
      if (s1(1:2) == "(/" .and. s1(len_trim(s1)-1:) == "/)") then
        s2 = adjustl(s1(3:len_trim(s1)-2))

      elseif (s1(1:1) == "[" .and. s1(len_trim(s1):len_trim(s1)) == "]") then
        s2 = adjustl(s1(2:len_trim(s1)-1))

      elseif (s1(1:1) == "(" .and. s1(len_trim(s1):len_trim(s1)) == ")") then
        s2 = adjustl(s1(2:len_trim(s1)-1))

      else
        s2 = s1

      endif

    end function

    subroutine string_to_array_string (string, value, iostat)

      character(len=*), intent(in) :: string
      character(len=*), intent(out) :: value(:)
      integer, optional :: iostat
      character(len=256) :: tmpstr
      integer :: q, q2

      tmpstr = strip_brackets(trim(adjustl(string)))

      do q=1,size(value)
        q2 = index(tmpstr,',')
        if (q2 == 0) then
          q2 = len(tmpstr)+1

          if (q /= size(value)) then
            write(*,*) "command-line :: expected array of size",size(value),', got:', q
            call signal_error(iostat)
            return 
          endif

        else

          if (q == size(value)) then
            write(*,*) "command-line :: array size exceeded",size(value)
            call signal_error(iostat)
            return
          endif

        endif

        read(tmpstr(:q2-1), *, iostat=iostat) value(q)
        if (present(iostat)) then
          if (iostat /= 0) return
        endif
        tmpstr = tmpstr(q2+1:)
      enddo

    end subroutine

    subroutine string_to_array_integer (string, value, iostat)

      implicit none 

      character(len=*), intent(in) :: string 
      integer, intent(out) :: value(:)
      integer, optional :: iostat
      character(len=256) :: tmpvec(size(value))
      integer :: q

      call string_to_array_string (string, tmpvec, iostat)
      if (present(iostat))then
        if (iostat /=0) return
      endif

      do q=1,size(tmpvec)
        read(tmpvec(q), *, iostat=iostat) value(q)
        if (present(iostat))then
          if (iostat /=0) return
        endif
      enddo

    end subroutine

    subroutine string_to_array_double (string, value, iostat)

      implicit none 

      character(len=*), intent(in) :: string 
      real(Float), intent(out) :: value(:)
      integer, optional :: iostat
      character(len=256) :: tmpvec(size(value))
      integer :: q

      call string_to_array_string (string, tmpvec, iostat)
      if (present(iostat))then
        if (iostat /=0) return
      endif

      do q=1,size(tmpvec)
        read(tmpvec(q), *, iostat=iostat) value(q)
        if (present(iostat))then
          if (iostat /=0) return
        endif
      enddo

    end subroutine

    subroutine string_to_array_logical (string, value, iostat)

      implicit none 

      character(len=*), intent(in) :: string 
      logical, intent(out) :: value(:)
      integer, optional :: iostat
      character(len=256) :: tmpvec(size(value))
      integer :: q

      call string_to_array_string (string, tmpvec, iostat)
      if (present(iostat))then
        if (iostat /=0) return
      endif

      do q=1,size(tmpvec)
        read(tmpvec(q), *, iostat=iostat) value(q)
        if (present(iostat))then
          if (iostat /=0) return
        endif
      enddo

    end subroutine

end module
