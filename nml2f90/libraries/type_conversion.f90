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

    subroutine string_to_array_string_deprecated (string, value, iostat)
      !! this used to deal with white space as separator, but it was error prone
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

    subroutine string_to_array_string (string, value, iostat)

      implicit none 

      character(len=*), intent(IN) :: string 
      character(len=*) :: value(:)
      character(len=256) :: tmpvec(size(value))
      character(len=256) :: tmpstr
      integer, optional :: iostat
      integer :: stat, q, q2

      tmpstr = trim(adjustl(string))
      tmpvec(:) = "" 

      do q=1,size(tmpvec)
        q2 = index(tmpstr,',')
        if (q2 == 0) then
          q2 = len(tmpstr)+1
          if (q /= size(tmpvec)) then
            write(*,*) "expected array of size",size(tmpvec),', got:', q
            iostat = -1
            return
          endif
        endif
        tmpvec(q) = tmpstr(:q2-1)
        tmpstr = tmpstr(q2+1:)
      enddo

    end subroutine

    subroutine string_to_array_integer (string, value, iostat)

      implicit none 

      character(len=*), intent(IN) :: string 
      integer :: value(:)
      integer, optional :: iostat
      character(len=256) :: tmpvec(size(value))

      call string_to_array_string (string, tmpvec, iostat)

      do q=1,size(tmpvec)
        read(tmpvec(q), *, iostat=iostat) value(q)
      enddo

    end subroutine

    subroutine string_to_array_double (string, value, iostat)

      implicit none 

      character(len=*), intent(IN) :: string 
      real(Float) :: value(:)
      integer, optional :: iostat
      character(len=256) :: tmpvec(size(value))

      call string_to_array_string (string, tmpvec, iostat)

      do q=1,size(tmpvec)
        read(tmpvec(q), *, iostat=iostat) value(q)
      enddo

    end subroutine

    subroutine string_to_array_logical (string, value, iostat)

      implicit none 

      character(len=*), intent(IN) :: string 
      logical :: value(:)
      integer, optional :: iostat
      character(len=256) :: tmpvec(size(value))

      call string_to_array_string (string, tmpvec, iostat)

      do q=1,size(tmpvec)
        read(tmpvec(q), *, iostat=iostat) value(q)
      enddo

    end subroutine

end module
