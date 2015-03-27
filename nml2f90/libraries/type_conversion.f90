module type_conversion
! =============================================================
!
! Type conversion functions (Courtesy of Alex Robinson's nml module)
! ==> useful to read array (lists) from command list argument)
!
! =============================================================

  integer, parameter :: Float = kind(0.d0)

    interface string_to_array
      module procedure :: string_to_array_integer
      module procedure :: string_to_array_double
      module procedure :: string_to_array_string
      module procedure :: string_to_array_logical
    end interface

  contains


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
      real(Float) :: value(:)
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

end module
