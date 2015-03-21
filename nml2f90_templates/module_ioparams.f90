module {io_module_name}
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Automatically generated module by nml2f90.py from {input_nml}
    ! 
    ! Contains read / write subroutines for all derived types imported below.
    ! As well as setter / getter access by field name
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    implicit none

    private
    public :: {list_of_types}
    public :: read_nml, write_nml          ! nml I/O
    public :: set_param, get_param         ! generic set/get
    public :: parse_command_argument       ! parse and assign command-line arg
    public :: print_help                   ! print command-line argument help
    public :: has_param, set_param_string  ! useful fine-grained control on parse_command

    integer, parameter :: dp = kind(0.d0)
    logical :: VERBOSE = .TRUE.

    {type_definitions}

    interface read_nml
        {read_nml_proc}
    end interface

    interface write_nml
        {write_nml_proc}
    end interface

    interface parse_command_argument
        {parse_command_argument_proc}
    end interface

    interface print_help
        {print_help_proc}
    end interface

    interface has_param
        {has_param_proc}
    end interface

    interface set_param_string
        {set_param_string_proc}
    end interface

    interface set_param
        {set_param_proc}
    end interface

    interface get_param
        {get_param_proc}
    end interface

    interface string_to_vector
      module procedure :: string_to_vector_integer
      module procedure :: string_to_vector_double
      module procedure :: string_to_vector_string
      module procedure :: string_to_vector_logical
    end interface

contains

    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! IO routines
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

{io_routines}

    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Routines useful to process command-line parameters: 
    ! - has_param
    ! - set_param_string
    ! - parse_command_argument
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

{cmd_routines}

    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! SET / GET routines
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

{setget_routines}

    ! =============================================================
    !
    ! Type conversion functions (Courtesy of Alex Robinson's nml module)
    ! ==> useful to read vector (lists) from command list argument)
    !
    ! =============================================================

subroutine string_to_vector_integer (string, value, iostat)

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

subroutine string_to_vector_double (string, value, iostat)

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

subroutine string_to_vector_logical (string, value, iostat)

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
subroutine string_to_vector_string (string, value, iostat)

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


end module {io_module_name}


