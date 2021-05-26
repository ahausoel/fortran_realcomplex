module mymod

    type :: dTBath
        real :: x
    end type

    type :: zTBath
        complex :: x
    end type

    type :: bath
        type(dTBath), allocatable :: dBath
        type(zTBath), allocatable :: zBath
    end type

contains

    type(bath) function generate_bath(ch) result(this)
        logical :: ch

        if(ch)then
            allocate(this%dBath)
            write(*,*) "alloc real"
        else
            allocate(this%zBath)
            write(*,*) "alloc compl"
        endif

    end function

end module

program test
    use mymod
    implicit none

    type(bath) :: mybath
    logical :: ch
    ch = .true.
    ch = .false.

    mybath = generate_bath(ch)

end program
