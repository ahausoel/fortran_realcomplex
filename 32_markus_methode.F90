module mymod

    type :: dTBath
        real :: x
        real :: xsquare
    end type

    type :: zTBath
        complex :: x
        complex :: xsquare
    end type

    type :: bath
        type(dTBath), allocatable :: dBath
        type(zTBath), allocatable :: zBath
    end type

contains

    type(bath) function generate_bath(ch) result(this)
        logical :: ch

        if(.not.ch)then
            allocate(this%dBath)
            write(*,*) "alloc real"
            this%dBath%x = 2.0
        else
            allocate(this%zBath)
            write(*,*) "alloc compl"
            this%zBath%x = (2.0, 0.0)
        endif

    end function

end module

module mymoves
    use mymod

contains

    subroutine square(mybath, ch)
        type(bath) :: mybath
        logical :: ch

        if(ch)then
            mybath%zbath%xsquare = mybath%zbath%x**2
        else
            mybath%dbath%xsquare = mybath%dbath%x**2
        endif

    end subroutine

end module

program test
    use mymod
    use mymoves
    implicit none

    type(bath) :: mybath
    logical :: ch
    ch = .false.

    mybath = generate_bath(ch)

    ! Now the real functions use mybath%dbath%x 
    ! and the complex ones mybath%zbath%x.
    ! Is this correct?

    call square(mybath, ch)
    write(*,*) "mybath%dbath%xsquare", mybath%dbath%xsquare

    ! I don't see, how here generic subroutines 
    ! dsquare and zsquare could be called...

end program
