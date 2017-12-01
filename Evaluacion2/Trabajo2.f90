!
!! Trabajo2.f90
!! 
!! Made by (Edwin Herrera Gallegos)
!! Login   <edwin@ltsp163.example.com>
!! 
!! Started on  Fri Dec  1 13:44:18 2017 Edwin Herrera Gallegos
!! Last update Time-stamp: <01-dic-2017 14:37:05 edwin>
!

! ----------- Begin ------------

!taylor.f90

program taylor

    implicit none
    real (kind=8), external :: exptaylor
    integer, parameter :: npts=100
    real, dimension(100) :: x, exp_true, y
    integer, dimension(15) :: n
    integer :: i, j, k

    open (unit=1, file='exptaylor', status='unknown')
    
    do i=1,npts
       x(i)= float(i)/(10.0)
    end do
    
    do j=1,15,2
       n(j) = j
       y = exptaylor(x,n(j))
    end do
    
    exp_true = exp(x)
    
    
    do i=1,npts

     write(1,*) x(i), y(i)
     write(1,*) ' '   

    end do
 
 close (unit=1)
 
end program taylor

!==========================
subroutine subtaylor(x,n)
!==========================
    implicit none

    ! function arguments:
    real (kind=8), intent(in) :: x
    integer, intent(in) :: n
    real (kind=8) :: exptaylor

    ! local variables:
    real (kind=8) :: term, partial_sum
    integer :: j

    term = 1.
    partial_sum = term

    do j=1,n
        ! j'th term is  x**j / j!  which is the previous term times x/j:
        term = term*x/j   
        ! add this term to the partial sum:
        partial_sum = partial_sum + term   
        end do
     exptaylor = partial_sum  ! this is the value returned
end subroutine subtaylor
! --------  End -------------
