!
!! Trabajo1.f90
!! 
!! Made by (Edwin Herrera Gallegos)
!! Login   <edwin@ltsp163.example.com>
!! 
!! Started on  Fri Dec  1 11:15:29 2017 Edwin Herrera Gallegos
!! Last update Time-stamp: <01-dic-2017 11:17:13 edwin>
!

! ----------- Begin ------------

!taylor.f90

program taylor

    implicit none                  
real (kind=8) :: x, exp_true, y
    real (kind=8), external :: exptaylor
    integer :: n

    n = 20               ! number of terms to use
    x = 1.0
    exp_true = exp(x)
    y = exptaylor(x,n)   ! uses function below
    print *, "x = ",x
    print *, "exp_true  = ",exp_true
    print *, "exptaylor = ",y
    print *, "error     = ",y - exp_true

end program taylor

!==========================
function exptaylor(x,n)
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
end function exptaylor

! --------  End -------------
