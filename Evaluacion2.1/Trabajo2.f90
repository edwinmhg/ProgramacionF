!
!! Trabajo2.f90
!! 
!! Made by (Edwin Herrera Gallegos)
!! Login   <edwin@ltsp173.example.com>
!! 
!! Started on  Thu Dec  7 17:37:05 2017 Edwin Herrera Gallegos
!! Last update Time-stamp: <07-dic-2017 17:38:14 edwin>
  !
	 

program expty
	real, dimension (15) :: f
	integer :: i, j, n
	real, dimension (100)   :: x
	real, dimension (100) :: exptay
        real, dimension (100) :: funcion
	real :: fi, fj, term, partial_sum

     open (unit=1, file = 'exp.dat', status = 'unknown')
	
	do n=1, 15, 2

	   do i=0, 100, 1

	   fi = float(i)
	   fi = fi / 10

	     call exptaylor (n, j, fi, fj, exptay)

	     funcion(n) = exptay(n)

	     write (1,*) fi, funcion(n)

	   end do

write (1,*) ' '

        end do

     close (unit=1)

end program expty


subroutine exptaylor (n, j, fi, fj, exptay)
	
        integer, intent (in)      :: n
	real, intent (in) :: fi
	integer :: j
	real, dimension (100), intent(out) :: exptay
	real :: fj, term, partial_sum
	
	term = 1

	partial_sum = term

	do j = 1, n
	 fj = float(j)

	  term = term * fi / fj

	  partial_sum = partial_sum + term

	  exptay(j) = partial_sum

	end do
 
end subroutine exptaylor

