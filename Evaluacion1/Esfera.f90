!
!! cilindro.f90
!! 
!! Made by (Edwin Herrera Gallegos)
!! Login   <edwin@ltsp141.example.com>
!! 
!! Started on  Mon Oct 30 11:15:44 2017 Edwin Herrera Gallegos
!! Last update Time-stamp: <30-oct-2017 11:46:25 edwin>
!

program sphere

! Calculate the surface area of a cylinder.
!
! Declare variables and constants.
! constants=pi
! variables=radius squared and height

  implicit none

! Require all variables to be explicitly declared

  integer :: ierr
  character(1) :: yn
  real :: radius, area, volume
  real, parameter :: pi = 3.141592653589793

  interactive_loop: do

!   Prompt the user for radius
!   and read them.

    write (*,*) 'Enter radius.'
    read (*,*,iostat=ierr) radius

!   If radius  could not be read from input,
!   then cycle through the loop.

    if (ierr /= 0) then
      write(*,*) 'Error, invalid input.'
      cycle interactive_loop
    end if

!   Compute area.  The ** means "raise to a power."

    area = 4 * pi * (radius ** 2) 
    volume = (4 * pi * (radius ** 3) / 3)
    

!   Write the input variable (radius)
!   and output (area and volume) to the screen.

    write (*,'(1x,a7,f6.2,5x,a7,f8.2,5x,a5,f8.2)') &
      'radius=',radius,'area=',area, 'volume=',volume

    yn = ' '
    yn_loop: do
      write(*,*) 'Perform another calculation? y[n]'
      read(*,'(a1)') yn
      if (yn=='y' .or. yn=='Y') exit yn_loop
      if (yn=='n' .or. yn=='N' .or. yn==' ') exit interactive_loop
    end do yn_loop

  end do interactive_loop

end program sphere
