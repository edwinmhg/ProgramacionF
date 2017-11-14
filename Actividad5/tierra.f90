!
!! tierra.f90
!! 
!! Made by (Edwin Herrera Gallegos)
!! Login   <edwin@ltsp157.example.com>
!! 
!! Started on  Mon Nov 13 11:11:44 2017 Edwin Herrera Gallegos
!! Last update Time-stamp: <13-nov-2017 12:52:24 edwin>

!Declaramos variables

program begin

  implicit none
  double precision :: fi
  double precision, parameter :: r=1.496d8, pi=3.1416d0  
  integer :: i
  integer, parameter :: ntimes = 360
  double precision, dimension(1000) :: x, y
  
  
 
  open (unit=1, file ='datos.dat', status = 'unknown')
  
  do i = 1, ntimes, 1
     fi = dble(i)

!Convertimos el angulo a radianes.
     fi = fi * pi / 180d0
    
!Calculamos la posicion.

     x(i) = r * dcos(fi)      
     y(i) = r * dsin(fi) 

     write (1,*) x(i), y(i)
     write (1,*) ' '
     
  end do
  
  close (unit=1)
  
  end program
