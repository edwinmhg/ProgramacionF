!
!! luna.f90
!! 
!! Made by (Edwin Herrera Gallegos)
!! Login   <edwin@ltsp156.example.com>
!! 
!! Started on  Thu Nov 23 17:17:10 2017 Edwin Herrera Gallegos
!! Last update Time-stamp: <30-nov-2017 20:12:52 edwin>
!

function solx(thetaL) result (x)
  double precision, intent(in) :: thetaL
  double precision :: x
  double precision, parameter :: Rsol = 1.49d8
  x = Rsol * dcos(thetaL)
end function solx

function soly(thetaL) result (y)
  double precision, intent(in) :: thetaL
  double precision :: y
  double precision,parameter :: Rsol = 1.49d8
  y = Rsol * dsin(thetaL)
end function soly

subroutine luna(Rsol, Rluna, Px, Py, thetaL, thetaS)
   double precision, intent (in) :: Rsol, thetaL, thetaS
   double precision, intent (out) :: Px, Py
   double precision :: Rluna
   Rluna = Rsol / 4.0d0
   Px = (Rsol * dcos(thetaS)) + (Rluna * dcos(thetaL))
   Py = (Rsol * dsin(thetaS)) + (Rluna * dsin(thetaL))
 
end subroutine luna 

program begin

  implicit none
  double precision :: fi, fj, Rsol, Rluna, Px, Py, thetaL, rd, dia
  double precision :: Vluna, Vsol, solx, soly, thetaS
  double precision, parameter :: pi=3.1416d0, Tsol = 360, Tluna = 28 
  integer :: j
  double precision, dimension(360) :: xtotal, ytotal
  double precision, dimension(360) :: x, y

  !Convertimos los angulos a radianes.
  rd = pi / 180d0
  !Definimos el radio del sol.
  Rsol = 1.496d8
  !Los días que pasan por 1 radian.
  dia = 365.26d0 / (360d0*rd)
  !Velocidad de la Luna.
  Vluna = 2d0 * (pi / Tluna)
  !Velocidad del Sol.
  Vsol = 2d0 * (pi / Tsol)

  
   open (unit=1, file = 'Luna-Tierra.dat', status = 'unknown')
   open (unit=2, file = 'Sol-Tierra.dat', status = 'unknown')

do j=1, 360, 1
   fj = dble(j)
   thetaS = fj * Vsol
   thetaL = fj * Vluna
   
   x(j)= solx(thetaS)
   y(j)= soly(thetaS)
   

   call luna(Rsol, Rluna, Px, Py, thetaL, thetaS)
   xtotal(j) = Px
   ytotal(j) = Py

     write (1,*) xtotal(j), ytotal(j)
     write (1,*) ' '
     write (2,*) x(j), y(j)
     write (2,*) ' '
     
end do
     
  close (unit=1)
  close (unit=2)
  
end program
