!
!! altura_max.f90
!! 
!! Made by (Edwin Herrera Gallegos)
!! Login   <edwin@ltsp85.example.com>
!! 
!! Started on  Thu Sep 14 15:12:36 2017 Edwin Herrera Gallegos
!! Last update Time-stamp: <14-sep-2017 15:23:17 edwin>
!

program projectile
  implicit none

  ! definimos constantes
  real, parameter :: g = 9.8
  real, parameter :: pi = 3.1415927

  ! definimos las variables
  real :: a, u, h
  real :: theta

  
  write(*,*) 'Dame una rapidez inicial y un angulo'
  read(*,*) u, a

  ! convirtiendo ángulo a radianes
  a = a * pi / 180.0
  
  ! las ecuaciones de la altura máxima.
  h =((u**2)* (sin(a)**2.))/( 2. * g)
 
  
 
 ! escribiendo el resultado en la pantalla
  write(*,*) 'h: ',h  

end program projectile
