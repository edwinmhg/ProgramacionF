|!
!! tiempo_vuelo.f90
!! 
!! Made by (Edwin Herrera Gallegos)
!! Login   <edwin@ltsp91.example.com>
!! 
!! Started on  Thu Sep  7 17:54:40 2017 Edwin Herrera Gallegos
!! Last update Time-stamp: <14-sep-2017 15:04:43 edwin>
!

program projectile
  implicit none

  ! definimos constantes
  real, parameter :: g = 9.8
  real, parameter :: pi = 3.1415927

  ! definimos las variables
  real :: a, t, u
  real :: theta

  ! Leer valores para el ángulo a, el tiempo t, y la velocidad inicial u desde la terminal
  write(*,*) 'Dame una rapidez inicial y un angulo'
  read(*,*) u, a

  ! convirtiendo ángulo a radianes
  a = a * pi / 180.0
  
  ! las ecuaciones del tiempo total
  t = 2 * u * sin(a) / g
 
  
 
 ! escribiendo el resultado en la pantalla
  write(*,*) 't: ',t , 'segudnos'

end program projectile
