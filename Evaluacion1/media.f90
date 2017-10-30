!
!! media.f90
!! 
!! Made by (Edwin Herrera Gallegos)
!! Login   <edwin@ltsp141.example.com>
!! 
!! Started on  Mon Oct 30 11:48:25 2017 Edwin Herrera Gallegos
!! Last update Time-stamp: <30-oct-2017 13:45:11 edwin>
!

 

! sum.f90
! Performs summations using in a loop using EXIT statement
! Saves input information and the summation in a data file


program summation
implicit none
integer :: j
real :: mean, a, i, mean2

print*, "This program performs Arithmetic and Harmonic means. Enter 0 to stop."
open(unit=10, file="MeanData.DAT")
mean2 = 0
mean = 0
i=0
  
do
 print*, "Add:"
 read*, a
 if (a == 0) then
  exit
   else
   
mean = ( mean + a )   
i = i + 1

mean2 = mean2 + (1/a)

end if

end do
mean = mean / i
mean2 = 1 / (mean2 /i)
 write(10,*) a, i

print*, "Media aritmetica =", mean, "i =", i,"Media armonica =", mean2
write(10,*) "Media aritmetica =", mean, "Media armonica =", mean2
close(10)

end program
