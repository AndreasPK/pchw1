! Compilers for Parallel Systems
! 185.A64 SS 2018 H. Moritsch
! F90 generated  from EFL source

program test1

integer, dimension(1:10,1:10) :: a
integer, dimension(1:10,1:10) :: b
integer :: i
integer :: j

901 do i = 2, 4
902     do j = 1, 3
            write(*,*) '001 a DEF ',i,j
001         a(i,j) = 100*i+j
            write(*,*) '002 b DEF ',i,10-j+1
            write(*,*) '002 a USE ',i-1,j
002         b(i,10-j+1) = a(i-1,j)*10
        end do
    end do

end program test1
