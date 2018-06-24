! Compilers for Parallel Systems
! 185.A64 SS 2018 H. Moritsch
! F90 generated  from EFL source

program test1

integer, dimension(1:50,1:50) :: a
integer, dimension(1:50,1:50) :: b
integer :: i
integer :: j
integer :: t

901 do i = 20, 21
902     do j = 20, 21
001         t = i*2+a(t,i)
            write(*,*) '001 a USE ',t,i
            write(*,*) '002 b DEF ',a(i+1,j-1),50-j+1
002         b(a(i+1,j-1),50-j+1) = a(i-1,j)+a(i,b(i+2,j-3))
            write(*,*) '002 a USE ',i+1,j-1
            write(*,*) '002 a USE ',i+1,j-1
            write(*,*) '002 a USE ',i-1,j
            write(*,*) '002 b USE ',i+2,j-3
            write(*,*) '002 a USE ',i,b(i+2,j-3)
            write(*,*) '002 b USE ',i+2,j-3
        end do
    end do

end program test1
