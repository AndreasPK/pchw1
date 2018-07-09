! Compilers for Parallel Systems
! 185.A64 SS 2018 A. Klebinger
! Generated from EFL IR

program vec_example

real, dimension(1:20, 1:20) :: a
real, dimension(1:20, 1:20) :: b
real, dimension(1:20, 1:20) :: c
real, dimension(1:20, 1:20) :: d
integer :: i
integer :: j

    write(*,*) '901 i loop_begin'
901  do i = 2 , 20-1 ,1
        write(*,*) '901 i iteration_start ', i
        write(*,*) '902 j loop_begin'
902      do j = 3 , 20-4 ,1
            write(*,*) '902 j iteration_start ', j
            write(*,*) '001 c DEF ', i, j
            write(*,*) '001 a USE ', i, j
            write(*,*) '001 i USE '
            write(*,*) '001 j USE '
            write(*,*) '001 b USE ', i, j
001         c(i, j) = a(i, j)*b(i, j)
            write(*,*) '002 a DEF ', i+1, j+1
            write(*,*) '002 c USE ', i, j-2
            write(*,*) '002 i USE '
            write(*,*) '002 j USE '
            write(*,*) '002 c USE ', i-1, j
002         a(i+1, j+1) = c(i, j-2)/2.000000+c(i-1, j)*3.000000
            write(*,*) '003 d DEF ', i, j
            write(*,*) '003 d USE ', i-1, j-1
            write(*,*) '003 i USE '
            write(*,*) '003 j USE '
003         d(i, j) = d(i-1, j-1)+1.000000
            write(*,*) '004 b DEF ', i, j+4
            write(*,*) '004 d USE ', i, j
            write(*,*) '004 i USE '
            write(*,*) '004 j USE '
004         b(i, j+4) = d(i, j)-1.000000
            write(*,*) '902 j iteration_end ', j
        end do
        write(*,*) '902 j loop_end'
        write(*,*) '901 i iteration_end ', i
    end do
    write(*,*) '901 i loop_end'

end program vec_example
