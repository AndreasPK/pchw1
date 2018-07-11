! Compilers for Parallel Systems
! 185.A64 SS 2018 A. Klebinger
! Generated from EFL IR

program jacobi

real, dimension(1:10, 1:10) :: u
real, dimension(1:10, 1:10) :: f
real, dimension(1:10, 1:10) :: uhelp
real :: omega
real :: diff
real :: norm
integer :: i
integer :: j
integer :: iter


    write(*,*) '001 omega DEF '
001 omega = 0.500000
    write(*,*) '901 iter loop_begin'
901  do iter = 1 , 4 ,1
        write(*,*) '901 iter iteration_start ', iter
        write(*,*) '902 i loop_begin'
902      do i = 2 , 10-1 ,1
            write(*,*) '902 i iteration_start ', i
            write(*,*) '903 j loop_begin'
903          do j = 2 , 10-1 ,1
                write(*,*) '903 j iteration_start ', j
                write(*,*) '002 uhelp DEF ', i, j
                write(*,*) '002 omega USE '
                write(*,*) '002 u USE ', i, j
                write(*,*) '002 i USE '
                write(*,*) '002 j USE '
                write(*,*) '002 f USE ', i, j
                write(*,*) '002 u USE ', i-1, j
                write(*,*) '002 u USE ', i+1, j
                write(*,*) '002 u USE ', i, j+1
                write(*,*) '002 u USE ', i, j-1
002             uhelp(i, j) = (1.000000-omega)*u(i, j)+omega*0.250000*(f(i, j)+u(i-1, j)+u(i+1, j)+u(i, j+1)+u(i, j-1))
                write(*,*) '903 j iteration_end ', j
            end do
            write(*,*) '903 j loop_end'
            write(*,*) '902 i iteration_end ', i
        end do
        write(*,*) '902 i loop_end'
        write(*,*) '904 i loop_begin'
904      do i = 2 , 10-1 ,1
            write(*,*) '904 i iteration_start ', i
            write(*,*) '905 j loop_begin'
905          do j = 2 , 10-1 ,1
                write(*,*) '905 j iteration_start ', j
                write(*,*) '003 diff DEF '
                write(*,*) '003 uhelp USE ', i, j
                write(*,*) '003 i USE '
                write(*,*) '003 j USE '
                write(*,*) '003 u USE ', i, j
003             diff = uhelp(i, j)-u(i, j)
                write(*,*) '004 norm DEF '
                write(*,*) '004 norm USE '
                write(*,*) '004 diff USE '
004             norm = norm+diff*diff
                write(*,*) '905 j iteration_end ', j
            end do
            write(*,*) '905 j loop_end'
            write(*,*) '904 i iteration_end ', i
        end do
        write(*,*) '904 i loop_end'
        write(*,*) '906 i loop_begin'
906      do i = 1 , 10 ,1
            write(*,*) '906 i iteration_start ', i
            write(*,*) '907 j loop_begin'
907          do j = 1 , 10 ,1
                write(*,*) '907 j iteration_start ', j
                write(*,*) '005 u DEF ', i, j
                write(*,*) '005 uhelp USE ', i, j
                write(*,*) '005 i USE '
                write(*,*) '005 j USE '
005             u(i, j) = uhelp(i, j)
                write(*,*) '907 j iteration_end ', j
            end do
            write(*,*) '907 j loop_end'
            write(*,*) '906 i iteration_end ', i
        end do
        write(*,*) '906 i loop_end'
        write(*,*) '901 iter iteration_end ', iter
    end do
    write(*,*) '901 iter loop_end'

end program jacobi
