subroutine writeVtK
    use vars, only: Imax, Jmax, T
    implicit none
    
    INTEGER :: i, j

    ! open file, replace exists
    open(UNIT = 4, FILE = 'laplace-Solution.vtk', STATUS = 'REPLACE')

    ! write the vtk standard header
    write(4,"(a)")'# vtk DataFile Version 3.0'
    write(4,"(a)")'Laplace problem'
    write(4,"(a)")'ASCII'

    ! new line (not necessary)
    write(4,*)''

    write(4,"(a)")'DATASET STRUCTURED_GRID'
    write(4,"(a,i6,i6,i6)")'DIMENSIONS ', Imax+2, Jmax+2, 1
    write(4,"(a,i9,a)")'POINTS ', (Imax+2)*(Jmax+2),' FLOAT'
    do j = 0, Jmax+1
        do i = 0, Imax+1
            write(4,*) i,' ', j,' 0'
        end do 
    end do

    ! write temp values (node based data type)
    write(4,"(a)") ' '
    write(4,*) 'POINT_DATA ', (Imax+2)*(Jmax+2)
    write(4,"(a)") 'SCALARS T double'
    write(4,"(a)") 'LOOKUP_TABLE default'
    do j = 0, Jmax+1
        do i = 0, Imax+1
            write(4,*) T(i,j)
        end do
    end do 

end subroutine writeVTK