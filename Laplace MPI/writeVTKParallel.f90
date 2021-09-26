subroutine writeVTKParallel
    use mpi
    use vars, only: Imax, Jmax, T, nPE, cPE, Jmax_local, ierr
    implicit none
    
    INTEGER :: i, j, w


    if (nPE == 0) then
        ! open file, replace exists
        open(UNIT = 4, FILE = 'laplace-Solution.vtk', STATUS = 'REPLACE')

        ! write the vtk standard header
        write(4,"(a)")'# vtk DataFile Version 3.0'
        write(4,"(a)")'Laplace problem'
        write(4,"(a)")'ASCII'

        ! new line (not necessary)
        write(4,*)''

        write(4,"(a)")'DATASET STRUCTURED_GRID'
        write(4,"(a,i6,i6,i6)")'DIMENSIONS ', Imax, Jmax, 1
        write(4,"(a,i9,a)")'POINTS ', Imax*Jmax,' FLOAT'
        do j = 1, Jmax
            do i = 1, Imax
                write(4,*) i,' ', j,' 0'
            end do 
        end do

        ! write temp values (node based data type)
        write(4,"(a)") ' '
        write(4,*) 'POINT_DATA ', Imax*Jmax
        write(4,"(a)") 'SCALARS T double'
        write(4,"(a)") 'LOOKUP_TABLE default'
        do j = 1, Jmax_local
            do i = 1, Imax
                write(4,*) T(i,j)
            end do
        end do 

        close(4)
    end if

    CALL MPI_Barrier(mpi_comm_world, ierr)

    do w = 1,cPE-1
        if ( nPE == w ) then
            open(UNIT = 4, FILE = 'laplace-Solution.vtk', POSITION = 'APPEND' , STATUS = 'OLD')

            do j = 1, Jmax_local
                do i = 1, Imax
                    write(4,*) T(i,j)
                end do
            end do 

            close(4)
        end if
        CALL MPI_Barrier(mpi_comm_world,ierr)
    end do

contains

    function str(i) result(res)
        character(:),allocatable :: res
        integer,intent(in) :: i
        character(range(i)+2) :: tmp
        write(tmp,'(i0)') i
        res = trim(tmp)
    end function

end subroutine writeVTKParallel

