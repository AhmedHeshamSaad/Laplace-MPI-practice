subroutine writeVTK
    use vars, only: Imax, Jmax, T, nPE, cPE, Jmax_local
    implicit none
    
    INTEGER :: i, j
    character(len=100) :: FileName

    ! open file, replace exists
    write (FileName, '(7a)') 'laplace-Solution',str(nPE),'of',str(cPE),'.vtk'
    open(UNIT = 4, FILE = trim(FileName), STATUS = 'REPLACE')

    ! write the vtk standard header
    write(4,"(a)")'# vtk DataFile Version 3.0'
    write(4,"(a)")'Laplace problem'
    write(4,"(a)")'ASCII'

    ! new line (not necessary)
    write(4,*)''

    write(4,"(a)")'DATASET STRUCTURED_GRID'
    write(4,"(a,i6,i6,i6)")'DIMENSIONS ', Imax+2, Jmax_local+2, 1
    write(4,"(a,i9,a)")'POINTS ', (Imax+2)*(Jmax_local+2),' FLOAT'
    do j = 0, Jmax_local+1
        do i = 0, Imax+1
            write(4,*) i,' ', j,' 0'
        end do 
    end do

    ! write temp values (node based data type)
    write(4,"(a)") ' '
    write(4,*) 'POINT_DATA ', (Imax+2)*(Jmax_local+2)
    write(4,"(a)") 'SCALARS T double'
    write(4,"(a)") 'LOOKUP_TABLE default'
    do j = 0, Jmax_local+1
        do i = 0, Imax+1
            write(4,*) T(i,j)
        end do
    end do 

contains

    function str(i) result(res)
        character(:),allocatable :: res
        integer,intent(in) :: i
        character(range(i)+2) :: tmp
        write(tmp,'(i0)') i
        res = trim(tmp)
    end function

end subroutine writeVTK

