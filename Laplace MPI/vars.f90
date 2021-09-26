module vars
    implicit none

    integer, parameter ::   Imax = 1000, & 
                            Jmax = 1000, &  ! problem size
                            nMax = 1000000      ! max n of iterations

    DOUBLE PRECISION ::     e = 0.0000010d0, &     ! minimum temp diff
                            TSource = 100       ! right bottom corner temperature

    ! DOUBLE PRECISION, dimension(0:Imax+1, 0:Jmax+1) :: T, Told      ! temperatre and old temperature with ghost cells
    DOUBLE PRECISION, ALLOCATABLE, dimension(:, :) :: T, Told      ! temperatre and old temperature with ghost cells

    ! MPI variables
    integer ::  cPE, &
                nPE, &
                ierr, &
                Jmax_local, & ! n of rows for each PE
                r               ! FLOOR(real(Jmax) / real(cPE))

    ! integer :: status(MPI_STATUS_SIZE)

    
end module vars