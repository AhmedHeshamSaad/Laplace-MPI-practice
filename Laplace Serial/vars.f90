module vars
    implicit none

    integer, parameter ::   Imax = 1000, & 
                            Jmax = 1000, &  ! problem size
                            nMax = 100      ! max n of iterations

    DOUBLE PRECISION ::     e = 0.0010d0, &     ! minimum temp diff
                            TSource = 100       ! right bottom corner temperature
    DOUBLE PRECISION, dimension(0:Imax+1, 0:Jmax+1) :: T, Told      ! temperatre and old temperature with ghost cells

    
end module vars