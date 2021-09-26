program laplaceSerial
    use mpi
    use vars, only: T, Told, Imax, Jmax, nMax, e, Jmax_local, cPE, nPE, ierr

    implicit none

    DOUBLE PRECISION :: tStart, &       ! start time
                        tEnd, &         ! end time
                        dTMax,  &
                        dTMax_global = 99      ! maximum allowed temperature difference
    integer ::  n = 1, &    ! iteration number
                i, &        ! i node
                j           ! j node
    
    ! MPI variables ( rest is defined in vars module)
    integer :: status(MPI_STATUS_SIZE)

    nPE = 1
    cPE = 1
    ! Initialize MPI 
    call MPI_Init(ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, cPE, ierr)   ! PE count
    call MPI_Comm_rank(MPI_COMM_WORLD, nPE, ierr)   ! PE number

    ! get start time
    call cpu_time(tStart)

    ! initialize Domain and BCs
    call init   

    ! print header
    CALL MPI_Barrier(mpi_comm_world,ierr)
    if (nPE == 0) then
        write(*,"(2a12)") 'iter', 'dTmax'
    end if

    ! iterate until reach max n of iterations or temp diff is less than e
    do while (n <= nMax .and. dTMax_global > e)

        ! calculate new temp values at each node except at BCs
        do j = 1, Jmax_local
            do i = 1, Imax
                T(i, j) = 0.25d0 * (Told(i+1, j) + Told(i-1, j) + Told(i, j-1) + Told(i, j+1))
            end do
        end do

        ! communicate ghost nodes between the PEs
        ! send data UP 
        if ( nPE /= cPE-1 ) then
            call MPI_SEND(T(1, Jmax_local), Imax, MPI_DOUBLE_PRECISION, nPE+1, 1, MPI_COMM_WORLD, ierr)
        end if
        ! recieve data UP
        if ( nPE /= 0 ) then
            call MPI_RECV(T(1,0), Imax, MPI_DOUBLE_PRECISION, MPI_ANY_SOURCE, 1, MPI_COMM_WORLD, status, ierr)
        end if
        ! send date DOWN
        if ( nPE /= 0 ) then
            call MPI_SEND(T(1,1), Imax, MPI_DOUBLE_PRECISION, nPE-1, 2, MPI_COMM_WORLD, ierr)
        end if
        ! recieve data DOWN
        if ( nPE /= cPE-1 ) then
            call MPI_RECV(T(1,Jmax_local+1), Imax, MPI_DOUBLE_PRECISION, MPI_ANY_SOURCE, 2, MPI_COMM_WORLD, status, ierr)
        end if

        ! calculate maximum temp difference btw old and new temps
        dTMax = MAXVAL(abs(T-Told))

        ! reduce dTMax to get global maximum btw all PEs and broadcast it to all PEs
        call MPI_REDUCE(dTMax, dTMax_global, 1, MPI_DOUBLE_PRECISION, MPI_MAX, 0, MPI_COMM_WORLD, ierr)
        call MPI_BCAST(dTMax_global, 1, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, ierr)

        ! feedback each 50 iterations
        if (MOD(n,100) == 0) then
            if ( nPE == 0 ) print "(i12, 4es12.3)", n, dTMax_global
        end if

        ! update old temp values 
        Told = T

        ! update current iteration number
        n = n + 1 
    end do

    ! calculate and print the total run time 
    CALL MPI_Barrier(mpi_comm_world,ierr)
    call cpu_time(tEnd)
    if ( nPE == 0 ) print *, 'Total run time is ', tEnd-tStart, ' seconds.'

    ! export data in one vtk file
    if ( nPE == 0 ) print *, 'Exporting to vtk ..'
    call writeVTKParallel
    CALL MPI_Barrier(mpi_comm_world,ierr)
    if ( nPE == 0 ) print *, 'Done.'

    ! end MPI
    call MPI_Finalize(ierr)

end program laplaceSerial

