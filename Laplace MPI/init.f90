subroutine init
    use vars, only: T, Told, Imax, Jmax, Tsource, cPE, nPE, Jmax_local, r
    implicit none

    integer :: i, j 

    r = FLOOR(real(Jmax) / real(cPE))

    ! define Jmax_local for each PE
    if ( nPE /= (cPE -1)) then
        Jmax_local = r
    else 
        Jmax_local = r + MOD(Jmax, cPE)
    end if

    write(*,*) 'PE', nPE, ' Jmax_local= ', Jmax_local

    ! allocate T and Told matricies
    ALLOCATE(T(0:Imax+1,0:Jmax_local+1), Told(0:Imax+1,0:Jmax_local+1))

    write(*,*) 'PE', nPE, ' says ok'

    ! initiatize domain to 0
    T = 0.0d0

    ! Keep left and top BCs temp equals to 0    

    ! set bottom BC temp as linear gradient (just for PE = 0)
    if ( nPE == 0 ) then
        T(:, 0) = Tsource / Imax * [ (i, i=0, Imax+1) ]
    end if
    
    ! set right BC temp as linear gradient
    T(Imax+1, :) = Tsource - Tsource / Jmax * ( [(j, j=0, Jmax_local+1)] + nPE*r )

    Told = T
end subroutine init