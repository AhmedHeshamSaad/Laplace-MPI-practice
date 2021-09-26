subroutine init
    use vars, only: T, Told, Imax, Jmax, Tsource
    implicit none

    integer :: i, j

    ! initiatize domain to 0
    Told = 0.0d0

    ! left and top BCs temp is 0

    ! set bottom BC temp as linear gradient
    Told(:, 0) = Tsource / Imax * [ (i, i=0, Imax+1) ]
    ! print*, Told(:, 0)

    ! set right BC temp as linear gradient
    Told(Imax+1, :) = 100 - Tsource / Jmax * [ (j, j=0, Jmax+1) ]
    ! print*, Told(Imax+1, :)

    T = Told

end subroutine init