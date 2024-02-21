module laplace
!
!  created by Taras Kuzyo
!  as part of laplace-2D numerical code

!
!  defines and interface for the solution
!  subroutine and a number of different
!  solving subroutines using different methods
!
    use globals, only: debug, pi
    use utils, only: confargs
    implicit none

contains

    !
    ! A wrraper of the solving function.
    ! Calls laplace_setup_solver with the solver
    ! based on the value of args%solver
    !
    subroutine laplace_solve(u, src, args, nsteps)

        real(kind=8), intent(inout) :: u(:, :)
        real(kind=8), intent(in)    :: src(:, :)
        type(confargs), intent(in)  :: args
        integer, intent(out)        :: nsteps

        if (args%solver == 'SOR5') then
            call laplace_setup_solver(u, src,  SOR5, args, nsteps)
        else if (args%solver == 'SOR9') then
            call laplace_setup_solver(u, src,  SOR9, args, nsteps)
        else if (args%solver == 'SOR5-RB') then
            call laplace_setup_solver(u, src,  SOR5_red_black, args, nsteps)
        else
            stop "invalid solver set."
        end if

    end subroutine laplace_solve


    subroutine laplace_setup_solver(u, src, solver, args, nsteps)

        real(kind=8), intent(inout) :: u(:, :)
        real(kind=8), intent(in)    :: src(:, :)
        type(confargs), intent(in)  :: args
        integer, intent(out)        :: nsteps

        interface
            subroutine solver(u, src, dx, dy, change)
               real(kind=8), intent(inout) :: u(:, :)
               real(kind=8), intent(in)    :: src(:, :), dx, dy
               real(kind=8), intent(out)   :: change
            end subroutine solver
        end interface

        integer :: k = 0, kmax, rate, start_time, stop_time
        real(kind=8) :: tolerance, change

        kmax = args%maxiter
        tolerance = args%eps
        change = 2.0*tolerance

        if ( debug  ) then
            write (*, '(A)') "Step   Tolerance     Time, s"
        endif

        call system_clock(count_rate=rate)
        call system_clock(start_time)
        ! iterate the solution until it converges below the level of the tolerance
        do while ((change > tolerance) .and. (k < kmax))

            call solver(u, src, args%dx(1), args%dx(2), change)
            if (debug .and. mod(k, 100) == 0) then
                call system_clock(stop_time)
                write (*, '("#", I5.5, " ", E12.6, " ", F13.3)') k, change, (stop_time - start_time)/real(rate)
            endif
            k = k + 1

        end do
        call system_clock(stop_time)

        nsteps = k
        if (nsteps == kmax) write (*, *) 'warning: the maximum number of iterations reached'
        write (*, '(A, I5)') 'nsteps = ', nsteps
        write (*, '(A, F13.3, A)') 'time elapsed = ', (stop_time - start_time)/real(rate), ' s'

    end subroutine laplace_setup_solver

    ! 5-point SOR method
    subroutine SOR5(u, src, dx, dy, change)


        real(kind=8), intent(inout) :: u(:, :)
        real(kind=8), intent(in)    :: src(:, :), dx, dy
        real(kind=8), intent(out)   :: change

        integer :: i, j
        real(kind=8) :: diff, omega
        real(kind=8) :: alpha, beta

        alpha = dy**2/(2d0 * (dx**2 + dy**2))
        beta  = dx**2/(2d0 * (dx**2 + dy**2))

        omega = 2d0 - 2d0*pi*sqrt(dx*dy)

        change = 0.0
        do j = lbound(u, 2) + 1, ubound(u, 2) - 1
            do i = lbound(u, 1) + 1, ubound(u, 1) - 1
                diff = omega * ( alpha * ( u(i+1, j) + u(i-1, j) ) + beta * ( u(i, j+1) + u(i, j-1) ) - &
                                 u(i, j) - 0.25d0 * src(i, j) )

                u(i, j) = u(i, j) + diff
                change = max(change, abs(diff))

            end do
        end do

    end subroutine SOR5

    ! 5-point SOR method with red-black ordering
    ! and OpenMP parallelization
    subroutine SOR5_red_black(u, src, dx, dy, change)

        real(kind=8), intent(inout) :: u(:, :)
        real(kind=8), intent(in)    :: src(:, :), dx, dy
        real(kind=8), intent(out)   :: change

        integer :: i, j
        real(kind=8) :: diff, omega, change_red, change_black
        real(kind=8) :: alpha, beta

        alpha = dy**2/(2d0 * (dx**2 + dy**2))
        beta  = dx**2/(2d0 * (dx**2 + dy**2))

        omega = 2d0 - 2d0*pi*sqrt(dx*dy)

        change_red = 0.0
        change_black = 0.0

        ! red cells
        !$OMP PARALLEL
        !$OMP DO PRIVATE(diff) REDUCTION(MAX:change_red)
        do j = lbound(u, 2) + 1, ubound(u, 2) - 1
            do i = lbound(u, 1) + 1 + mod(j, 2), ubound(u, 1) - 1, 2
                diff = omega * ( alpha * ( u(i+1, j) + u(i-1, j) ) + beta * ( u(i, j+1) + u(i, j-1) ) - &
                                 u(i, j) - 0.25d0 * src(i, j) )

                u(i, j) = u(i, j) + diff
                change_red = max(change_red, abs(diff))
            end do
        end do
        !$OMP END DO

        ! black cells
        !$OMP DO PRIVATE(diff) REDUCTION(MAX:change_black)
        do j = lbound(u, 2) + 1, ubound(u, 2) - 1
            do i = lbound(u, 1) + 2 - mod(j, 2), ubound(u, 1) - 1, 2
                diff = omega * ( alpha * ( u(i+1, j) + u(i-1, j) ) + beta  * ( u(i, j+1) + u(i, j-1) ) - &
                                 u(i, j) - 0.25d0 * src(i, j) )

                u(i, j) = u(i, j) + diff
                change_black = max(change_black, abs(diff))
            end do
        end do
        !$OMP END DO
        !$OMP END PARALLEL

        change = max(change_red, change_black)

    end subroutine SOR5_red_black

    ! 9-point SOR method
    subroutine SOR9(u, src, dx, dy, change)


        real(kind=8), intent(inout) :: u(:, :)
        real(kind=8), intent(in)    :: src(:, :), dx, dy
        real(kind=8), intent(out)   :: change

        integer :: i, j
        real(kind=8) :: diff, omega
        real(kind=8) :: alpha, beta

        alpha = (5d0*dy**2 - dx**2)/(10d0 * (dx**2 + dy**2))
        beta  = (5d0*dx**2 - dy**2)/(10d0 * (dx**2 + dy**2))

        omega = 1.8d0 !2d0 - sqrt(936d0 / 209d0)*pi*sqrt(dx*dy) TODO

        change = 0.0
        do j = lbound(u, 2) + 1, ubound(u, 2) - 1
            do i = lbound(u, 1) + 1, ubound(u, 1) - 1
                diff = omega * ( alpha * ( u(i+1, j) + u(i-1, j) ) + beta * ( u(i, j+1) + u(i, j-1) ) + &
                                 ( u(i+1, j+1) + u(i-1, j+1) + u(i-1, j+1) + u(i-1, j-1) - src(i, j) )/20d0 - u(i, j) )
                u(i, j) = u(i, j) + diff

                change = max(change, abs(diff))

            end do
        end do

    end subroutine SOR9


end module laplace
