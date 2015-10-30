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
        
        integer :: k = 0, kmax
        real(kind=8) :: tolerance, change 
        real(kind=8) :: start_time, stop_time
        
        kmax = args%maxiter
        tolerance = args%eps
        change = 2.0*tolerance
         
        if ( debug  ) then
            write (*, '(A)') "Step   Tolerance     Time, s"
        endif
        
        call cpu_time(start_time)
        ! iterate the solution until it converges below the level of the tolerance
        do while ((change > tolerance) .and. (k < kmax))
            call solver(u, src, args%dx(1), args%dx(2), change)
            k = k + 1
            
            if ( debug .and. mod(k, 100) == 0 ) then
                call cpu_time(stop_time)
                write (*, '("#", I5.5, " ", E12.6, " ", F13.3)') k, change, (stop_time - start_time)
            endif
            
        end do
        call cpu_time(stop_time)
        
        nsteps = k
        if (nsteps == kmax) write (*, *) 'warning: the maximum number of iterations reached'
        write (*, '(A, I5)') 'nsteps = ', nsteps
        write (*, '(A, F13.3, A)') 'time elapsed = ', stop_time - start_time, ' s'
    
    end subroutine laplace_setup_solver


    subroutine SOR5(u, src, dx, dy, change)
    
        real(kind=8), intent(inout) :: u(:, :)
        real(kind=8), intent(in)    :: src(:, :), dx, dy
        real(kind=8), intent(out)   :: change

        integer :: i, j
        real(kind=8) :: diff, omega
        real(kind=8) :: alpha, beta
        
        alpha = dy**2/(dx**2 + dy**2)
        beta  = dx**2/(dx**2 + dy**2)
        
        omega = 2d0 - 2d0*pi*sqrt(dx*dy)
        
        change = 0.0
        do j = lbound(u, 2) + 1, ubound(u, 2) - 1
            do i = lbound(u, 1) + 1, ubound(u, 1) - 1
                diff = 0.5*omega * ( alpha * ( u(i+1, j) + u(i-1, j) ) + &
                                     beta  * ( u(i, j+1) + u(i, j-1) ) - &
                                     2d0 * u(i, j) - 0.5 * src(i, j) )

                u(i, j) = u(i, j) + diff
                change  = max(change, abs(diff))
                
            end do
        end do
              
    end subroutine SOR5


end module laplace
