module laplace 

    use globals, only: debug
    use utils, only: confargs
    implicit none

contains


    subroutine laplace_solve(u, src, args, nsteps)
    
        real(kind=8), intent(inout) :: u(:, :, :)
        real(kind=8),   intent(in) :: src(:, :, :)
        type(confargs), intent(in) :: args
        integer, intent(out) :: nsteps
    
        if (args%solver == 'SOR5') then
            call laplace_set_solver(u, src,  SOR5, args, nsteps)
        else
            stop "invalid solver set."
        end if
        
    end subroutine laplace_solve
    

    subroutine laplace_set_solver(u, src,  solver, args, nsteps)
    
        real(kind=8), intent(inout) :: u(:, :, :)
        real(kind=8),   intent(in)  :: src(:, :, :)
        external                    :: solver
        type(confargs), intent(in)  :: args
        integer, intent(out)        :: nsteps
        
        
        integer :: k = 0, kmax
        real(kind=8) :: tolerance, change   
        
        kmax = args%maxiter
        tolerance = args%eps
        change = 2.0*tolerance 
        
        ! iterate the solution until it converges below the level of the tolerance
        do while ((change > tolerance) .and. (k < kmax))
            call solver(u, src, change)
            k = k + 1
        end do
        
        nsteps = k
        if (nsteps == kmax) write (*, *) 'warning: the maximum number of iterations reached'
    
    end subroutine laplace_set_solver


    subroutine SOR5(u, src, change)
    
        real(kind=8), intent(inout) :: u(:, :, :)
        real(kind=8), intent(in)    :: src(:, :, :)
        real(kind=8), intent(out)   :: change

        integer :: i, j, n_x, n_y
        real(kind=8) :: diff, omega
        real(kind=8) :: dx, dy, alpha, beta
        
        n_x = size(u, dim=1) 
        n_y = size(u, dim=2)
        
        change =  1.0
        
                   
    end subroutine SOR5


end module laplace
