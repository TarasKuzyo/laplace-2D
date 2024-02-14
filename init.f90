module init

! 
!  created by Taras Kuzyo 
!  as part of laplace-2D numerical code
! 
!  Defines a function for the initilization of 
!  solution array and source term array.
! 
    
    use utils, only: confargs
    use boundaries
    implicit none
    
contains 

    ! 
    ! Takes an array of indexes in range 1..n, values of lower and 
    ! upper bounds for the new array. Returns array of the original 
    ! size with its values in range of lower .. upper.
    ! 
    function bounds(index_arr, start_val, end_val)
    
        real(kind=8), intent(in) :: index_arr(:), start_val, end_val
        real(kind=8) :: bounds(size(index_arr))
        
        bounds = start_val + (index_arr - 1) * (end_val - start_val) / (size(index_arr) - 1)
        
    end function bounds
    
    ! 
    !  Takes two 2D arrays for solution and source term and 
    !  a structure with initial setup. Initializes the two 
    !  arrays using function from boundaries module.
    ! 
    subroutine initialize(u, src, args)
        
        real(kind=8),  intent(out) :: u(:, :)
        real(kind=8),  intent(out) :: src(:, :)
        type(confargs), intent(in) :: args
        
        integer :: i, j
        real(kind=8) :: xx( size(u, 1) ), yy( size(u, 2) ), h2
        
        xx = (/ (i, i = 1, size(u, 1)) /)
        yy = (/ (i, i = 1, size(u, 2)) /)
        
        xx = bounds(xx, args%start_pos(1), args%end_pos(1))
        yy = bounds(yy, args%start_pos(2), args%end_pos(2))
        h2 = 2d0*(args%dx(1)**2*args%dx(2)**2)/(args%dx(1)**2 + args%dx(2)**2)
        
        ! set the whole array to zero
        !u = 0.0d0
        
        
        
        ! set the source term array
        do j = 1, size(u, 2)
            do i = 1, size(u, 1)
                u(i, j) = 0.0d0
                src(i, j) = h2 * fsrc(xx(i), yy(j))
            end do
        end do
        
        ! set boundary conditions
        u(:, lbound(u, 2)) = fx0(xx)
        u(:, ubound(u, 2)) = fx1(xx)
        u(lbound(u, 1), :) = fy0(yy)
        u(ubound(u, 1), :) = fy1(yy)
        
    end subroutine initialize        

end module init


