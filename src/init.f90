module init
    
    use utils, only: confargs
    use boundaries
    implicit none
    
contains 

    function bounds(index_arr, start_val, end_val)
    
        real(kind=8), intent(in) :: index_arr(:), start_val, end_val
        real(kind=8) :: bounds(size(index_arr))
        
        bounds = start_val + (index_arr - 1)*end_val/(size(index_arr) - 1)
        
    end function bounds
    

    subroutine initialize(args, u)
        
        type(confargs), intent(in) :: args
        real(kind=8),  intent(out) :: u(:, :, :)
        
        integer :: i, j, k
        real(kind=8), dimension(:) :: xx(size(u, 1)), yy(size(u, 2)), zz(size(u, 3))
        
        xx = (/ (i, i = 1, size(u, 1)) /)
        yy = (/ (i, i = 1, size(u, 2)) /)
        yy = (/ (i, i = 1, size(u, 3)) /)
        
        xx = bounds(xx, args%start_pos(1), args%end_pos(1))
        yy = bounds(yy, args%start_pos(2), args%end_pos(2))
        zz = bounds(zz, args%start_pos(3), args%end_pos(3))
        
        ! set the whole array to zero
        u = 0.0d0
        
        if      (args%ndim == 1) then
            u(lbound(u, 1), 1, 1) = f0()
            u(ubound(u, 1), 1, 1) = f1()
            
        else if (args%ndim == 2) then
            u(:, lbound(u, 2), 1) = fx0(xx)
            u(:, ubound(u, 2), 1) = fx1(xx)
            u(lbound(u, 1), :, 1) = fy0(yy)
            u(ubound(u, 1), :, 1) = fy1(yy)
            
        else if (args%ndim == 3) then
            u(:, :, lbound(u, 3)) = fxy0( spread(xx, 2, size(yy)), transpose( spread(yy, 2, size(xx)) ) )        
            u(:, :, ubound(u, 3)) = fxy1( spread(xx, 2, size(yy)), transpose( spread(yy, 2, size(xx)) ) ) 
            u(:, lbound(u, 2), :) = fxz0( spread(xx, 2, size(zz)), transpose( spread(zz, 2, size(xx)) ) )        
            u(:, ubound(u, 2), :) = fxz1( spread(xx, 2, size(zz)), transpose( spread(zz, 2, size(xx)) ) )    
            u(lbound(u, 1), :, :) = fyz0( spread(yy, 2, size(zz)), transpose( spread(zz, 2, size(yy)) ) )           
            u(ubound(u, 1), :, :) = fyz1( spread(yy, 2, size(zz)), transpose( spread(zz, 2, size(yy)) ) )     
            
        else
            stop "Invalid value for ndim."
        end if
        
        
    end subroutine initialize        

end module init
