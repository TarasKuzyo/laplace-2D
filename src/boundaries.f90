module boundaries

!
!  created by Taras Kuzyo 
!  as part of laplace-2D numerical code
!
!  Defines 4 functions for setting the boundary
!  conditions and a function for the source term.
!

!---------------------------------------------------------
!---------------------------------------------------------
!
!          Equation:
!
!          $ u_{xx} + u_{yy} = f(x, y) $
!
!          Geometry:
!
!                   fx1(x)
!           _____________________
!          |                     |
!          |                     |
!          |                     |
!          |                     |
!          |                     |
!  fy0(y)  |       f(x, y)       |  fy1(y)
!          |                     |
!          |                     |
!          |                     |
!          |                     |
!          |_____________________|
!
!                   fx0(x)
!
!
!
!---------------------------------------------------------
!---------------------------------------------------------  


    implicit none
    real(kind=8), parameter :: pi = 4.0*atan(1.0)

contains

    !-------------------------------------------------------------------------
    !
    !                              2D boundaries
    !
    !-------------------------------------------------------------------------
    
    elemental function fx0(x)
    
        real(kind=8), intent(in) :: x
        real(kind=8) :: fx0
        
        fx0 = 0.0d0*x
        
    end function fx0
    
    
    elemental function fx1(x)
    
        real(kind=8), intent(in) :: x
        real(kind=8) :: fx1
        
        fx1 = 2d0*x*(1d0 - x)
    
    end function fx1
    
    
    elemental function fy0(y)
    
        real(kind=8), intent(in) :: y
        real(kind=8) :: fy0
        
        fy0 = 3d0*y*(1d0 - y)
    
    end function fy0
    
    
    elemental function fy1(y)
    
        real(kind=8), intent(in) :: y
        real(kind=8) :: fy1
        
        fy1 = 4d0*y*(1d0 - y)
    
    end function fy1
    
    
    !-------------------------------------------------------------------------
    !
    !                         source term function
    !
    !-------------------------------------------------------------------------   
    
    
    elemental function fsrc(x, y)
    
        real(kind=8), intent(in) :: x, y
        real(kind=8) :: fsrc
    
        fsrc = 0.0d0*x*y
    
    end function fsrc
    
    
    elemental function fexact(x, y)
    
        real(kind=8), intent(in) :: x, y
        real(kind=8) :: fexact
    
        fexact = log((x+1)**2 + y**2)
    
    end function fexact
    

end module boundaries


