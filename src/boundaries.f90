



!---------------------------------------------------------
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
!  fy0(y)  |    fsource(x, y)    |  fy1(y)
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
!---------------------------------------------------------  



module boundaries

    implicit none
    real(kind=8), parameter :: pi = 4.0*atan(1.0)

contains

    !-------------------------------------------------------------------------
    !
    !                              1D boundaries
    !
    !-------------------------------------------------------------------------

    elemental function f0()
    
        real(kind=8) :: f0
        
        f0 = 1d0
        
    end function f0
    
    
    elemental function f1()
    
        real(kind=8) :: f1
        
        f1 = 5d0
        
    end function f1


    !-------------------------------------------------------------------------
    !
    !                              2D boundaries
    !
    !-------------------------------------------------------------------------
    
    
    elemental function fx0(x)
    
        real(kind=8), intent(in) :: x
        real(kind=8) :: fx0
        
        fx0 = 0.0d0
        
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
    !                              3D boundaries
    !
    !-------------------------------------------------------------------------   
    
    
    elemental function fxy0(x, y)
    
        real(kind=8), intent(in) :: x, y
        real(kind=8) :: fxy0
        
        fxy0 = 0
        
    end function fxy0 
    
    
    elemental function fxy1(x, y)
    
        real(kind=8), intent(in) :: x, y
        real(kind=8) :: fxy1
        
        fxy1 = 0
        
    end function fxy1
    
    
    elemental function fyz0(y, z)
    
        real(kind=8), intent(in) :: y, z
        real(kind=8) :: fyz0
        
        fyz0 = 0
        
    end function fyz0
    
    
    elemental function fyz1(y, z)
    
        real(kind=8), intent(in) :: y, z
        real(kind=8) :: fyz1
        
        fyz1 = 0
        
    end function fyz1
    
    
    elemental function fxz0(x, z)
    
        real(kind=8), intent(in) :: x, z
        real(kind=8) :: fxz0
        
        fxz0 = x**2 + z**2
        
    end function fxz0
    
    
    elemental function fxz1(x, z)
    
        real(kind=8), intent(in) :: x, z
        real(kind=8) :: fxz1
        
        fxz1 = - x**2 + z**2
        
    end function fxz1
    
    
    !-------------------------------------------------------------------------
    !
    !                         source term function
    !
    !-------------------------------------------------------------------------   
    
    elemental function fsrc(x, y, z)
    
        real(kind=8), intent(in) :: x, y, z
        real(kind=8) :: fsrc
    
        fsrc = 0.0d0
    
    end function fsrc
    
    
    elemental function fexact(x, y, z)
    
        real(kind=8), intent(in) :: x, y, z
        real(kind=8) :: fexact
    
        fexact = log((x+1)**2 + y**2 + z**2)
    
    end function fexact
    

end module boundaries


