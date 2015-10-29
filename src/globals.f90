module globals

! 
!  created by Taras Kuzyo 
!  as part of laplace-2D numerical code
! 
!  Defines some global constants and variables
!  for use in other modules
! 

    implicit none
    ! number of spatial dimensions
    integer, parameter :: nd = 2
    
    real(kind=8), parameter :: pi = 4.0d0*atan(1.0d0)
    ! if .true. prints dbug messages during execution    
    logical :: debug = .true.       

end module globals
