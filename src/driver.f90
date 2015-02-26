program driver

    use utils, only: confargs, read_config, write_binary
    use init,  only: initialize
    implicit none
    
    type(confargs) :: args
    integer :: alstat
    real(kind=8), allocatable :: u(:, :, :)
    
    write(*, *) 'Hello, world!'
    
    call read_config("laplace.ini", args)
    
    allocate( u(args%d(1), args%d(2), args%d(3)), stat=alstat )
    if (alstat /= 0) stop "Unable to allocate memory for the data array."
    
    call initialize(args, u)
    
    
    call write_binary('output.dbl', u)
        
    deallocate(u)

end program driver



