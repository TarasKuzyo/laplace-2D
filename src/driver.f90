program driver

    use utils
    implicit none
    
    type(confargs) :: args
    
    write(*, *) 'Hello, world!'
    
    call read_config("laplace.ini", args)


end program driver
