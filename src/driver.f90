program driver
!
!  created by Taras Kuzyo 
!  as part of laplace-2D numerical code
!
!  Defines the main program.
!  -- describe command line parameter
!  --
!

    use utils,   only: confargs, read_config, write_binary
    use init,    only: initialize
    use laplace, only: laplace_solve
    implicit none
    
    type(confargs) :: args
    integer :: alstat, nsteps
    character(len=64) :: config_filename, output_filename
    real(kind=8), allocatable :: u(:, :), source(:, :)
    
    config_filename = 'laplace.ini'
    output_filename = 'output.dbl'
    
    if (command_argument_count() == 1) then
        call get_command_argument(1, output_filename)
        write(*, *) output_filename
    end if
    
    
    call read_config(config_filename, args)
    
    
    allocate( u(args%d(1), args%d(2)), stat=alstat )
    if (alstat /= 0) stop "Unable to allocate memory for the data array."
    
    allocate( source(args%d(1), args%d(2)), stat=alstat )
    if (alstat /= 0) stop "Unable to allocate memory for the source term."
    
    
    call initialize(u, source, args)
    call laplace_solve(u, source, args, nsteps)
    
    call write_binary(output_filename, u)
        
    deallocate(u, source)


end program driver



