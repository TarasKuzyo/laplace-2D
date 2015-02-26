module utils

    use globals, only: debug
    implicit none
    
    type confargs
        integer       :: ndim, maxiter, d(3)
        real(kind=8)  :: start_pos(3), end_pos(3), dx(3), eps
        character(16) :: solver
    end type confargs


contains


    subroutine read_config(filename, args)
    
        character(len=*), intent(in) :: filename
        type(confargs),  intent(out) :: args
        
        character(len=16) :: skip_buffer
        
        integer, parameter :: read_uid = 21
        integer :: i, openstatus
        
        open(unit=read_uid, file=filename, action="read", iostat=openstatus)
        if (openstatus > 0) stop "Cannot open config file to read."
        
        read(read_uid, *) skip_buffer, args%ndim
        read(read_uid, *) skip_buffer, args%eps
        read(read_uid, *) skip_buffer, args%maxiter
        read(read_uid, *) skip_buffer, args%solver
        
        do i = 1, 3
            read(read_uid, *) skip_buffer, args%start_pos(i), args%end_pos(i), args%d(i)
            if (args%d(i) < 1) args%d(i) = 1         ! force grid size to be positive
        end do
        
        if (args%ndim < 1 .or. args%ndim > 3) args%ndim = 1
        do i = 1, 3
            if (i > args%ndim) args%d(i) = 1
        end do
        
        args%dx = (args%end_pos - args%start_pos)/(args%d - 1)
        
        if (debug) then
            write(*, *) '----------------- problem setup ----------------------------------------------'
            write(*, *)
            write(*, *) 'dimensions : ', args%ndim
            write(*, *) 'tolerance  : ', args%eps
            write(*, *) 'maxiter    : ', args%maxiter
            write(*, *) 'solver     : ', args%solver
            write(*, *) 'x-dir      : ', args%start_pos(1), args%end_pos(1), args%d(1)
            write(*, *) 'y-dir      : ', args%start_pos(2), args%end_pos(2), args%d(2)
            write(*, *) 'z-dir      : ', args%start_pos(3), args%end_pos(3), args%d(3)
            write(*, *)
            write(*, *) '------------------------------------------------------------------------------'
        end if
        
        close(read_uid)
        
    end subroutine read_config   
    

    subroutine write_binary(filename, data_array)
    
        character(len=*), intent(in) :: filename
        real(kind=8), intent(in)     :: data_array(:, :, :)
        
        integer, parameter :: write_uid = 22
        integer :: openstatus
        
        open(unit=write_uid, file=filename, status="replace", &
             action="write", position="rewind", access="stream", &
             form="unformatted", iostat=openstatus)
        if (openstatus > 0) stop "Cannot open a file to write."
        
        write(write_uid) data_array
        close(write_uid)
    
    end subroutine write_binary  
                      
        
end module utils



