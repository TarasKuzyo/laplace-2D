module utils

    use globals, only: nd, debug
    implicit none
    
    type confargs
        integer       :: maxiter, d(nd)
        real(kind=8)  :: start_pos(nd), end_pos(nd), dx(nd), eps
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
        
        read(read_uid, *) skip_buffer, args%eps
        read(read_uid, *) skip_buffer, args%maxiter
        read(read_uid, *) skip_buffer, args%solver
        
        do i = 1, nd
            read(read_uid, *) skip_buffer, args%start_pos(i), args%end_pos(i), args%d(i)
            if (args%d(i) < 1) args%d(i) = 1         ! force grid size to be positive
        end do
        
        
        args%dx = (args%end_pos - args%start_pos)/(args%d - 1)
        
        if (debug) then
            write(*, '(A)')               '----------------- problem setup -------------------------------'
            write(*, *)
            write(*, '(A, X, E13.7)')     'tolerance  : ', args%eps
            write(*, '(A, X, I6)')        'maxiter    : ', args%maxiter
            write(*, '(A, X, A)')         'solver     : ', args%solver
            write(*, '(A, X, 2F9.5, I5)') 'x-dir      : ', args%start_pos(1), args%end_pos(1), args%d(1)
            write(*, '(A, X, 2F9.5, I5)') 'y-dir      : ', args%start_pos(2), args%end_pos(2), args%d(2)
            write(*, *)
            write(*, '(A)')               '----------------------------------------------------------------'
        end if
        
        close(read_uid)
        
    end subroutine read_config   
    

    subroutine write_binary(filename, data_array)
    
        character(len=*), intent(in) :: filename
        real(kind=8), intent(in)     :: data_array(:, :)
        
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



