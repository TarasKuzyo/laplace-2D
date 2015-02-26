module utils

    use globals, only: debug
    implicit none
    
    type confargs
        integer       :: dims(3), maxiter
        real(kind=8)  :: start_pos(3), end_pos(3), eps
        character(16) :: solver
    end type confargs

contains

    subroutine read_config(filename, args)
    
        character(len=*), intent(in) :: filename
        type(confargs),  intent(out) :: args
        
        integer :: n_dim
        character(len=16) :: skip_buffer
        
        integer, parameter :: read_uid = 21
        integer :: i, j, openstatus
        
        open(unit=read_uid, file=filename, action="read", iostat=openstatus)
        if (openstatus > 0) stop "Cannot open config file to read."
        
        read(read_uid, *) skip_buffer, n_dim
        read(read_uid, *) skip_buffer, args%eps
        read(read_uid, *) skip_buffer, args%maxiter
        read(read_uid, *) skip_buffer, args%solver
        
        do i = 1, 3
            read(read_uid, *) skip_buffer, args%start_pos(i), args%end_pos(i), args%dims(i)
        end do
        
        if (n_dim < 1 .or. n_dim > 3) n_dim = 1
        do i = 1, 3
            if (i > n_dim) args%dims(i) = 1
        end do
        
        if (debug) then
            write(*, *) args
        end if
        
        close(read_uid)
        
    end subroutine read_config                       
        
end module utils
