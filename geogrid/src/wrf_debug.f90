subroutine wrf_debug(i, msg)

   implicit none

   integer :: i
   character (len=*) msg

   write(6,*) 'WRF_DEBUG:'//msg

end subroutine wrf_debug


subroutine wrf_message(msg)

   implicit none

   character (len=*) msg

   write(6,*) 'WRF_MESSAGE:'//msg

end subroutine wrf_message


subroutine wrf_error_fatal(msg)

   implicit none

   character (len=*) msg

   write(6,*) 'WRF_ERROR_FATAL:'//msg

   stop

end subroutine wrf_error_fatal


subroutine wrf_error_fatal3(msg)

   implicit none

   character (len=*) msg

   write(6,*) 'WRF_ERROR_FATAL:'//msg

   stop

end subroutine wrf_error_fatal3
