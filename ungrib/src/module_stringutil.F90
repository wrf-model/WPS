module stringutil

!BUG: STRSIZE should be as large as the longest string length used in WPS
   integer, parameter :: STRSIZE = 1024

   contains

   function get_path(s)

      implicit none

      ! Arguments
      character (len=*) :: s

      ! Return value
      character (len=STRSIZE) :: get_path

      ! Local variables
      integer :: n, i

      n = len(s)

      if (n > STRSIZE) then
         write(6,*) 'ERROR: Maximum string length exceeded in get_path()'
         stop
      end if

      write(get_path,'(a)') './'
  
      do i=n,1,-1
         if (s(i:i) == '/') then
            write(get_path,'(a)') s(1:i)
            exit
         end if
      end do

   end function get_path

end module stringutil
