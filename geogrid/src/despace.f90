!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Name: despace
!
! Purpose: Remove all space and tab characters from a string, thus compressing
!   the string to the left.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine despace(string)

   implicit none
 
   ! Arguments
   character (len=*), intent(inout) :: string
 
   ! Local variables
   integer :: i, j, length, iquoted
 
   length = len(string)
 
   iquoted = 0
   j = 1
   do i=1,length
      ! Check for a quote mark
      if (string(i:i) == '"' .or. string(i:i) == '''') iquoted = mod(iquoted+1,2)
  
      ! Check for non-space, non-tab character, or if we are inside quoted text
      if ((string(i:i) /= ' ' .and. string(i:i) /= achar(9)) .or. iquoted == 1) then
         string(j:j) = string(i:i)
         j = j + 1
      end if
   end do
 
   do i=j,length
      string(i:i) = ' '
   end do

end subroutine despace
