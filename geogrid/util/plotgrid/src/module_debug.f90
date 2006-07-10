module module_debug

   integer, parameter :: STDOUT=-1, DEBUG=0, INFORM=1, WARN=2, ERROR=3

   contains

   subroutine mprintf(assertion, level, fmtstring, i1, i2, i3, f1, f2, f3, s1, s2, s3)

      implicit none

      ! Arguments
      integer, intent(in) :: level
      logical, intent(in) :: assertion
      character (len=*), intent(in) :: fmtstring
      integer, intent(in), optional :: i1, i2, i3
      real, intent(in), optional :: f1, f2, f3
      character (len=*), intent(in), optional :: s1, s2, s3

      ! Local variables 
      integer :: idxi, idxf, idxs, istart, i, iend, ia
      real :: fa
      character (len=128) :: sa

      idxi = 1
      idxf = 1
      idxs = 1
      istart = 1
      iend = len_trim(fmtstring)

      if (assertion) then

         if (level == DEBUG) then
            write(6,'(a)',advance='no') 'DEBUG: '
         else if (level == INFORM) then
            write(6,'(a)',advance='no') 'INFORM: '
         else if (level == WARN) then
            write(6,'(a)',advance='no') 'WARNING: '
         else if (level == ERROR) then
            write(6,'(a)',advance='no') 'ERROR: '
         end if
      
         i = index(fmtstring(istart:iend),'%')
         do while (i > 0 .and. i < iend)
            i = i + istart - 1
            write(6,'(a)',advance='no') fmtstring(istart:i-1)
   
            if (fmtstring(i+1:i+1) == '%') then
               write(6,'(a1)',advance='no') '%'
                            
            else if (fmtstring(i+1:i+1) == 'i') then
               if (idxi == 1 .and. present(i1)) then
                  ia = i1
               else if (idxi == 2 .and. present(i2)) then
                  ia = i2
               else if (idxi == 3 .and. present(i3)) then
                  ia = i3
               end if
   
               if (ia < 10) then
                  write(6,'(i1)',advance='no') ia
               else if (ia < 100) then
                  write(6,'(i2)',advance='no') ia
               else if (ia < 1000) then
                  write(6,'(i3)',advance='no') ia
               else if (ia < 10000) then
                  write(6,'(i4)',advance='no') ia
               else if (ia < 100000) then
                  write(6,'(i5)',advance='no') ia
               else
                  write(6,'(i9)',advance='no') ia
               end if
               idxi = idxi + 1
   
            else if (fmtstring(i+1:i+1) == 'f') then
               if (idxf == 1 .and. present(f1)) then
                  fa = f1
               else if (idxf == 2 .and. present(f2)) then
                  fa = f2
               else if (idxf == 3 .and. present(f3)) then
                  fa = f3
               end if
   
               if (fa < 10.) then
                  write(6,'(f6.4)',advance='no') fa
               else if (fa < 100.) then
                  write(6,'(f7.4)',advance='no') fa
               else if (fa < 1000.) then
                  write(6,'(f8.4)',advance='no') fa
               else if (fa < 10000.) then
                  write(6,'(f9.4)',advance='no') fa
               else if (fa < 100000.) then
                  write(6,'(f10.4)',advance='no') fa
               else
                  write(6,'(f15.4)',advance='no') fa
               end if
               idxf = idxf + 1
   
            else if (fmtstring(i+1:i+1) == 's') then
               if (idxs == 1 .and. present(s1)) then
                  sa = s1
               else if (idxs == 2 .and. present(s2)) then
                  sa = s2
               else if (idxs == 3 .and. present(s3)) then
                  sa = s3
               end if
   
               write(6,'(a)',advance='no') trim(sa)
               idxs = idxs + 1
   
            end if
   
            istart = i+2
            i = index(fmtstring(istart:iend),'%')
         end do
   
         write(6,'(a)') fmtstring(istart:iend)

         if (level == ERROR) stop

      end if


   end subroutine mprintf

end module module_debug
