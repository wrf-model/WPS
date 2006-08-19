module module_debug

   use parallel_module

   integer, parameter :: QUIET=-100, LOGFILE=-2, DEBUG=0, INFORM=1, WARN=2, ERROR=3, STDOUT=100

   integer :: the_debug_level = DEBUG
   integer :: log_unit = 55
   logical :: log_is_opened = .false.

   contains

   subroutine set_debug_level(ilev)

      implicit none
     
      ! Arguments
      integer, intent(in) :: ilev

      the_debug_level = ilev

   end subroutine set_debug_level


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
      integer :: write_unit
      real :: fa
      character (len=8) :: cur_date
      character (len=10) :: cur_time
      character (len=10) :: print_date
      character (len=12) :: print_time
      character (len=128) :: sa

      if (my_proc_id /= IO_NODE) return

      idxi = 1
      idxf = 1
      idxs = 1
      istart = 1
      iend = len_trim(fmtstring)

      if (assertion) then

         if (.not. log_is_opened) then
#ifdef _GEOGRID
            open(unit=log_unit,file='geogrid.log',form='formatted',status='replace')
#endif
#ifdef _METGRID
            open(unit=log_unit,file='metgrid.log',form='formatted',status='replace')
#endif
            log_is_opened = .true.
         end if

         if (level /= STDOUT) then 
            call date_and_time(date=cur_date,time=cur_time)
            write(print_date,'(a10)') cur_date(1:4)//'-'//cur_date(5:6)//'-'//cur_date(7:8)
            write(print_time,'(a12)') cur_time(1:2)//':'//cur_time(3:4)//':'//cur_time(5:10)
            write(unit=log_unit,fmt='(a)',advance='no') print_date//' '//print_time//' --- '
         end if

         if (level == LOGFILE) then
            write_unit = QUIET
         else
            write_unit = 6
         end if

         if (level == DEBUG) then
            if (level >= the_debug_level) &
               write(unit=write_unit,fmt='(a)',advance='no') 'DEBUG: '
            write(unit=log_unit,fmt='(a)',advance='no') 'DEBUG: '
         else if (level == INFORM) then
            if (level >= the_debug_level) &
               write(unit=write_unit,fmt='(a)',advance='no') 'INFORM: '
            write(unit=log_unit,fmt='(a)',advance='no') 'INFORM: '
         else if (level == WARN) then
            if (level >= the_debug_level) &
               write(unit=write_unit,fmt='(a)',advance='no') 'WARNING: '
            write(unit=log_unit,fmt='(a)',advance='no') 'WARNING: '
         else if (level == ERROR) then
            if (level >= the_debug_level) &
               write(unit=write_unit,fmt='(a)',advance='no') 'ERROR: '
            write(unit=log_unit,fmt='(a)',advance='no') 'ERROR: '
         end if
      
         i = index(fmtstring(istart:iend),'%')
         do while (i > 0 .and. i < iend)
            i = i + istart - 1
            if (level >= the_debug_level) &
               write(unit=write_unit,fmt='(a)',advance='no') fmtstring(istart:i-1)
            if (level /= STDOUT) &
               write(unit=log_unit,fmt='(a)',advance='no') fmtstring(istart:i-1)
   
            if (fmtstring(i+1:i+1) == '%') then
               if (level >= the_debug_level) &
                  write(unit=write_unit,fmt='(a1)',advance='no') '%'
               if (level /= STDOUT) &
                  write(unit=log_unit,fmt='(a1)',advance='no') '%'
                            
            else if (fmtstring(i+1:i+1) == 'i') then
               if (idxi == 1 .and. present(i1)) then
                  ia = i1
               else if (idxi == 2 .and. present(i2)) then
                  ia = i2
               else if (idxi == 3 .and. present(i3)) then
                  ia = i3
               end if
   
               if (ia <= -10000000) then
                  if (level >= the_debug_level) &
                     write(unit=write_unit,fmt='(i9)',advance='no') ia
                  if (level /= STDOUT) &
                     write(unit=log_unit,fmt='(i9)',advance='no') ia
               else if (ia <= -1000000) then
                  if (level >= the_debug_level) &
                     write(unit=write_unit,fmt='(i8)',advance='no') ia
                  if (level /= STDOUT) &
                     write(unit=log_unit,fmt='(i8)',advance='no') ia
               else if (ia <= -100000) then
                  if (level >= the_debug_level) &
                     write(unit=write_unit,fmt='(i7)',advance='no') ia
                  if (level /= STDOUT) &
                     write(unit=log_unit,fmt='(i7)',advance='no') ia
               else if (ia <= -10000) then
                  if (level >= the_debug_level) &
                     write(unit=write_unit,fmt='(i6)',advance='no') ia
                  if (level /= STDOUT) &
                     write(unit=log_unit,fmt='(i6)',advance='no') ia
               else if (ia <= -1000) then
                  if (level >= the_debug_level) &
                     write(unit=write_unit,fmt='(i5)',advance='no') ia
                  if (level /= STDOUT) &
                     write(unit=log_unit,fmt='(i5)',advance='no') ia
               else if (ia <= -100) then
                  if (level >= the_debug_level) &
                     write(unit=write_unit,fmt='(i4)',advance='no') ia
                  if (level /= STDOUT) &
                     write(unit=log_unit,fmt='(i4)',advance='no') ia
               else if (ia <= -10) then
                  if (level >= the_debug_level) &
                     write(unit=write_unit,fmt='(i3)',advance='no') ia
                  if (level /= STDOUT) &
                     write(unit=log_unit,fmt='(i3)',advance='no') ia
               else if (ia < 0) then
                  if (level >= the_debug_level) &
                     write(unit=write_unit,fmt='(i2)',advance='no') ia
                  if (level /= STDOUT) &
                     write(unit=log_unit,fmt='(i2)',advance='no') ia
               else if (ia < 10) then
                  if (level >= the_debug_level) &
                     write(unit=write_unit,fmt='(i1)',advance='no') ia
                  if (level /= STDOUT) &
                     write(unit=log_unit,fmt='(i1)',advance='no') ia
               else if (ia < 100) then
                  if (level >= the_debug_level) &
                     write(unit=write_unit,fmt='(i2)',advance='no') ia
                  if (level /= STDOUT) &
                     write(unit=log_unit,fmt='(i2)',advance='no') ia
               else if (ia < 1000) then
                  if (level >= the_debug_level) &
                     write(unit=write_unit,fmt='(i3)',advance='no') ia
                  if (level /= STDOUT) &
                     write(unit=log_unit,fmt='(i3)',advance='no') ia
               else if (ia < 10000) then
                  if (level >= the_debug_level) &
                     write(unit=write_unit,fmt='(i4)',advance='no') ia
                  if (level /= STDOUT) &
                     write(unit=log_unit,fmt='(i4)',advance='no') ia
               else if (ia < 100000) then
                  if (level >= the_debug_level) &
                     write(unit=write_unit,fmt='(i5)',advance='no') ia
                  if (level /= STDOUT) &
                     write(unit=log_unit,fmt='(i5)',advance='no') ia
               else if (ia < 1000000) then
                  if (level >= the_debug_level) &
                     write(unit=write_unit,fmt='(i6)',advance='no') ia
                  if (level /= STDOUT) &
                     write(unit=log_unit,fmt='(i6)',advance='no') ia
               else if (ia < 10000000) then
                  if (level >= the_debug_level) &
                     write(unit=write_unit,fmt='(i7)',advance='no') ia
                  if (level /= STDOUT) &
                     write(unit=log_unit,fmt='(i7)',advance='no') ia
               else if (ia < 100000000) then
                  if (level >= the_debug_level) &
                     write(unit=write_unit,fmt='(i8)',advance='no') ia
                  if (level /= STDOUT) &
                     write(unit=log_unit,fmt='(i8)',advance='no') ia
               else
                  if (level >= the_debug_level) &
                     write(unit=write_unit,fmt='(i9)',advance='no') ia
                  if (level /= STDOUT) &
                     write(unit=log_unit,fmt='(i9)',advance='no') ia
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
   
               if (fa <= -100000000.) then
                  if (level >= the_debug_level) &
                     write(unit=write_unit,fmt='(f15.4)',advance='no') fa
                  if (level /= STDOUT) &
                     write(unit=log_unit,fmt='(f15.4)',advance='no') fa
               else if (fa <= -10000000.) then
                  if (level >= the_debug_level) &
                     write(unit=write_unit,fmt='(f14.4)',advance='no') fa
                  if (level /= STDOUT) &
                     write(unit=log_unit,fmt='(f14.4)',advance='no') fa
               else if (fa <= -1000000.) then
                  if (level >= the_debug_level) &
                     write(unit=write_unit,fmt='(f13.4)',advance='no') fa
                  if (level /= STDOUT) &
                     write(unit=log_unit,fmt='(f13.4)',advance='no') fa
               else if (fa <= -100000.) then
                  if (level >= the_debug_level) &
                     write(unit=write_unit,fmt='(f12.4)',advance='no') fa
                  if (level /= STDOUT) &
                     write(unit=log_unit,fmt='(f12.4)',advance='no') fa
               else if (fa <= -10000.) then
                  if (level >= the_debug_level) &
                     write(unit=write_unit,fmt='(f11.4)',advance='no') fa
                  if (level /= STDOUT) &
                     write(unit=log_unit,fmt='(f11.4)',advance='no') fa
               else if (fa <= -1000.) then
                  if (level >= the_debug_level) &
                     write(unit=write_unit,fmt='(f10.4)',advance='no') fa
                  if (level /= STDOUT) &
                     write(unit=log_unit,fmt='(f10.4)',advance='no') fa
               else if (fa <= -100.) then
                  if (level >= the_debug_level) &
                     write(unit=write_unit,fmt='(f9.4)',advance='no') fa
                  if (level /= STDOUT) &
                     write(unit=log_unit,fmt='(f9.4)',advance='no') fa
               else if (fa <= -10.) then
                  if (level >= the_debug_level) &
                     write(unit=write_unit,fmt='(f8.4)',advance='no') fa
                  if (level /= STDOUT) &
                     write(unit=log_unit,fmt='(f8.4)',advance='no') fa
               else if (fa < 0.) then
                  if (level >= the_debug_level) &
                     write(unit=write_unit,fmt='(f7.4)',advance='no') fa
                  if (level /= STDOUT) &
                     write(unit=log_unit,fmt='(f7.4)',advance='no') fa
               else if (fa < 10.) then
                  if (level >= the_debug_level) &
                     write(unit=write_unit,fmt='(f6.4)',advance='no') fa
                  if (level /= STDOUT) &
                     write(unit=log_unit,fmt='(f6.4)',advance='no') fa
               else if (fa < 100.) then
                  if (level >= the_debug_level) &
                     write(unit=write_unit,fmt='(f7.4)',advance='no') fa
                  if (level /= STDOUT) &
                     write(unit=log_unit,fmt='(f7.4)',advance='no') fa
               else if (fa < 1000.) then
                  if (level >= the_debug_level) &
                     write(unit=write_unit,fmt='(f8.4)',advance='no') fa
                  if (level /= STDOUT) &
                     write(unit=log_unit,fmt='(f8.4)',advance='no') fa
               else if (fa < 10000.) then
                  if (level >= the_debug_level) &
                     write(unit=write_unit,fmt='(f9.4)',advance='no') fa
                  if (level /= STDOUT) &
                     write(unit=log_unit,fmt='(f9.4)',advance='no') fa
               else if (fa < 100000.) then
                  if (level >= the_debug_level) &
                     write(unit=write_unit,fmt='(f10.4)',advance='no') fa
                  if (level /= STDOUT) &
                     write(unit=log_unit,fmt='(f10.4)',advance='no') fa
               else if (fa < 1000000.) then
                  if (level >= the_debug_level) &
                     write(unit=write_unit,fmt='(f11.4)',advance='no') fa
                  if (level /= STDOUT) &
                     write(unit=log_unit,fmt='(f11.4)',advance='no') fa
               else if (fa < 10000000.) then
                  if (level >= the_debug_level) &
                     write(unit=write_unit,fmt='(f12.4)',advance='no') fa
                  if (level /= STDOUT) &
                     write(unit=log_unit,fmt='(f12.4)',advance='no') fa
               else if (fa < 100000000.) then
                  if (level >= the_debug_level) &
                     write(unit=write_unit,fmt='(f13.4)',advance='no') fa
                  if (level /= STDOUT) &
                     write(unit=log_unit,fmt='(f13.4)',advance='no') fa
               else if (fa < 1000000000.) then
                  if (level >= the_debug_level) &
                     write(unit=write_unit,fmt='(f14.4)',advance='no') fa
                  if (level /= STDOUT) &
                     write(unit=log_unit,fmt='(f14.4)',advance='no') fa
               else
                  if (level >= the_debug_level) &
                     write(unit=write_unit,fmt='(f15.4)',advance='no') fa
                  if (level /= STDOUT) &
                     write(unit=log_unit,fmt='(f15.4)',advance='no') fa
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
   
               if (level >= the_debug_level) &
                  write(unit=write_unit,fmt='(a)',advance='no') trim(sa)
               if (level /= STDOUT) &
                  write(unit=log_unit,fmt='(a)',advance='no') trim(sa)
               idxs = idxs + 1
   
            end if
   
            istart = i+2
            i = index(fmtstring(istart:iend),'%')
         end do
   
         if (level >= the_debug_level) &
            write(unit=write_unit,fmt='(a)') fmtstring(istart:iend)
         if (level /= STDOUT) &
            write(unit=log_unit,fmt='(a)') fmtstring(istart:iend)

         if (level == ERROR) stop

      end if


   end subroutine mprintf

end module module_debug
