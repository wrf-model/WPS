program rdnml_util
  implicit none

! Declare the namelist variables:

  integer :: start_year = 0
  integer :: start_month = 0
  integer :: start_day = 0
  integer :: start_hour = 0
  integer :: start_minute = 0
  integer :: start_second = 0

  integer :: end_year = 0
  integer :: end_month = 0
  integer :: end_day = 0
  integer :: end_hour = 0
  integer :: end_minute = 0
  integer :: end_second = 0

  integer :: interval = 0

  integer :: debug_level = 0

  namelist /record1/ start_year, start_month, start_day, start_hour, &
       start_minute, start_second, &
       end_year, end_month, end_day, end_hour, &
       end_minute, end_second,&
       interval, &
       debug_level

  read(*, NML=record1) 
  write(*,'(2(I4,3(1x,I2.2),1x))') start_year, start_month, start_day, &
       start_hour, end_year, end_month, end_day, end_hour
  
end program rdnml_util
