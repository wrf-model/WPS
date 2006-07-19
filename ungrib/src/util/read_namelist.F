subroutine read_namelist(hstart, hend, delta_time, ntimes,&
     ordered_by_date, print_flag, iprint, out_format)
  implicit none
  integer , parameter :: maxim_doms = 21
  character(len=200) :: extdataroot, file_name_namelist
  character(len=19) :: hstart, hend
  integer :: delta_time
  integer :: ntimes
  logical :: ordered_by_date
  logical :: print_flag
  integer :: iprint

  integer :: ierr
  integer :: idts
  logical :: stop_flag = .FALSE.

! Declare the namelist variables:

  integer , dimension(maxim_doms) :: start_year
  integer , dimension(maxim_doms) :: start_month
  integer , dimension(maxim_doms) :: start_day
  integer , dimension(maxim_doms) :: start_hour
  integer , dimension(maxim_doms) :: start_minute
  integer , dimension(maxim_doms) :: start_second

  integer , dimension(maxim_doms) :: end_year
  integer , dimension(maxim_doms) :: end_month
  integer , dimension(maxim_doms) :: end_day
  integer , dimension(maxim_doms) :: end_hour
  integer , dimension(maxim_doms) :: end_minute
  integer , dimension(maxim_doms) :: end_second

  character (len=128) , dimension(maxim_doms) :: start_date, end_date
  character (len=128) :: opt_output_from_geogrid_path
  integer :: interval_seconds = 0
  character (len=3) :: wrf_core = 'ARW'
  integer :: max_dom, io_form_geogrid

  character(len=3) :: out_format
  logical :: debug_print = .FALSE.

  namelist /share/ wrf_core, max_dom, &
       start_year, start_month, start_day, start_hour, &
       start_minute, start_second, &
       end_year, end_month, end_day, end_hour, &
       end_minute, end_second,&
       interval_seconds, &
       start_date, end_date, &
       io_form_geogrid, opt_output_from_geogrid_path, debug_print

  namelist /ungrib/ out_format, &
       ordered_by_date

  start_year = 0
  start_month = 0
  start_day = 0
  start_hour = 0
  start_minute = 0
  start_second = 0

  end_year = 0
  end_month = 0
  end_day = 0
  end_hour = 0
  end_minute = 0
  end_second = 0

! Start routine:

! Build the namelist file name:

  CALL GETENV('EXT_DATAROOT',extdataroot)
  file_name_namelist =  'namelist.wps'
 
! Open the namelist file:
  open(10, file=file_name_namelist, status='old', iostat=ierr)
  if (ierr.ne.0) then
     write(*,'(80(''-''), /, ''**** ERROR:'')')
     write(*,'(''**** Error opening namelist file namelist.wps'')')
     write(*,'(/,80("-"))')
     stop
  endif

   REWIND (10) 

  ! set default:
  ordered_by_date = .TRUE.
  start_date(1)(1:4) = '0000'
  end_date(1)(1:4)   = '0000'

  read(10,NML=share)

! Build the Starting date HSTART and the ending date HEND from the namelist
! date/time information. start_date takes priority over the multi-variable method.

  if ( start_date(1)(1:4) .eq. '0000' ) then
    call build_hdate(hstart, start_year(1), start_month(1), start_day(1), start_hour(1), &
       start_minute(1), start_second(1))
  else
    hstart = start_date(1)(1:19)
  endif
  if ( end_date(1)(1:4) .eq. '0000' ) then
    call build_hdate(hend, end_year(1), end_month(1), end_day(1), end_hour(1), &
       end_minute(1), end_second(1))
  else
    hend = end_date(1)(1:19)
  endif

! Compute the time difference between start date and end date:

  call geth_idts(hend, hstart, idts)

! Check that INTERVAL is greater than zero:

  if (interval_seconds <= 0) then
     write(*,'(//,80("*"),/)')
     write(*,'(2x,"ERROR STOP",/)')
     write(*,'(6x,"INTERVAL must be greater than zero:")')
     write(*,'(6x,"Start time: ", A19)') hstart
     write(*,'(6x,"End time  : ", A19)') hend
     write(*,'(6x,"INTERVAL  : ", I10, " seconds")')interval_seconds
     write(*,'(/,6x,"Change your namelist, and resubmit")')
     write(*,'(/,80("*"),//)')
     stop_flag = .TRUE.
  endif

! Check that the selected INTERVAL evenly fills difference between 
! start time and end time:

  if ((idts/interval_seconds)*interval_seconds /= idts) then
     write(*,'(//,80("*"),/)')
     write(*,'(2x,"ERROR STOP",/)')
     write(*,'(6x,"INTERVAL must fit evenly between start time and end time:")')
     write(*,'(6x,"Start time: ", A19)') hstart
     write(*,'(6x,"End time  : ", A19)') hend
     write(*,'(6x,"INTERVAL  : ", I10, " seconds = ", F15.8, " hours")')&
          interval_seconds, float(interval_seconds)/3600.
     write(*,'(/,6x,"Change your namelist, and resubmit")')
     write(*,'(/,80("*"),//)')
     stop_flag = .TRUE.
  endif

! Check that start time is not later than end time:

  if (hstart > hend) then
     write(*,'(//,80("*"),/)')
     write(*,'(2x,"ERROR STOP",/)')
     write(*,'(6x,"Start time must not be later than end time:")')
     write(*,'(6x,"Start time: ", A19)') hstart
     write(*,'(6x,"End time  : ", A19)') hend
     write(*,'(6x,"INTERVAL  : ", I10, " seconds")') interval_seconds
     write(*,'(/,6x,"Change your namelist, and resubmit")')
     write(*,'(/,80("*"),//)')
     stop_flag = .TRUE.
  endif

! Compute the number of time periods to process:

  if (stop_flag) stop

  ntimes = idts/interval_seconds + 1

  print*, 'HSTART, HEND = ', hstart//'    '//hend

  if (debug_print) then
     iprint=1
     write(*,'(//)')
     write(*,'("Namelist information (coarse domain): ")')
     write(*,'(5x,"start_year = ", I8)') start_year(1)
     write(*,'(5x,"start_month = ", I8)') start_month(1)
     write(*,'(5x,"start_day = ", I8)') start_day(1)
     write(*,'(5x,"start_hour = ", I8)') start_hour(1)
     write(*,'(5x,"start_minute = ", I8)') start_minute(1)
     write(*,'(5x,"start_second = ", I8)') start_second(1)
     write(*,'(5x,"end_year = ", I8)') end_year(1)
     write(*,'(5x,"end_month = ", I8)') end_month(1)
     write(*,'(5x,"end_day = ", I8)') end_day(1)
     write(*,'(5x,"end_hour = ", I8)') end_hour(1)
     write(*,'(5x,"end_minute = ", I8)') end_minute(1)
     write(*,'(5x,"end_second = ", I8)') end_second(1)
     write(*,'(5x,"interval_seconds = ", I10)') interval_seconds
     write(*,'(5x,"ordered_by_date = ", L4)') ordered_by_date
     write(*,'(5x,"debug_print = ", L4)') debug_print
     write(*,'(//)')
  else
     iprint=0
  endif

  print_flag = debug_print
  delta_time = interval_seconds

  rewind(10)
  out_format = 'WPS'
  read(10,NML=ungrib,END=100)

100  continue
   if (out_format(1:2) .eq. 'WP' .or. out_format(1:2) .eq. 'wp') then
     out_format = 'WPS'
     write(6,*) 'output format is WPS'
   else if (out_format(1:2) .eq. 'SI' .or. out_format(1:2) .eq. 'si') then
     out_format = 'SI '
     write(6,*) 'output format is SI'
   else if (out_format(1:2) .eq. 'MM' .or. out_format(1:2) .eq. 'mm') then
     out_format = 'MM5'
     write(6,*) 'output format is MM5 pregrid'
   else 
     write(6,*) 'I do not recognize the output format, ',out_format,' , stopping.'
     stop 'read_namelist'
   endif

! Close the namelist file:

  close(10)
  
end subroutine read_namelist
