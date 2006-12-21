subroutine rrpr(hstart, ntimes, interval, nlvl, maxlvl, plvl, debug_level, out_format, prefix)
!                                                                             !
! In case you are wondering, RRPR stands for "Read, ReProcess, and wRite"     !
!                                                                             !
!*****************************************************************************!
!                                                                             !
! Recent changes:                                                             !
!                                                                             !
!    2004-10-29:                                                              !
!               - Sync rrpr.F WRFSI v2.0.1 with MM5 v3.5                      !
!                 Added DATELEN: length of date strings to use for            !
!                 our output file names.                                      !
!                 Added MM5 interpolation from surrounding levels             !
!                 if upper-air U or V are missing                             !
!                 Added test to see if we've got a SEAICE field,              ! 
!                 make sure that it is all Zeros and Ones:                    !
!                                                                             !
!    2002-05-16:                                                              !
!               - Handle the Mercator projection.                             !
!                 This change also required changes to output.F, rd_grib.F,   !
!                 datint.F, gribcode.F                                        !
!                                                                             !
!    2002-02-13:                                                              !
!               - Added vertical interpolation in pressure in case of missing !
!                 U, V, T (the check for RH was already there)                !
!                                                                             !
!    2001-02-14:                                                              !
!               - Allow file names to have date stamps out to minutes or      !
!                 seconds, if the user requests a time interval (in seconds)  !
!                 that is not evenly divisible into hours or minutes.         !
!                 INTERVAL is checked for divisibility into 3600 (for hours)  !
!                 or 60 (for minutes).  The local variable DATELEN is set     !
!                 to be the number of characters to use in our character      !
!                 dates.  Valid values for DATELEN are 13 (for hours),        !
!                 16 (for minutes), and 19 (for seconds).                     !
!                                                                             !
!                 This change also requires changes to pregrid_grib.F,        !
!                 output.F, datint.F, file_delete.F                           !
!                                                                             !
!               - Do processing not just if the requested date matches one we !
!                 want, but if the requested date falls between the startdate !
!                 and the enddate.                                            !
!                                                                             !
!*****************************************************************************!

  use filelist
  use gridinfo
  use storage_module
  use table
  use module_debug
  use stringutil

  implicit none

!------------------------------------------------------------------------------
! Arguments:

! HSTART:  Starting date of times to process 
  character (LEN=19) :: hstart

! NTIMES:  Number of time periods to process
  integer :: ntimes

! INTERVAL:  Time inteval (seconds) of time periods to process.
  integer :: interval

! NLVL:  The number of levels in the stored data.
  integer :: nlvl

! MAXLVL: The parameterized maximum number of levels to allow.
  integer :: maxlvl

! PLVL:  Array of pressure levels (Pa) in the dataset
  real , dimension(maxlvl) :: plvl

! DEBUG_LEVEL:  Integer level of debug printing (from namelist)
  integer :: debug_level

!------------------------------------------------------------------------------

  character (LEN=25) :: units
  character (LEN=46) :: Desc
  real, allocatable, dimension(:,:) :: scr2d
  real, pointer, dimension(:,:) :: ptr2d

  integer :: k, kk, m, n, ierr, ifv
  integer :: iunit=13

  character(LEN=19) :: hdate, hend
  character(LEN=24) :: hdate_output
  character(LEN=3)  :: out_format
  character(LEN=256)  :: prefix
  real :: xfcst, level
  character(LEN=9) :: field

  integer :: ntime, idts

! DATELEN:  length of date strings to use for our output file names.
  integer :: datelen

! Decide the length of date strings to use for output file names.  
! DATELEN is 13 for hours, 16 for minutes, and 19 for seconds.
  if (mod(interval,3600) == 0) then
     datelen = 13
  else if (mod(interval, 60) == 0) then
     datelen = 16
  else
     datelen = 19
  endif

  if ( debug_level .gt. 100 ) then
    call mprintf(.true.,DEBUG,"Begin rrpr")
    call mprintf(.true.,DEBUG,"nfiles = %i , ntimes = %i )",i1=nfiles,i2=ntimes)
    do n = 1, nfiles
      call mprintf(.true.,DEBUG,"filedates(%i) = %s",i1=n,s1=filedates(n))
    enddo
  endif

! Compute the ending time:

  call geth_newdate(hend, hstart, interval*ntimes)

  call clear_storage

! We want to do something for each of the requested times:
  TIMELOOP : do ntime = 1, ntimes
     idts = (ntime-1) * interval
     call geth_newdate(hdate, hstart, idts)
     call mprintf(.true.,DEBUG, &
     "RRPR: hstart = %s , hdate = %s , idts = %i",s1=hstart,s2=hdate,i1=idts)

! Loop over the output file dates, and do stuff if the file date matches
! the requested time we are working on now.

     FILELOOP : do n = 1, nfiles
       if ( debug_level .gt. 100 ) then
         call mprintf(.true.,DEBUG, &
            "hstart = %s , hend = %s",s1=hstart,s2=hend)
         call mprintf(.true.,DEBUG, &
            "filedates(n) = %s",s1=filedates(n))
         call mprintf(.true.,DEBUG, &
            "filedates(n) = %s",s1=filedates(n)(1:datelen))
       end if
       if (filedates(n)(1:datelen).ne.hdate(1:datelen)) cycle FILELOOP
       if (debug_level .gt. 50 ) then
	 call mprintf(.true.,INFORM, &
            "RRPR Processing : %s",s1=filedates(n)(1:datelen))
       endif
       open(iunit, file=trim(get_path(prefix))//'PFILE:'//filedates(n)(1:datelen), &
          form='unformatted',status='old')

! Read the file:

     rdloop: do 
        read (iunit, iostat=ierr) ifv
        if (ierr.ne.0) exit rdloop
        if ( ifv .eq. 5) then     ! WPS
          read (iunit) hdate_output, xfcst, map%source, field, units, Desc, &
               level, map%nx, map%ny, map%igrid
          hdate = hdate_output(1:19)
          select case (map%igrid)
          case (0, 4)
             read (iunit) map%startloc, map%lat1, map%lon1, map%dy, map%dx, map%r_earth
          case (3)
           read (iunit) map%startloc, map%lat1, map%lon1, map%dx, map%dy, map%lov, &
                map%truelat1, map%truelat2, map%r_earth
          case (5)
             read (iunit) map%startloc, map%lat1, map%lon1, map%dx, map%dy, map%lov, &
                map%truelat1, map%r_earth
          case (1)
           read (iunit) map%startloc, map%lat1, map%lon1, map%dy, map%dx, &
                map%truelat1, map%r_earth
          case default
             call mprintf(.true.,ERROR, &
                "Unrecognized map%%igrid: %i in RRPR 1",i1=map%igrid)
          end select
          read (iunit) map%grid_wind

        else if ( ifv .eq. 4 ) then          ! SI
          read (iunit) hdate_output, xfcst, map%source, field, units, desc, level, &
                map%nx, map%ny, map%igrid
          hdate = hdate_output(1:19)
          select case (map%igrid)
          case (0, 4)
             read(iunit) map%startloc, map%lat1, map%lon1, map%dy, map%dx
          case (3)
             read (iunit) map%startloc, map%lat1, map%lon1, map%dx, map%dy, &
                map%lov, map%truelat1, map%truelat2
          case (5)
             read (iunit) map%startloc, map%lat1, map%lon1, map%dx, map%dy, &
                map%lov, map%truelat1
          case default
	     call mprintf(.true.,ERROR, &  
                "Unrecognized map%%igrid: %i in RRPR 2",i1=map%igrid)
          end select

        else if ( ifv .eq. 3 ) then          ! MM5
          read(iunit) hdate_output, xfcst, field, units, desc, level,&
                map%nx, map%ny, map%igrid
          hdate = hdate_output(1:19)
          select case (map%igrid)
          case (3)      ! lamcon
            read (iunit) map%lat1, map%lon1, map%dx, map%dy, map%lov, &
                    map%truelat1, map%truelat2
           case (5)      ! Polar Stereographic
              read (iunit) map%lat1, map%lon1, map%dx, map%dy, map%lov, &
                   map%truelat1
           case (0, 4)      ! lat/lon
              read (iunit) map%lat1, map%lon1, map%dy, map%dx
           case (1)      ! Mercator
              read (iunit) map%lat1, map%lon1, map%dy, map%dx, map%truelat1
           case default
	     call mprintf(.true.,ERROR, &  
                "Unrecognized map%%igrid: %i in RRPR 3",i1=map%igrid)
           end select
        else
           call mprintf(.true.,ERROR, &
              "unknown out_format, ifv = %i",i1=ifv)
        endif

        allocate(ptr2d(map%nx,map%ny))
        read (iunit) ptr2d
        call refw_storage(nint(level), field, ptr2d, map%nx, map%ny)
        nullify (ptr2d)
     enddo rdloop
!
! We have reached the end of file, so time to close it.
!
     close(iunit)
     if (debug_level .gt. 100 ) call print_storage
!
! By now the file has been read completely.  Now, see if we need to fill in 
! missing fields:
!

! Retrieve the number of levels in storage:
!
     call get_plvls(plvl, maxlvl, nlvl)
!
! Fill the surface level (code 200100) from higher 200100s, as necessary
!
        do k = 1, nlvl
           if ((plvl(k).gt.200100) .and. (plvl(k).lt.200200)) then
           ! We found a level between 200100 and 200200, now find the field
           ! corresponding to that level.
              MLOOP : do m = 1, maxvar
                 if (is_there(nint(plvl(k)), namvar(m))) then
                    INLOOP : do kk = 200101, nint(plvl(k))
                       if (is_there(kk, namvar(m))) then
                          if ( debug_level .gt. 100 ) then
                            call mprintf(.true.,DEBUG, &
               "Copying %s at level %i to level 200100.",s1=namvar(m),i1=kk)
                          end if
                          call get_dims(kk, namvar(m))
                          allocate(scr2d(map%nx,map%ny))
                          call get_storage &
                               (kk, namvar(m), scr2d, map%nx, map%ny)
                          call put_storage &
                               (200100,namvar(m), scr2d,map%nx,map%ny)
                          deallocate(scr2d)
                          EXIT INLOOP
                       endif
                    enddo INLOOP
                 endif
              enddo MLOOP
           endif
        enddo

!
! If upper-air U is missing, see if we can interpolate from surrounding levels.
! This is a simple vertical interpolation, linear in pressure.
! Currently, this simply fills in one missing level between two present levels. 
!

        do k = 2, nlvl-1, 1
           if (plvl(k-1) .lt. 200000.) then
              if ( (.not. is_there(nint(plvl(k)),'UU')) .and. &
                   ( is_there(nint(plvl(k-1)), 'UU')) .and.&
                   ( is_there(nint(plvl(k+1)), 'UU')) ) then
                 call get_dims(nint(plvl(k+1)), 'UU')
                 call vntrp(plvl, maxlvl, k, "UU      ", map%nx, map%ny)
              endif
           endif
        enddo

!
! If upper-air V is missing, see if we can interpolate from surrounding levels.
! This is a simple vertical interpolation, linear in pressure.
! Currently, this simply fills in one missing level between two present levels. 
!

        do k = 2, nlvl-1, 1
           if (plvl(k-1) .lt. 200000.) then
              if ( (.not. is_there(nint(plvl(k)),'VV')) .and. &
                   ( is_there(nint(plvl(k-1)), 'VV')) .and.&
                   ( is_there(nint(plvl(k+1)), 'VV')) ) then
                 call get_dims(nint(plvl(k+1)), 'VV')
                 call vntrp(plvl, maxlvl, k, "VV      ", map%nx, map%ny)
              endif
           endif
        enddo

!--- Tanya's change for initializing WRF with RUC
!   This allows for the ingestion for RUC isentropic data
!
        do k = 1, nlvl
           if (plvl(k).lt.200000.) then
              if (.not. is_there(nint(plvl(k)), 'TT').and. &
                   is_there(nint(plvl(k)), 'VPTMP')) then
                 call get_dims(nint(plvl(k)), 'VPTMP')
                 call compute_t_vptmp(map%nx, map%ny, plvl(k))
              endif
           endif
        enddo
!!!
!
! If upper-air T is missing, see if we can interpolate from surrounding levels.
! This is a simple vertical interpolation, linear in pressure.
! Currently, this simply fills in one missing level between two present levels. 
!

        do k = 2, nlvl-1, 1
           if (plvl(k-1) .lt. 200000.) then
              if ( (.not. is_there(nint(plvl(k)),'TT')) .and. &
                   ( is_there(nint(plvl(k-1)), 'TT')) .and.&
                   ( is_there(nint(plvl(k+1)), 'TT')) ) then
                 call get_dims(nint(plvl(k+1)), 'TT')
                 call vntrp(plvl, maxlvl, k, "TT      ", map%nx, map%ny)
              endif
           endif
        enddo

!
! If upper-air SPECHUMD is missing, see if we can compute SPECHUMD from QVAPOR:
!--- Tanya's change for initializing WRF with RUC

        do k = 1, nlvl
           if (plvl(k).lt.200000.) then
              if (.not. is_there(nint(plvl(k)), 'SPECHUMD').and. &
                   is_there(nint(plvl(k)), 'QV')) then
                 call get_dims(nint(plvl(k)), 'QV')
                 call compute_spechumd_qvapor(map%nx, map%ny, plvl(k))
              endif
           endif
        enddo

!
! Check to see if we need to fill HGT from GEOPT.
!
        do k = 1, nlvl
           if (plvl(k).lt.200000.) then
              if (.not. is_there(nint(plvl(k)), 'HGT').and. &
                   is_there(nint(plvl(k)), 'GEOPT')) then
                 call get_dims(nint(plvl(k)), 'GEOPT')
                 allocate(scr2d(map%nx,map%ny))
                 call get_storage(nint(plvl(k)), 'GEOPT', scr2d, map%nx, map%ny)
                 scr2d = scr2d / 9.81
                 call put_storage(nint(plvl(k)), 'HGT',   scr2d, map%nx, map%ny)
                 deallocate(scr2d)
              endif
           endif
        enddo


! If upper-air RH is missing, see if we can compute RH from Specific Humidity:

        do k = 1, nlvl
           if (plvl(k).lt.200000.) then
              if (.not. is_there(nint(plvl(k)), 'RH').and. &
                   is_there(nint(plvl(k)), 'SPECHUMD')) then
                 call get_dims(nint(plvl(k)), 'TT')
                 call compute_rh_spechumd_upa(map%nx, map%ny, plvl(k))
              endif
           endif
        enddo

! If upper-air RH is missing, see if we can compute RH from Vapor Pressure:
!   (Thanks to Bob Hart of PSU ESSC -- 1999-05-27.)

        do k = 1, nlvl
           if (plvl(k).lt.200000.) then
              if (.not. is_there(nint(plvl(k)),'RH').and. &
                   is_there(nint(plvl(k)),'VAPP')) then
                 call get_dims(nint(plvl(k)),'TT')
                 call compute_rh_vapp_upa(map%nx, map%ny, plvl(k))
              endif
           endif
        enddo

! If upper-air RH is missing, see if we can compute RH from Dewpoint Depression:

        do k = 1, nlvl
           if (plvl(k).lt.200000.) then
              if (.not. is_there(nint(plvl(k)),'RH').and. &
                   is_there(nint(plvl(k)),'DEPR')) then
                 call get_dims(nint(plvl(k)),'TT')
                 call compute_rh_depr(map%nx, map%ny, plvl(k))
              endif
           endif
        enddo
!
! If upper-air RH is missing, see if we can interpolate from surrounding levels.
! This is a simple vertical interpolation, linear in pressure.
! Currently, this simply fills in one missing level between two present levels. 
! May expand this in the future to fill in additional levels.  May also expand 
! this in the future to vertically interpolate other variables.
!

        do k = 2, nlvl-1, 1
           if (plvl(k-1) .lt. 200000.) then
              if ( (.not. is_there(nint(plvl(k)),'RH')) .and. &
                   ( is_there(nint(plvl(k-1)), 'RH')) .and.&
                   ( is_there(nint(plvl(k+1)), 'RH')) ) then
                 call get_dims(nint(plvl(k+1)), 'RH')
                 call vntrp(plvl, maxlvl, k, "RH      ", map%nx, map%ny)
              endif
           endif
        enddo

!
! Check to see if we need to fill RH above 300 mb to 10%:
!
        if (is_there(30000, 'RH')) then
           call get_dims(30000, 'RH')
           allocate(scr2d(map%nx,map%ny))
           scr2d = 10.

           do k = 1, nlvl
              if (plvl(k).lt.30000.) then
                 if (.not. is_there(nint(plvl(k)), 'RH')) then
                    call put_storage(nint(plvl(k)),'RH',scr2d,map%nx,map%ny)
                 endif
              endif
           enddo
           deallocate(scr2d)
        endif
!
! If surface RH is missing, see if we can compute RH from Specific Humidity 
! or Dewpoint or Dewpoint depression:
!
        if (.not. is_there (200100, 'RH')) then
           if (is_there(200100, 'TT').and. &
                is_there(200100, 'PSFC'    )   .and. &
                is_there(200100, 'SPECHUMD')) then
              call get_dims(200100, 'TT')
              call compute_rh_spechumd(map%nx, map%ny)
	      call mprintf(.true.,DEBUG, &
                "RRPR:   SURFACE RH is computed")
           elseif (is_there(200100, 'TT'       ).and. &
                is_there(200100, 'DEWPT')) then
              call get_dims(200100, 'TT')
              call compute_rh_dewpt(map%nx, map%ny)
           elseif (is_there(200100, 'TT').and. &
                is_there(200100, 'DEPR')) then
              call get_dims(200100, 'TT')
              call compute_rh_depr(map%nx, map%ny, 200100.)
           endif
        endif

! If we've got a SEAICE field, make sure that it is all Zeros and Ones:

        if (is_there(200100, 'SEAICE')) then
           call get_dims(200100, 'SEAICE')
           call make_zero_or_one(map%nx, map%ny)
        endif

	call mprintf(.true.,INFORM, &
           "RRPR: hdate = %s ",s1=hdate)
        call output(hdate, nlvl, maxlvl, plvl, interval, 2, out_format, prefix, debug_level)
        call clear_storage
        exit FILELOOP
     enddo FILELOOP
   enddo TIMELOOP
end subroutine rrpr

subroutine make_zero_or_one(ix, jx)
! Make sure the SEAICE field is zero or one.
  use storage_module
  implicit none
  integer :: ix, jx
  real, dimension(ix,jx) :: seaice

  call get_storage(200100, 'SEAICE',seaice, ix, jx)
  where(seaice > 0.5)
     seaice = 1.0
  elsewhere
     seaice = 0.0
  end where
  call put_storage(200100, 'SEAICE',seaice, ix, jx)
end subroutine make_zero_or_one


subroutine compute_spechumd_qvapor(ix, jx, plvl)
! Compute specific humidity from water vapor mixing ratio.
  use storage_module
  implicit none
  integer :: ix, jx
  real :: plvl
  real :: lat1, lon1, dx, dy
  real, dimension(ix,jx) :: QVAPOR, SPECHUMD

  real startlat, startlon, deltalat, deltalon

  call get_storage(nint(plvl), 'QV', QVAPOR, ix, jx)

  SPECHUMD = QVAPOR/(1.+QVAPOR)

  call put_storage(nint(plvl), 'SPECHUMD', spechumd, ix, jx)
 if(nint(plvl).eq.1) then
  call put_storage(200100,'SPECHUMD', spechumd, ix, jx)
 endif

end subroutine compute_spechumd_qvapor

subroutine compute_t_vptmp(ix, jx, plvl)
! Compute relative humidity from specific humidity.
  use storage_module
  implicit none
  integer :: ix, jx
  real :: plvl
  real :: lat1, lon1, dx, dy
  real, dimension(ix,jx) :: T, VPTMP, P, Q

  real, parameter :: rovcp=0.28571

  real startlat, startlon, deltalat, deltalon

  call get_storage(nint(plvl), 'VPTMP',  VPTMP, ix, jx)
  IF (nint(plvl) .LT. 200) THEN
    call get_storage(nint(plvl), 'PRESSURE',   P, ix, jx)
  ELSE
    p = plvl
  ENDIF
  call get_storage(nint(plvl), 'SPECHUMD',   Q, ix, jx)

   t=vptmp * (p*1.e-5)**rovcp * (1./(1.+0.6078*Q))  

  call put_storage(nint(plvl), 'TT', t, ix, jx)
       if(nint(plvl).eq.1) then
  call put_storage(200100, 'PSFC', p, ix, jx) 
       endif

end subroutine compute_t_vptmp


subroutine compute_rh_spechumd(ix, jx)
! Compute relative humidity from specific humidity.
  use storage_module
  implicit none
  integer :: ix, jx
  real :: lat1, lon1, dx, dy
  real, dimension(ix,jx) :: T, P, RH, Q

  real, parameter :: svp1=611.2
  real, parameter :: svp2=17.67
  real, parameter :: svp3=29.65
  real, parameter :: svpt0=273.15
  real, parameter :: eps = 0.622

  real startlat, startlon, deltalat, deltalon

  call get_storage(200100, 'TT',        T, ix, jx)
  call get_storage(200100, 'PSFC',     P, ix, jx)
  call get_storage(200100, 'SPECHUMD', Q, ix, jx)

  rh = 1.E2 * (p*q/(q*(1.-eps) + eps))/(svp1*exp(svp2*(t-svpt0)/(T-svp3)))

  call put_storage(200100, 'RH', rh, ix, jx)

end subroutine compute_rh_spechumd

subroutine compute_rh_spechumd_upa(ix, jx, plvl)
! Compute relative humidity from specific humidity.
  use storage_module
  implicit none
  integer :: ix, jx
  real :: plvl
  real :: lat1, lon1, dx, dy
  real, dimension(ix,jx) :: T, P, RH, Q

  real, parameter :: svp1=611.2
  real, parameter :: svp2=17.67
  real, parameter :: svp3=29.65
  real, parameter :: svpt0=273.15
  real, parameter :: eps = 0.622

  real startlat, startlon, deltalat, deltalon

  IF ( nint(plvl).LT. 200) THEN
    call get_storage(nint(plvl), 'PRESSURE', P, ix, jx)
  ELSE
    P = plvl
  ENDIF
  call get_storage(nint(plvl), 'TT',        T, ix, jx)
  call get_storage(nint(plvl), 'SPECHUMD', Q, ix, jx)

  rh = 1.E2 * (p*q/(q*(1.-eps) + eps))/(svp1*exp(svp2*(t-svpt0)/(T-svp3)))
  
  call put_storage(nint(plvl), 'RH', rh, ix, jx)

end subroutine compute_rh_spechumd_upa

subroutine compute_rh_vapp_upa(ix, jx, plvl)
! Compute relative humidity from vapor pressure.
! Thanks to Bob Hart of PSU ESSC -- 1999-05-27.
  use storage_module
  implicit none
  integer :: ix, jx
  real :: plvl
  real :: lat1, lon1, dx, dy
  real, dimension(ix,jx) :: P, ES
  real, pointer, dimension(:,:) :: T, E, RH

  real, parameter :: svp1=611.2
  real, parameter :: svp2=17.67
  real, parameter :: svp3=29.65
  real, parameter :: svpt0=273.15
  real, parameter :: eps = 0.622

  real :: startlat, startlon, deltalat, deltalon

  allocate(RH(ix,jx))

  P = plvl
  call refr_storage(nint(plvl), 'TT',    T, ix, jx)
  call refr_storage(nint(plvl), 'VAPP', E, ix, jx)

  ES=svp1*exp(svp2*(T-svpt0)/(T-svp3))
  rh=min(1.E2*(P-ES)*E/((P-E)*ES), 1.E2)

  call refw_storage(nint(plvl), 'RH', rh, ix, jx)

  nullify(T,E)

end subroutine compute_rh_vapp_upa

subroutine compute_rh_depr(ix, jx, plvl)
! Compute relative humidity from Dewpoint Depression
  use storage_module
  implicit none
  integer :: ix, jx
  real :: plvl
  real :: lat1, lon1, dx, dy
  real, dimension(ix,jx) :: t, depr, rh

  real, parameter :: Xlv = 2.5e6
  real, parameter :: Rv = 461.5

  integer :: i, j

  call get_storage(nint(plvl), 'TT', T,  ix, jx)
  call get_storage(nint(plvl), 'DEPR', DEPR, ix, jx)

  where(DEPR < 100.)
     rh = exp(Xlv/Rv*(1./T - 1./(T-depr))) * 1.E2
  elsewhere
     rh = 0.0
  endwhere

  call put_storage(nint(plvl),'RH      ', rh, ix, jx)

end subroutine compute_rh_depr

subroutine compute_rh_dewpt(ix,jx)
! Compute relative humidity from Dewpoint
  use storage_module
  implicit none
  integer :: ix, jx
  real :: lat1, lon1, dx, dy
  real, dimension(ix,jx) :: t, dp, rh

  real, parameter :: Xlv = 2.5e6
  real, parameter :: Rv = 461.5

  call get_storage(200100, 'TT      ', T,  ix, jx)
  call get_storage(200100, 'DEWPT   ', DP, ix, jx)

  rh = exp(Xlv/Rv*(1./T - 1./dp)) * 1.E2

  call put_storage(200100,'RH      ', rh, ix, jx)

end subroutine compute_rh_dewpt

subroutine vntrp(plvl, maxlvl, k, name, ix, jx)
  use storage_module
  implicit none
  integer :: ix, jx, k, maxlvl
  real, dimension(maxlvl) :: plvl
  character(len=8) :: name
  real, dimension(ix,jx) :: a, b, c
  real :: frc

  write(*,'("Interpolating to fill in ", A, " at level ", I8)') trim(name), nint(plvl(k))

  call  get_storage(nint(plvl(k-1)), name, a, ix, jx)
  call  get_storage(nint(plvl(k+1)), name, c, ix, jx)

  frc = (plvl(k) - plvl(k+1)) / ( plvl(k-1)-plvl(k+1))

  b = (1.-frc)*a + frc*c
!KWM  b = 0.5 * (a + c)
  call  put_storage(nint(plvl(k)), name, b, ix, jx)

end subroutine vntrp

