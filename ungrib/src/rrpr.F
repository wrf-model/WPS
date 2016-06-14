subroutine rrpr(hstart, ntimes, interval, nlvl, maxlvl, plvl, debug_level, out_format, prefix)
!                                                                             !
! In case you are wondering, RRPR stands for "Read, ReProcess, and wRite"     !
!                                                                             !
!*****************************************************************************!
!                                                                             !

  use filelist
  use gridinfo
  use storage_module
  use table
  use module_debug
  use misc_definitions_module
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
  real, allocatable, dimension(:,:) :: scr2d, tmp2d
  real, pointer, dimension(:,:) :: ptr2d

  integer :: k, kk, mm, n, ierr, ifv
  integer :: iunit=13

  character(LEN=19) :: hdate, hend
  character(LEN=24) :: hdate_output
  character(LEN=3)  :: out_format
  character(LEN=MAX_FILENAME_LEN)  :: prefix
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

   write (0,*) 'Name of source model =>',map%source
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
              MLOOP : do mm = 1, maxvar
                 if (is_there(nint(plvl(k)), namvar(mm))) then
                    INLOOP : do kk = 200101, nint(plvl(k))
                       if (is_there(kk, namvar(mm))) then
                          if ( debug_level .gt. 100 ) then
                            call mprintf(.true.,DEBUG, &
               "Copying %s at level %i to level 200100.",s1=namvar(mm),i1=kk)
                          end if
                          call get_dims(kk, namvar(mm))
                          allocate(scr2d(map%nx,map%ny))
                          call get_storage &
                               (kk, namvar(mm), scr2d, map%nx, map%ny)
                          call put_storage &
                               (200100,namvar(mm), scr2d,map%nx,map%ny)
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

!--- Tanya's change for initializing WRF with RUC
!   This allows for the ingestion for RUC isentropic data
!
        do k = 1, nlvl
           if (plvl(k).lt.200000.) then
              if (.not. is_there(nint(plvl(k)), 'TT').and. &
                   is_there(nint(plvl(k)), 'VPTMP').and. &
                   is_there(nint(plvl(k)), 'SPECHUMD')) then
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
                 call mprintf(.true.,DEBUG, &
		   "RRPR:   Computing GHT from GEOPT ")
                 deallocate(scr2d)
              endif
           endif
        enddo

!
! If this is GFS data, we might have data at the level of max wind speed, 
! or the level of the tropopause.  If so, we want to replicate the pressures
! at those levels (new names).  The replicated names are to allow the
! metgrid program to interpolate the 2d pressure array with both a nearest
! neighbor AND a 4-pt technique.  Those two pressures are used in ARW real
! for vertical interpolation of the trop and max wind level data.
!

        if (index(map%source,'NCEP GFS') .ne. 0 ) then
          call mprintf(.true.,DEBUG, &
             "RRPR:   Replicating GFS pressures for max wind and trop")
            if ( is_there(200100,'PMAXW  ') .or. &
                 is_there(200100,'PTROP  ') ) then
              call gfs_trop_maxw_pressures (map%nx, map%ny)
            endif
        endif

! Repair GFS and ECMWF pressure-level RH
        if (index(map%source,'NCEP GFS') .ne. 0 .or.  &
            index(map%source,'NCEP CDAS CFSV2') .ne. 0 .or.  &
            index(map%source,'ECMWF') .ne. 0 ) then
          call mprintf(.true.,DEBUG, &
             "RRPR:   Adjusting RH values ")
          do k = 1, nlvl
            if ( is_there(nint(plvl(k)),'RH') .and. &
                 is_there(nint(plvl(k)),'TT') ) then
              call fix_gfs_rh (map%nx, map%ny, plvl(k))
            endif
          enddo
        endif

! If upper-air RH is missing, see if we can compute RH from Specific Humidity:

        do k = 1, nlvl
           if (plvl(k).lt.200000.) then
              if (.not. is_there(nint(plvl(k)), 'RH') .and. &
                   is_there(nint(plvl(k)), 'TT') .and. &
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
                   is_there(nint(plvl(k)), 'TT') .and. &
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
                   is_there(nint(plvl(k)), 'TT') .and. &
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
! Check to see if we need to fill RH above 300 mb:
!
        if (is_there(30000, 'RH')) then
           call get_dims(30000, 'RH')
           allocate(scr2d(map%nx,map%ny))

           do k = 1, nlvl
!   Set missing RH to 5% between 300 and 70 hPa. Set RH to 0 above 70 hPa.
!   The stratospheric RH will be adjusted further in real.
              if (plvl(k).le.7000.) then
                scr2d = 0.
              else if (plvl(k).lt.30000.) then
                scr2d = 5.
              endif
              if (plvl(k).lt.30000. .and. plvl(k) .gt. 10. ) then
              ! levels higher than .1 mb are special - do not fill
                 if (.not. is_there(nint(plvl(k)), 'RH')) then
                    call put_storage(nint(plvl(k)),'RH',scr2d,map%nx,map%ny)
                    call mprintf(.true.,DEBUG, &
                 "RRPR:   RH missing at %i hPa, inserting synthetic RH ",i1=nint(plvl(k)/100.))
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

!
! If surface SNOW is missing, see if we can compute SNOW from SNOWRUC
! (From Wei Wang, 2007 June 21, modified 12/28/2007)
!
        if (.not. is_there(200100, 'SNOW') .and. &
             is_there(200100, 'SNOWRUC')) then
           call get_dims(200100, 'SNOWRUC')
           allocate(scr2d(map%nx,map%ny))
           call get_storage(200100, 'SNOWRUC', scr2d, map%nx, map%ny)
           scr2d = scr2d * 1000. 
           call put_storage(200100, 'SNOW',   scr2d, map%nx, map%ny)
           deallocate(scr2d)
        endif

! compute snow water equivalent (SNOW) for NCEP RUC  models
! As of Sept. 14  2011
        if ( index(map%source,'NCEP RUC Model') .ne. 0) then
          if (is_there(200100, 'SNOWH') .and. .not. is_there(200100, 'SNOW')) then
          call get_dims(200100, 'SNOWH')
          allocate(scr2d(map%nx,map%ny))
          call get_storage(200100, 'SNOWH', scr2d, map%nx, map%ny)
          call mprintf(.true.,DEBUG, &
             "RRPR:   Computing SNOWH from SNOW")
            if (is_there(200100, 'RHOSN')) then        ! If we have snow density, use it to compute snowh
              call get_dims(200100, 'RHOSN')
              allocate(tmp2d(map%nx,map%ny))
              call get_storage(200100, 'RHOSN', tmp2d, map%nx, map%ny)
              scr2d = scr2d * tmp2d
              deallocate(tmp2d)
            else
              scr2d = scr2d * 200.0          ! Assume 200:1 ratio
            endif
          call put_storage(200100, 'SNOW', scr2d, map%nx, map%ny)
          deallocate(scr2d)
          endif
        endif

! Add residual soil moisture to SOILM* if initialized from the GSD RUC model or from NCEP RUC
      if (index(map%source,'NOAA GSD') .ne. 0 .or.    &
          index(map%source,'NCEP RUC Model') .ne. 0) then
            if ( .not. is_there(200100, 'SOILM000') .and.& 
                       is_there(200100, 'SM000ruc') ) then
           call get_dims(200100, 'SM000ruc')
             print *,'Adjust RUC soil moisture'
          call mprintf(.true.,DEBUG, &
             "RRPR:   Adjusting RUC soil moisture ")
              call fix_ruc_soilm (map%nx, map%ny)
            endif
       endif

!
! Check to see if we need to fill SOILHGT from SOILGEO.
! (From Wei Wang, 2007 June 21)
!
        if (.not. is_there(200100, 'SOILHGT') .and. &
             is_there(200100, 'SOILGEO')) then
           call get_dims(200100, 'SOILGEO')
           allocate(scr2d(map%nx,map%ny))
           call get_storage(200100, 'SOILGEO', scr2d, map%nx, map%ny)
           scr2d = scr2d / 9.81
           call put_storage(200100, 'SOILHGT', scr2d, map%nx, map%ny)
           call mprintf(.true.,DEBUG, &
             "RRPR:   Computing SOILGHT from SOILGEO ")
           deallocate(scr2d)
        endif

! For hybrid-level input, soilgeo is in level 1 (e.g. ERA40)
        if (.not. is_there(200100, 'SOILHGT') .and. &
             is_there(1, 'SOILGEO')) then
           call get_dims(1, 'SOILGEO')
           allocate(scr2d(map%nx,map%ny))
           call get_storage(1, 'SOILGEO', scr2d, map%nx, map%ny)
           scr2d = scr2d / 9.81
           call put_storage(200100, 'SOILHGT', scr2d, map%nx, map%ny)
           deallocate(scr2d)
        endif

! For NCEP RR (using the same ID as for RUC) native-level input, 
! may need to move PSFC from level 1 to 2001.
! From TGS 8 Sept. 2011
        if ( index(map%source,'NCEP RUC Model') .ne. 0) then
        if (.not. is_there(200100, 'PSFC') .and. &
             is_there(1, 'PRESSURE')) then
    print *,'Process PSFC for NCEP RR'
           call get_dims(1, 'PRESSURE')
           allocate(scr2d(map%nx,map%ny))
           call get_storage(1, 'PRESSURE', scr2d, map%nx, map%ny)
           call put_storage(200100, 'PSFC', scr2d, map%nx, map%ny)
           deallocate(scr2d)
        endif
        endif

! For ECMWF hybrid-level input, may need to move psfc from level 1 to 2001.
        if ( index(map%source,'ECMWF') .ne. 0) then
        if (.not. is_there(200100, 'PSFC') .and. &
             is_there(1, 'PSFCH')) then
           call get_dims(1, 'PSFCH')
           allocate(scr2d(map%nx,map%ny))
           call get_storage(1, 'PSFCH', scr2d, map%nx, map%ny)
           call put_storage(200100, 'PSFC', scr2d, map%nx, map%ny)
           deallocate(scr2d)
        endif
        endif

! ECMWF snow depth in meters of water equivalent (Table 128). Convert to kg/m2 
! 
        if (is_there(200100, 'SNOW_EC')) then
           call get_dims(200100, 'SNOW_EC')
           allocate(scr2d(map%nx,map%ny))
           call get_storage(200100, 'SNOW_EC', scr2d, map%nx, map%ny)
           scr2d = scr2d * 1000.
           call put_storage(200100, 'SNOW', scr2d, map%nx, map%ny)
           deallocate(scr2d)
        endif

! Convert the ECMWF LANDSEA mask from a fraction to a flag

        if ( index(map%source,'ECMWF') .ne. 0) then
        if (is_there(200100, 'LANDSEA')) then
           call get_dims(200100, 'LANDSEA')
           call make_zero_or_one(map%nx, map%ny, 'LANDSEA')
        endif
        endif

! NCEP GFS weasd is one-half of the NAM value. Increase it for use in WRF.
! The GFS-based reanalyses values should be OK as is.
        if ((index(map%source,'NCEP GFS') .ne. 0 .or. &
            index(map%source,'NCEP GEFS') .ne. 0) .and. &
            is_there(200100, 'SNOW')) then
           call mprintf(.true.,DEBUG, &
              "RRPR:   Recomputing SNOW for NCEP GFS")
           call get_dims(200100, 'SNOW')
           allocate(scr2d(map%nx,map%ny))
           call get_storage(200100, 'SNOW', scr2d, map%nx, map%ny)
           scr2d = scr2d * 2.
           call put_storage(200100, 'SNOW', scr2d, map%nx, map%ny)
           deallocate(scr2d)
        endif

! compute physical snow depth (SNOWH) for various models
! As of March 2011, this is done here instead of real because we have model
! source information.
        if (is_there(200100, 'SNOW') .and. .not. is_there(200100, 'SNOWH')) then
          call get_dims(200100, 'SNOW')
          allocate(scr2d(map%nx,map%ny))
          call get_storage(200100, 'SNOW', scr2d, map%nx, map%ny)
          call mprintf(.true.,DEBUG, &
             "RRPR:   Computing SNOWH from SNOW")
          if ( index(map%source,'NCEP ') .ne. 0) then
            scr2d = scr2d * 0.005          ! Assume 200:1 ratio as used at NCEP and in NOAH
          else if (index(map%source,'ECMWF') .ne. 0) then
            if (is_there(200100, 'SNOW_DEN')) then        ! If we have snow density, use it to compute snowh
              call get_dims(200100, 'SNOW_DEN')
              allocate(tmp2d(map%nx,map%ny))
              call get_storage(200100, 'SNOW_DEN', tmp2d, map%nx, map%ny)
              scr2d = scr2d / tmp2d 
              deallocate(tmp2d)
            else
              scr2d = scr2d * 0.004     ! otherwise, assume a density of 250 mm/m (i.e. 250:1 ratio).
            endif
          else                       ! Other models
            scr2d = scr2d * 0.005    ! Use real's default method (200:1)
          endif
          call put_storage(200100, 'SNOWH', scr2d, map%nx, map%ny)
          deallocate(scr2d)
        endif

! As of March 2011, SEAICE can be a flag or a fraction. It will be converted
! to the appropriate values in real depending on whether or not the polar mods are used.

!! If we've got a SEAICE field, make sure that it is all Zeros and Ones:

!       if (is_there(200100, 'SEAICE')) then
!          call get_dims(200100, 'SEAICE')
!          call make_zero_or_one(map%nx, map%ny, 'SEAICE')
!       endif

! If we've got an ICEMASK field, re-flag it for output to met_em and real:
!     Field  | GRIB In  |  Out
!    -------------------------
!    water   |    0     |  0 
!    land    |   -1     |  1
!    ice     |    1     |  0

        if (is_there(200100, 'ICEMASK')) then
           call get_dims(200100, 'ICEMASK')
           call re_flag_ice_mask(map%nx, map%ny)
        endif

! If we have an ICEFRAC field, convert from % to fraction
        if (is_there(200100, 'ICEFRAC')) then
           call get_dims(200100, 'ICEFRAC')
           allocate(scr2d(map%nx,map%ny))
           call get_storage(200100, 'ICEFRAC', scr2d, map%nx, map%ny)
           scr2d = scr2d / 100.
           call put_storage(200100, 'ICEFRAC', scr2d, map%nx, map%ny)
           deallocate(scr2d)
        endif


        call mprintf(.true.,INFORM, &
           "RRPR: hdate = %s ",s1=hdate)
        call output(hdate, nlvl, maxlvl, plvl, interval, 2, out_format, prefix, debug_level)
        call clear_storage
        exit FILELOOP
     enddo FILELOOP
   enddo TIMELOOP
end subroutine rrpr

subroutine make_zero_or_one(ix, jx, infield)
! Make sure the input field (SEAICE or LANDSEA) is zero or one.
  use storage_module
  implicit none
  integer :: ix, jx
  real, dimension(ix,jx) :: seaice
  character(len=*) :: infield

  call get_storage(200100, infield, seaice, ix, jx)
  where(seaice > 0.5)
     seaice = 1.0
  elsewhere
     seaice = 0.0
  end where
  call put_storage(200100, infield, seaice, ix, jx)
end subroutine make_zero_or_one

subroutine re_flag_ice_mask(ix, jx)
!
! Change land points from -1 to 1
! Change ice  points from  1 to 0
! Water       points stay    at 0
!
  use storage_module
  implicit none
  integer :: ix, jx
  real, dimension(ix,jx) :: iceflag

  call get_storage(200100, 'ICEMASK',iceflag, ix, jx)
  where(iceflag > 0.5)     ! Ice points, set to water value
     iceflag = 0.0
  end where
  where(iceflag < -0.5)    ! Land points
     iceflag = 1.0
  end where
  call put_storage(200100, 'ICEMASK',iceflag, ix, jx)
end subroutine re_flag_ice_mask

subroutine compute_spechumd_qvapor(ix, jx, plvl)
! Compute specific humidity from water vapor mixing ratio.
  use storage_module
  implicit none
  integer :: ix, jx
  real :: plvl
  real, dimension(ix,jx) :: QVAPOR, SPECHUMD

  call get_storage(nint(plvl), 'QV', QVAPOR, ix, jx)

  SPECHUMD = QVAPOR/(1.+QVAPOR)

  call put_storage(nint(plvl), 'SPECHUMD', spechumd, ix, jx)
 if(nint(plvl).eq.1) then
  call put_storage(200100,'SPECHUMD', spechumd, ix, jx)
 endif

end subroutine compute_spechumd_qvapor

subroutine compute_t_vptmp(ix, jx, plvl)
! Compute temperature from virtual potential temperature
  use storage_module
  implicit none
  integer :: ix, jx
  real :: plvl
  real, dimension(ix,jx) :: T, VPTMP, P, Q

  real, parameter :: rovcp=0.28571

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
  real, dimension(ix,jx) :: T, P, RH, Q

  real, parameter :: svp1=611.2
  real, parameter :: svp2=17.67
  real, parameter :: svp3=29.65
  real, parameter :: svpt0=273.15
  real, parameter :: eps = 0.622

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
  real, dimension(ix,jx) :: T, P, RH, Q

  real, parameter :: svp1=611.2
  real, parameter :: svp2=17.67
  real, parameter :: svp3=29.65
  real, parameter :: svpt0=273.15
  real, parameter :: eps = 0.622

  IF ( nint(plvl).LT. 200) THEN
    if (is_there(nint(plvl), 'PRESSURE')) then
      call get_storage(nint(plvl), 'PRESSURE', P, ix, jx)
    else
      return     ! if we don't have pressure on model levels, return
    endif
  ELSE
    P = plvl
  ENDIF
  call get_storage(nint(plvl), 'TT',        T, ix, jx)
  call get_storage(nint(plvl), 'SPECHUMD', Q, ix, jx)
  Q=MAX(1.E-10,Q)

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
  real, dimension(ix,jx) :: P, ES
  real, pointer, dimension(:,:) :: T, E, RH

  real, parameter :: svp1=611.2
  real, parameter :: svp2=17.67
  real, parameter :: svp3=29.65
  real, parameter :: svpt0=273.15

  allocate(RH(ix,jx))

  IF ( nint(plvl).LT. 200) THEN
    if (is_there(nint(plvl), 'PRESSURE')) then
      call get_storage(nint(plvl), 'PRESSURE', P, ix, jx)
    else
      return     ! if we don't have pressure on model levels, return
    endif
  ELSE
    P = plvl
  ENDIF
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
  real, dimension(ix,jx) :: t, dp, rh

  real, parameter :: Xlv = 2.5e6
  real, parameter :: Rv = 461.5

  call get_storage(200100, 'TT      ', T,  ix, jx)
  call get_storage(200100, 'DEWPT   ', DP, ix, jx)

  rh = exp(Xlv/Rv*(1./T - 1./dp)) * 1.E2

  call put_storage(200100,'RH      ', rh, ix, jx)

end subroutine compute_rh_dewpt

subroutine gfs_trop_maxw_pressures(ix,jx)
! These are duplicate pressure values from the GFS, for
! the level of max wind speed and for the trop level.
! The duplicates are saved with a different name, so that
! the metgrid program can horizontally interpolate them to
! the model domain with a nearest neighbor method.
  use storage_module
  implicit none
  integer :: ix, jx
  real, dimension(ix,jx) :: pmaxw, pmaxwnn, ptrop, ptropnn

  if ( is_there(200100, 'PMAXW   ') ) then
     call get_storage(200100, 'PMAXW   ', pmaxw  , ix, jx)
     pmaxwnn = pmaxw
     call put_storage(200100, 'PMAXWNN ', pmaxwnn, ix, jx)
  end if

  if ( is_there(200100, 'PTROP   ') ) then
     call get_storage(200100, 'PTROP   ', ptrop  , ix, jx)
     ptropnn = ptrop
     call put_storage(200100, 'PTROPNN ', ptropnn, ix, jx)
  end if

end subroutine gfs_trop_maxw_pressures

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

  b = (1.-frc)*c + frc*a
!KWM  b = 0.5 * (a + c)
  call  put_storage(nint(plvl(k)), name, b, ix, jx)

end subroutine vntrp

subroutine fix_gfs_rh (ix, jx, plvl)
! This routine replaces GFS RH (wrt ice) with RH wrt liquid (which is what is assumed in real.exe).
  use storage_module
  implicit none
  integer :: ix, jx, i, j
  real :: plvl, eis, ews, r
  real, allocatable, dimension(:,:) :: rh, tt

  allocate(rh(ix,jx))
  allocate(tt(ix,jx))
  call get_storage(nint(plvl), 'RH', rh, ix, jx)
  call get_storage(nint(plvl), 'TT', tt, ix, jx)
  do j = 1, jx
  do i = 1, ix
    if ( tt(i,j) .le. 273.15 ) then
      ! Murphy and Koop 2005 ice saturation vapor pressure.
      ! eis and ews in hPA, tt is in K
      eis = .01 * exp (9.550426 - (5723.265 / tt(i,j)) + (3.53068 * alog(tt(i,j))) &
         - (0.00728332 * tt(i,j)))
      ! Bolton 1980 liquid saturation vapor pressure. For water saturation, most 
      ! formulae are very similar from 0 to -20, so we don't need a more exact formula.

      ews = 6.112 * exp(17.67 * (tt(i,j)-273.15) / ((tt(i,j)-273.15)+243.5))
      if ( tt(i,j) .gt. 253.15 ) then
        ! A linear approximation to the GFS blending region ( -20 > T < 0 )
        r = ((273.15 - tt(i,j)) / 20.)
        r = (r * eis) + ((1-r)*ews)
      else
        r = eis
      endif
      rh(i,j) = rh(i,j) * (r / ews)
    endif
  enddo
  enddo
  call put_storage(nint(plvl), 'RH', rh, ix, jx)
  deallocate (rh)
  deallocate (tt)
end subroutine fix_gfs_rh


subroutine fix_ruc_soilm (ix, jx)
! This routine adds residual soil moisture if initialized fron RUC
  use storage_module
  implicit none
  integer :: ix, jx, i, j
  REAL , DIMENSION(100) :: lqmi
  real, allocatable, dimension(:,:) :: soilm000, soilm005, soilm020, &
                     soilm040, soilm160, soilm300,soilcat
  allocate(soilm000(ix,jx))
  allocate(soilm005(ix,jx))
  allocate(soilm020(ix,jx))
  allocate(soilm040(ix,jx))
  allocate(soilm160(ix,jx))
  allocate(soilm300(ix,jx))
  allocate(soilcat(ix,jx))
  call get_storage(200100, 'SM000ruc', soilm000, ix, jx)
  call get_storage(200100, 'SM005ruc', soilm005, ix, jx)
  call get_storage(200100, 'SM020ruc', soilm020, ix, jx)
  call get_storage(200100, 'SM040ruc', soilm040, ix, jx)
  call get_storage(200100, 'SM160ruc', soilm160, ix, jx)
  call get_storage(200100, 'SM300ruc', soilm300, ix, jx)

  call get_storage(200100, 'SOILCAT', soilcat, ix, jx)

      lqmi(1:16) = &
      (/0.045, 0.057, 0.065, 0.067, 0.034, 0.078, 0.10,     &
        0.089, 0.095, 0.10,  0.070, 0.068, 0.078, 0.0,      &
        0.004, 0.065 /)

  do j = 1, jx
  do i = 1, ix

         SOILM000(i,j)=SOILM000(i,j) + lqmi(nint(soilcat(i,j)))
         SOILM005(i,j)=SOILM005(i,j) + lqmi(nint(soilcat(i,j)))
         SOILM020(i,j)=SOILM020(i,j) + lqmi(nint(soilcat(i,j)))
         SOILM040(i,j)=SOILM040(i,j) + lqmi(nint(soilcat(i,j)))
         SOILM160(i,j)=SOILM160(i,j) + lqmi(nint(soilcat(i,j)))
         SOILM300(i,j)=SOILM300(i,j) + lqmi(nint(soilcat(i,j)))
  enddo
  enddo
  call put_storage(200100, 'SOILM000', soilm000, ix, jx)
  call put_storage(200100, 'SOILM005', soilm005, ix, jx)
  call put_storage(200100, 'SOILM020', soilm020, ix, jx)
  call put_storage(200100, 'SOILM040', soilm040, ix, jx)
  call put_storage(200100, 'SOILM160', soilm160, ix, jx)
  call put_storage(200100, 'SOILM300', soilm300, ix, jx)

 print *,'fix_ruc_soilm is done!'

  deallocate(soilm000)
  deallocate(soilm005)
  deallocate(soilm020)
  deallocate(soilm040)
  deallocate(soilm160)
  deallocate(soilm300)
  deallocate(soilcat)

end subroutine fix_ruc_soilm
 

