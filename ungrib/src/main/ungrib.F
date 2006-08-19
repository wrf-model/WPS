!*****************************************************************************!
! program ungrib                                                              !
!                                                                             !
! Questions, comments, suggestions, even complaints should be directed to:    !
!                        wrfhelp@ucar.edu                                     !
! Externals:                                                                  !
!    Module TABLE                                                             !
!    Module GRIDINFO                                                          !
!    Module FILELIST                                                          !
!    Subroutine READ_NAMELIST                                                 !
!    Subroutine PARSE_TABLE                                                   !
!    Subroutine CLEAR_STORAGE                                                 !
!    Subroutine RD_GRIB                                                       !
!    Subroutine RD_GRIB2                                                      !
!    Subroutine PUT_STORAGE                                                   !
!    Subroutine OUTPUT                                                        !
!    Subroutine CCLOSE                                                        !
!    Subroutine RRPR                                                          !
!    Subroutine DATINT                                                        !
!    Subroutine FILE_DELETE                                                   !
!                                                                             !
! Kevin W. Manning                                                            !
! NCAR/MMM                                                                    !
! Summer 1998, and continuing                                                 !
! SDG                                                                         !
!                                                                             !
!*****************************************************************************!
!                                                                             !
! Recent changes:                                                             !
!   2001-02-14:  Allow output file names to have date stamps out to minutes   !
!                or seconds, if the user requests a time interval (in         !
!                seconds) that is not evenly divisible into hours.  For this  !
!                routine (pregrid_grib.F), this simply involves passing       !
!                the interval to routines output.F, rrpr.F, datint.F,         !
!                file_delete.F                                                !
!                                                                             !
!   2000-08-31:  Allow the program to recognize SKINTEMP as something to be   !
!                used as a sea-surface temperature.                           !
!                                                                             !
!*****************************************************************************!
!                                                                             !
! This program reads GRIB-formatted data and puts it into the REGRID format   !
! for passing data to program "regridder".  This format is described in file  !
! "REGRID/pregrid/Doc/Format".                                                !
!                                                                             !
! The program tries to read from files called "GRIBFILE.AAA", "GRIBFILE.AAB", !
! "GRIBFILE.AAC", ... "GRIBFILE.ABA", "GRIBFILE.ABB", ... "GRIBFILE.ZZZ" until!
! it cannot find a file.  This naming format allows for up to 17576 files,    !
! which should be enough for most applications.                               !
!                                                                             !
! The program searches through those "GRIBFILE.???" files, and pulls out all  !
! the requested fields which fall between a starting date and an ending date. !
! It writes the fields from a given time period to a file named according to  !
! the date and hour, i.e., "FILE:YYYY-MM-DD_HH"                               !
!                                                                             !
!*****************************************************************************!
program ungrib

  use table
  use gridinfo
  use storage_module
  use filelist
  use datarray
  implicit none

  integer :: nunit1 = 12
  character(LEN=132) :: gribflnm = 'GRIBFILE.AAA        '    ! won't work with len=12

  integer :: debug_level

  integer , parameter :: maxlvl = 100

  real , dimension(maxlvl) :: plvl
  integer :: iplvl

  integer :: nlvl

  real :: startlat, startlon, deltalat, deltalon
  real :: level
  character (LEN=9) ::  field
  character (LEN=3) ::  out_format

  logical :: readit

  integer, dimension(255) :: iuarr = 0

  character (LEN=19) :: HSTART, HEND, HDATE
  character(LEN=19) :: hsave  = '0000-00-00_00:00:00'
  integer :: itime
  integer :: ntimes
  integer :: interval
  integer :: ierr
  logical :: ordered_by_date
  integer :: grib_version
  integer :: vtable_columns


! -----------------
! Read the namelist, and return the information we want:

  call read_namelist(hstart, hend, interval, ntimes, &
       ordered_by_date, debug_level, out_format)

  write(*,*) 'Interval value:', interval

! -----------------
! Determine GRIB Edition number
  grib_version=0
  call edition_num(nunit1, gribflnm, grib_version, ierr)
  if (ierr.eq.3) STOP 'GRIB file problem' 
  if (grib_version.eq.2) then
     vtable_columns=11 
  else
     vtable_columns=7 
  endif
  print *,'Reading Grib Edition ',grib_version

! -----------------
! Read the "Vtable" file, and put the information contained therein into 
! the module "table".

  call parse_table(debug_level,vtable_columns)

  if ( debug_level .gt. 100 ) then
     write(6,*) 'Parsed the vtable'
  end if

! -----------------
! Initialize the input filename to GRIBFILE.AA{character just before A}
! That way, when we update the filename below for the first time, it will 
! have the correct value of GRIBFILE.AAA.

  gribflnm(12:12)=char(ichar(gribflnm(12:12))-1)

! -----------------
! LOOP2 cycles through the list of files named GRIBFILE.???, until it finds
! a non-existent file.  Then we exit

  LOOP2 : DO

     ! At the beginning of LOOP2 update the input filename.
     if (gribflnm(12:12).eq.'Z') then
	if (gribflnm(11:11).eq.'Z') then
          gribflnm(10:10) = char(ichar(gribflnm(10:10))+1)
          gribflnm(11:11) = 'A'
	else
          gribflnm(11:11) = char(ichar(gribflnm(11:11))+1)
	endif
        gribflnm(12:12) = 'A'
     else
        gribflnm(12:12) = char(ichar(gribflnm(12:12))+1)
     endif

     ! Set READIT to .TRUE., meaning that we have not read any records yet 
     ! from the file GRIBFLNM.

     if ( debug_level .gt. 100 ) then
        write(6,*) 'reading from gribflnm = ',gribflnm
     end if

     readit = .TRUE.  ! i.e., "Yes, we want to read a record."

    
! LOOP1 reads through the file GRIBFLNM, exiting under two conditions:
!        1) We have hit the end-of-file
!        2) We have read past the ending date HEND.
!
! Condition 2 assumes that the data in file GRIBFLNM are ordered in time.

     LOOP1 : DO
        ! At the beginning of LOOP1, we are at a new time period.
        ! Clear the storage arrays and associated level information.
        nlvl = 0
        plvl = -999.
        call clear_storage

! LOOP0 reads through the file GRIBFLNM, looking for data of the current 
! date.  It exits under the following conditions.
!          1) We have hit the end-of-file
!          2) The GRIBFLNM variable has been updated to a nonexistent file.
!          3) We start reading a new date and the data are assumed to be 
!             ordered by date.
!          4) We have a valid record and the data are not assumed to be 
!             ordered by date.

        LOOP0 : DO

           ! If we need to read a new grib record, then read one.
           if (READIT) then

              if (grib_version.ne.2) then 
                 if ( debug_level .gt. 100 ) then
                    write(6,*) 'calling rd_grib1 with iunit ', nunit1
                    write(6,*) 'flnm = ',gribflnm
                 end if
                 ! Read one record at a time from GRIB1 (and older Editions) 
                 call rd_grib1(nunit1, gribflnm, level, field, &
                      hdate, ierr, iuarr, debug_level)
              else 

                 ! Read one file of records from GRIB2.
                 if ( debug_level .gt. 100 ) then
                    write(6,*) 'calling rd_grib2'
                 end if
                 call rd_grib2(nunit1, gribflnm, hdate, &
                      grib_version, ierr, debug_level)
                 FIELD='NULL'

              endif

              if ( debug_level .gt. 100 ) then
                 write(6,*) 'ierr = ',ierr
              end if
              if (ierr.eq.1) then 
                 ! We have hit the end of a file.  Exit LOOP0 so we can 
                 ! write output for date HDATE, and then exit LOOP1
                 ! to update the GRIBFLNM
                 hsave = hdate
                 exit LOOP0
              endif

              if (ierr.eq.2) then
                 ! We have hit the end of all the files.  We can exit LOOP2
                 ! because there are no more files to read.
                 exit LOOP2
              endif

              if (debug_level .gt. 100) then
                 print*, 'Read a record '//field//' with date ', hdate(1:13)
              end if

           endif

           if (debug_level .gt. 100) then
              print*, "hdate, hsave = "//hdate(1:13)//"  "//hsave(1:13)
           end if

!           if (hdate < hstart) then
!              ! The data read has a date HDATE earlier than the starting
!              ! date HSTART.  So cycle LOOP0 to read the the next GRIB record.
!              READIT = .TRUE.
!              cycle LOOP0
!           endif

           if (FIELD.EQ.'NULL') then
              ! The data read does not match any fields requested
              ! in the Vtable.  So cycle LOOP0 to read the next GRIB record.
              READIT = .TRUE.
              cycle LOOP0
           endif

           if (ordered_by_date .and. (hdate > hsave)) then

              ! Exit LOOP0, because we started to read data from another 
              ! date.

              if (debug_level .gt. 100) then
                 write(*, '(A19, " > ", A19, " so exit LOOP0")') hdate, hsave
              endif

              ! We set READIT to FALSE because we have not yet processed
              ! the data from this record, and we will want to process this
              ! data on the next pass of LOOP1 (referring to the next time
              ! period of interest.

              READIT = .FALSE.

              exit LOOP0

           endif

! When we have reached this point, we have a data array ARRAY which has 
! some data we want to save, with field name FIELD at pressure level 
! LEVEL (Pa).  Dimensions of this data are map%nx and map%ny.  Put
! this data into storage.

           if (((field == "SST").or.(field == "SKINTEMP")) .and. &
                (level /= 200100.)) level = 200100.
           iplvl = int(level)
           if (.not.allocated(rdatarray)) then
              write(6,*) 'ERROR: GRIB data slab not allocated in ungrib.F before'
              write(6,*) '       call to put_storage.'
              stop
           end if
           call put_storage(iplvl,field, &
                reshape(rdatarray(1:map%nx*map%ny),(/map%nx, map%ny/)),&
                map%nx,map%ny)
           deallocate(rdatarray)

           ! Since we processed the record we just read, we set
           ! READIT to .TRUE. so that LOOP0 will read the next record.
           READIT = .TRUE.

           if (.not. ordered_by_date) then
              if (hdate >= hstart) then
                 hsave = hdate
              endif
              exit LOOP0
           endif

        enddo LOOP0

! When we have reached this point, we have either hit the end of file, or 
! hit the end of the current date.  Either way, we want to output
! the data found for this date.  This current date is in HSAVE.
! However, if (HSAVE == 0000-00-00_00:00:00), no output is necessary,
! because that 0000 date is the flag for the very opening of a file.

        if ((hsave(1:4).ne.'0000').and.(hsave.le.hend)) then
           if (debug_level .gt. 100) print*, 'Calling output: '//hsave(1:13)
           call output(hsave, nlvl, maxlvl, plvl, interval, 1, out_format, debug_level)
           hsave=hdate

           ! If the next record we process has a date later than HEND,
           ! then time to exit LOOP1.
           if ((ordered_by_date) .and. (hdate.gt.hend)) exit LOOP1

        else
           hsave = hdate
        endif

        ! If we hit the end-of-file, its time to exit LOOP1 so we can
        ! increment the GRIBFLNM to read the next file.
        if (ierr.eq.1) exit LOOP1

     enddo LOOP1

! When we have reached this point, we read all the data we want to from 
! file GRIBFLNM (either because we reached the end-of-file, or we 
! read past the date HEND).  So we close the file and cycle LOOP2 to read 
! the next file.

     if (grib_version.ne.2) then
        call cclose(iuarr(nunit1), debug_level, ierr)
        iuarr(nunit1) = 0
     endif 
     hsave = '0000-00-00_00:00:00'

  enddo LOOP2

! Now Reread, process, and reoutput.

  write(*,*) 'First pass done, doing a reprocess'

  call rrpr(hstart, ntimes, interval, nlvl, maxlvl, plvl, debug_level, out_format)

! Make sure the filedates are in order, with an inefficient sort:

  call sort_filedates

! Interpolate temporally to fill in missing times:

  call datint(filedates, nfiles, hstart, ntimes, interval, out_format)

! Now delete the temporary files:

  call file_delete(filedates, nfiles, "PFILE:", interval)

! And Now we are done:

   print*,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
   print*,'!  Successful completion of ungrib.   !'
   print*,'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'


contains
  subroutine sort_filedates
    implicit none

    integer :: n
    logical :: done
    if (nfiles > 1) then
       done = .FALSE.
       do while ( .not. done)
          done = .TRUE.
          do n = 1, nfiles-1
             if (filedates(n) > filedates(n+1)) then
                filedates(size(filedates)) = filedates(n)
                filedates(n) = filedates(n+1)
                filedates(n+1) = filedates(size(filedates))
                filedates(size(filedates)) = '0000-00-00 00:00:00.0000'
                done = .FALSE.
             endif
          enddo
       enddo
    endif
  end subroutine sort_filedates

end program ungrib
