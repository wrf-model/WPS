!*****************************************************************************!
! Subroutine RD_GRIB2                                                         !
!                                                                             !
! Purpose:                                                                    !
!    Read one record from the input GRIB2 file.  Based on the information in  !
!    the GRIB2 header and the user-defined Vtable, decide whether the field in!
!    the GRIB2 record is one to process or to skip.  If the field is one we   !
!    want to keep, extract the data from the GRIB2 record, and pass the data  !
!    back to the calling routine.                                             !
!                                                                             !
! Argument list:                                                              !
!    Input:                                                                   !
!       JUNIT   : "Unit Number" to open and read from.  Not really a Fortran  !
!                 unit number, since we do not do Fortran I/O for the GRIB2   !
!                 files.  Nor is it a UNIX File Descriptor returned from a C  !
!                 OPEN statement.  It is really just an array index to the    !
!                 array (IUARR) where the UNIX File Descriptor values are     !
!                 stored.                                                     !
!       GRIB2FILE: File name to open, if it is not already open.              !
!       ALENGTH : Length of an array to hold the 2d slab read from the GRIB2  !
!                 record.  This is a parameter in the main program.           !
!       DEBUG_LEVEL  : Integer for various amounts of printout.               !
!                                                                             !
!    Output:                                                                  !
!                                                                             !
!       HDATE    : The 19-character date of the field to process.             !
!       IERR     : Error flag: 0 - no error on read from GRIB2 file.          !
!                              1 - Hit the end of the GRIB2 file.             !
!                              2 - The file GRIBFLNM we tried to open does    !
!                                  not exist.                                 !
!                                                                             !
!                                                                             !
! Author: Paula McCaslin,                                                     !
! NOAA/FSL                                                                    !
! Sept 2004                                                                   !
! Code is based on code developed by Steve Gilbert NCEP & Kevin Manning NCAR  !
!*****************************************************************************!
      
      SUBROUTINE rd_grib2(junit, gribflnm, hdate, 
     &  grib_edition, ireaderr, debug_level)

      use grib_mod
      use params
      use table          ! Included to define g2code
      use gridinfo       ! Included to define map%
      use storage_module ! Included sub put_storage

      real, allocatable, dimension(:) :: hold_array
      parameter(msk1=32000,msk2=4000)
      character(len=1),allocatable,dimension(:) :: cgrib
      integer :: listsec0(3)
      integer :: listsec1(13)
      integer year, month, day, hour, minute, second, fcst
      character(len=*)  :: gribflnm
      character(len=*)  :: hdate
      character(len=8)  :: pabbrev
      character(len=20) :: labbrev
      character(len=80) :: tabbrev
      integer :: lskip, lgrib
      integer :: junit, itot, icount, iseek
      integer :: grib_edition
      integer :: i, j, ireaderr, ith , debug_level
      integer :: currlen
      logical :: unpack, expand
      type(gribfield) :: gfld
      ! For subroutine put_storage
      real :: level
      real :: scale_factor
      integer :: iplvl
      character (len=9) :: my_field
      character (len=8) :: tmp8
      ! For subroutine outout
      integer , parameter :: maxlvl = 100
      real , dimension(maxlvl) :: plvl
      integer :: nlvl
      integer , dimension(maxlvl) :: level_array

C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  SET ARGUMENTS

      call start()
      unpack=.true.
      expand=.true.
      hdate = '0000-00-00_00:00:00'
      ierr=0
      itot=0
      icount=0
      iseek=0
      lskip=0
      lgrib=0
      currlen=0
      ith=1
      scale_factor = 1e6

!/* IOS Return Codes from BACIO:  */
!/*  0    All was well                                   */
!/* -1    Tried to open read only _and_ write only       */
!/* -2    Tried to read and write in the same call       */
!/* -3    Internal failure in name processing            */
!/* -4    Failure in opening file                        */
!/* -5    Tried to read on a write-only file             */
!/* -6    Failed in read to find the 'start' location    */
!/* -7    Tried to write to a read only file             */
!/* -8    Failed in write to find the 'start' location   */
!/* -9    Error in close                                 */
!/* -10   Read or wrote fewer data than requested        */

!if ireaderr =1 we have hit the end of a file. 
!if ireaderr =2 we have hit the end of all the files. 
 

      ! Open a byte-addressable file.
      CALL BAOPENR(junit,gribflnm,IOS)
      if (ios.eq.0) then 
      VERSION: do

         ! Search opend file for the next GRIB2 messege (record).
         call skgb(junit,iseek,msk1,lskip,lgrib)

         ! Check for EOF, or problem
         if (lgrib.eq.0) then
            exit 
         endif

         ! Check size, if needed allocate more memory.
         if (lgrib.gt.currlen) then
            if (allocated(cgrib)) deallocate(cgrib)
            allocate(cgrib(lgrib),stat=is)
            !print *,'G2 allocate(cgrib(lgrib)) status: ',IS
            currlen=lgrib
         endif

         ! Read a given number of bytes from unblocked file.
         call baread(junit,lskip,lgrib,lengrib,cgrib)

         if (lgrib.ne.lengrib) then
            print *,'G2 rd_grib2: IO Error.',lgrib,".ne.",lengrib
            call errexit(9)
         endif
         iseek=lskip+lgrib
         icount=icount+1

         !PRINT *
         !PRINT *,'G2 GRIB MESSAGE ',icount,' starts at',lskip+1

         ! Unpack GRIB2 field
         call gb_info(cgrib,lengrib,listsec0,listsec1,
     &                numfields,numlocal,maxlocal,ierr)
         if (ierr.ne.0) then
           write(*,*) ' ERROR querying GRIB2 message = ',ierr
           stop 10
         endif
         itot=itot+numfields

         grib_edition=listsec0(2)
         if (grib_edition.ne.2) then
              exit VERSION
         endif
         
         ! Additional print statments for developer.
         if ( debug_level .GT. 100 ) then
         print *,'G2 SECTION 0: ',(listsec0(j),j=1,3)
         print *,'G2 SECTION 1: ',(listsec1(j),j=1,13)
         print *,'G2 Contains ',numlocal,' Local Sections ',
     &           ' and ',numfields,' data fields.'
         endif


         ! ----
         ! Once per file fill in date, model and projection values.

         if (lskip.lt.10) then 

           ! Build the 19-character date string, based on GRIB2 header date
           ! and time information, including forecast time information:

           n=1
           call gf_getfld(cgrib,lengrib,n,unpack,expand,gfld,ierr)
           year  =gfld%idsect(6)     !(FOUR-DIGIT) YEAR OF THE DATA
           month =gfld%idsect(7)     ! MONTH OF THE DATA
           day   =gfld%idsect(8)     ! DAY OF THE DATA
           hour  =gfld%idsect(9)     ! HOUR OF THE DATA
           minute=gfld%idsect(10)    ! MINUTE OF THE DATA
           second=gfld%idsect(11)    ! SECOND OF THE DATA

           fcst = 0
           ! Parse the forecast time info from Sect 4. 
           if (gfld%ipdtnum.eq.0) then  ! Product Definition Template 4.0

             ! Extract forecast time.
             fcst = gfld%ipdtmpl(9)

           endif

           ! Compute valid time. 

           !print *, 'ymd',gfld%idsect(6),gfld%idsect(7),gfld%idsect(8)
           !print *, 'hhmm  ',gfld%idsect(9),gfld%idsect(10)
   
           call build_hdate(hdate,year,month,day,hour,minute,second)
           if ( debug_level .gt. 100 ) then
              print *, 'G2 hdate = ',hdate
           end if
           call geth_newdate(hdate,hdate,3600*fcst)
           if ( debug_level .gt. 100 ) then
              print *, 'G2 hdate (fcst?) = ',hdate
           end if

           !--

           ! Indicator of the source (center) of the data.
           icenter = gfld%idsect(1)

           ! Indicator of model (or whatever) which generated the data.
           iprocess = gfld%ipdtmpl(5)


           if (icenter.eq.7) then
             if (iprocess.eq.83 .or. iprocess.eq.84) then
               map%source = 'NCEP NAM Model'
             elseif (iprocess.eq.81) then
               map%source = 'NCEP GFS Model'
             elseif (iprocess.eq.96) then
               map%source = 'NCEP GFS Model'
             elseif (iprocess.eq.109) then
               map%source = 'NCEP RTMA'
             elseif (iprocess.eq.105) then
               map%source = 'NCEP RUC Model'
             elseif (iprocess.eq.140) then
               map%source = 'NARR'
             else
               map%source = 'unknown model from NCEP'
	       write (6,*) 'iprocess = ',iprocess
             end if
           else
             map%source = 'unknown model and orig center'
           end if

           !--

           ! Store information about the grid on which the data is. 
           ! This stuff gets stored in the MAP variable, as defined in 
           ! module GRIDINFO.

           map%startloc = 'SWCORNER'

           if (gfld%igdtnum.eq.0) then ! Lat/Lon grid aka Cylindrical Equidistant
              map%igrid = 0
              map%nx = gfld%igdtmpl(8)
              map%ny = gfld%igdtmpl(9)
              map%dx = gfld%igdtmpl(17)
              map%dy = gfld%igdtmpl(18)
              map%lat1 = gfld%igdtmpl(12)
              map%lon1 = gfld%igdtmpl(13)

              ! Scale dx/dy values to degrees, default range is 1e6.
              if (map%dx.gt.10000) then 
                 map%dx = map%dx/scale_factor
              endif
              if (map%dy.gt.10000) then 
                 map%dy = map%dy/scale_factor
              endif

              ! Scale lat/lon values to 0-180, default range is 1e6.
              if (map%lat1.ge.scale_factor) then 
                 map%lat1 = map%lat1/scale_factor
              endif
              if (map%lon1.ge.scale_factor) then 
                 map%lon1 = map%lon1/scale_factor
              endif

	      ! the following is needed for NCEP GFS, 0.5 degree output
	      if ( map%lat1 .gt. gfld%igdtmpl(15) .and. 
     &               map%dy .gt. 0. ) then
                map%dy = -1. * map%dy
                write(6,*) 'Resetting map%dy for iprocess = ',iprocess
              endif

           elseif (gfld%igdtnum.eq.30) then ! Lambert Conformal Grid
              map%igrid = 3
              map%nx = gfld%igdtmpl(8)
              map%ny = gfld%igdtmpl(9)
              map%lov = gfld%igdtmpl(14) / scale_factor
              map%truelat1 = gfld%igdtmpl(19) / scale_factor
              map%truelat2 = gfld%igdtmpl(20) / scale_factor
              map%dx = gfld%igdtmpl(15) / scale_factor
              map%dy = gfld%igdtmpl(16) / scale_factor
              map%lat1 = gfld%igdtmpl(10) / scale_factor
              map%lon1 = gfld%igdtmpl(11) / scale_factor

           elseif(gfld%igdtnum.eq.40) then ! Gaussian Grid (we will call it lat/lon)
              map%igrid = 0
              map%nx = gfld%igdtmpl(8)
              map%ny = gfld%igdtmpl(9)
              map%dx = gfld%igdtmpl(17)
              map%dy = gfld%igdtmpl(18) ! ?not in Grid Definition Template 3.40 doc
              map%lat1 = gfld%igdtmpl(12)
              map%lon1 = gfld%igdtmpl(13)

              ! Scale dx/dy values to degrees, default range is 1e6.
              if (map%dx.gt.10000) then 
                 map%dx = map%dx/scale_factor
              endif
              if (map%dy.gt.10000) then 
                 map%dy = (map%dy/scale_factor)*(-1)
              endif

              ! Scale lat/lon values to 0-180, default range is 1e6.
              if (map%lat1.ge.scale_factor) then 
                 map%lat1 = map%lat1/scale_factor
              endif
              if (map%lon1.ge.scale_factor) then 
                 map%lon1 = map%lon1/scale_factor
              endif
              if ( debug_level .gt. 2 ) then
           print *,'Gaussian Grid: Dx,Dy,lat,lon',map%dx,map%dy,
     &       map%lat1,map%lon1
              end if

           elseif (gfld%igdtnum.eq.20) then ! Polar-Stereographic Grid.
              map%igrid = 5
              map%nx = gfld%igdtmpl(8)
              map%ny = gfld%igdtmpl(9)
              map%lov = gfld%igdtmpl(14) / scale_factor
              map%truelat1 = 60.
              map%truelat2 = 91.
              map%dx = gfld%igdtmpl(15) / scale_factor
              map%dy = gfld%igdtmpl(16) / scale_factor
              map%lat1 = gfld%igdtmpl(10) / scale_factor
              map%lon1 = gfld%igdtmpl(11) / scale_factor

           else
              print*, 'GRIB2 Unknown Projection: ',gfld%igdtnum
              print*, 'see Code Table 3.1: Grid Definition Template No'
           endif
         
         endif

         ! ----

         ! Continue to unpack GRIB2 field.
         do n=1,numfields ! e.g. U and V would =2, otherwise its usually =1
           call gf_getfld(cgrib,lengrib,n,unpack,expand,gfld,ierr)
           if (ierr.ne.0) then
             write(*,*) ' ERROR extracting field gf_getfld = ',ierr
             cycle
           endif

! ------------------------------------
         ! Additional print information for developer.
         if ( debug_level .GT. 100 ) then
           print *
           print *,'G2 FIELD ',n
           if (n==1) then
            print *,'G2 SECTION 0: ',gfld%discipline,gfld%version
            print *,'G2 SECTION 1: ',(gfld%idsect(j),j=1,gfld%idsectlen)
           endif
           if ( associated(gfld%local).AND.gfld%locallen.gt.0 ) then
              print *,'G2 SECTION 2: ',(gfld%local(j),j=1,gfld%locallen)
           endif
           print *,'G2 SECTION 3: ',gfld%griddef,gfld%ngrdpts,
     &                            gfld%numoct_opt,gfld%interp_opt,
     &                            gfld%igdtnum
           print *,'G2 GRID TEMPLATE 3.',gfld%igdtnum,': ',
     &            (gfld%igdtmpl(j),j=1,gfld%igdtlen)
           if ( gfld%num_opt .eq. 0 ) then
             print *,'G2 NO Section 3 List Defining No. of Data Points.'
           else
             print *,'G2 Section 3 Optional List: ',
     &                (gfld%list_opt(j),j=1,gfld%num_opt)
           endif
           print *,'G2 PRODUCT TEMPLATE 4.',gfld%ipdtnum,': ',
     &          (gfld%ipdtmpl(j),j=1,gfld%ipdtlen)

           pabbrev=param_get_abbrev(gfld%discipline,gfld%ipdtmpl(1),
     &                              gfld%ipdtmpl(2))
           !call prlevel(gfld%ipdtnum,gfld%ipdtmpl,labbrev)
           !call prvtime(gfld%ipdtnum,gfld%ipdtmpl,listsec1,tabbrev)
            print *,'G2 TEXT: ',pabbrev,trim(labbrev)," ",trim(tabbrev)

           if ( gfld%num_coord .eq. 0 ) then
             print *,'G2 NO Optional Vertical Coordinate List.'
           else
             print *,'G2 Section 4 Optional Coordinates: ',
     &             (gfld%coord_list(j),j=1,gfld%num_coord)
           endif
           if ( gfld%ibmap .ne. 255 ) then
              print *,'G2 Num. of Data Points = ',gfld%ndpts,
     &             '    with BIT-MAP ',gfld%ibmap
           else
              print *,'G2 Num. of Data Points = ',gfld%ndpts,
     &                '    NO BIT-MAP '
           endif
           print *,'G2 DRS TEMPLATE 5.',gfld%idrtnum,': ',
     &          (gfld%idrtmpl(j),j=1,gfld%idrtlen)
           fldmax=gfld%fld(1)
           fldmin=gfld%fld(1)
           sum=gfld%fld(1)
           do j=2,gfld%ndpts
             if (gfld%fld(j).gt.fldmax) fldmax=gfld%fld(j)
             if (gfld%fld(j).lt.fldmin) fldmin=gfld%fld(j)
             sum=sum+gfld%fld(j)
           enddo ! gfld%ndpts

           print *,'G2 Data Values:'
           write(*,fmt='("G2 MIN=",f21.8," AVE=",f21.8,
     &          " MAX=",f21.8)') fldmin,sum/gfld%ndpts,fldmax
           !do j=1,gfld%ndpts\20
           !   write(*,*) j, gfld%fld(j)
           !enddo
         endif ! Additional Print information 
! ------------------------------------

!         do i = 1, maxvar
!           write(6,'(a10,4i8)') namvar(i),(g2code(j,i),j=1,4)
!         enddo
	  if (debug_level .gt. 50) then
          write(6,*) 'looking for ',gfld%discipline,gfld%ipdtmpl(1),
     &       gfld%ipdtmpl(2),gfld%ipdtmpl(10)
          endif

         ! Test this data record again list of desired variables 
         ! found in Vtable.
         ! ----
         MATCH_LOOP: do i=1,maxvar ! Max variables found in Vtable,
                                   ! maxvar is defined in table.mod

           if ( gfld%discipline .eq. g2code(1,i) .and.   !Discipline 
     &          gfld%ipdtmpl(1) .eq. g2code(2,i) .and.   !Category
     &          gfld%ipdtmpl(2) .eq. g2code(3,i) .and.   !Parameter
     &          gfld%ipdtmpl(10) .eq. g2code(4,i)) then  !Elevation

            pabbrev=param_get_abbrev(gfld%discipline,gfld%ipdtmpl(1),
     &                               gfld%ipdtmpl(2))

              !my_field (e.g. RH, TMP, similar to, but not the same as pabbrev)
              my_field=namvar(i) 

	if (debug_level .gt. 50) then
	 write(6,*) 'namvar(i) = ',namvar(i),' pabbrev = ',pabbrev
	 write(6,*) 'Parameter = ',gfld%ipdtmpl(2)
	endif


! need to match up soil levels with those requested.
! For the Vtable levels, -88 = all levels, -99 = missing. The units
! vary depending on the level code (e.g. 106 = cm, 103 = m).
	      if ( gfld%ipdtmpl(10) .eq. 106 ) then
	        TMP8LOOP: do j = 1, maxvar
		  if ((g2code(4,j) .eq. 106) .and.
     &               (gfld%ipdtmpl(2) .eq. g2code(3,j)) .and.
     &               (gfld%ipdtmpl(12) .eq. level1(j)) .and.
     &               ((gfld%ipdtmpl(15) .eq. level2(j)) .or. 
     &                                   (level2(j) .le. -88))) then
		    my_field = namvar(j)
		    exit TMP8LOOP
		  endif
		enddo TMP8LOOP
		if (j .gt. maxvar ) then
		  write(6,'(a,i6,a,i6,a)') 'Subsoil level ',
     &               gfld%ipdtmpl(12),' to ',gfld%ipdtmpl(15),
     &           ' in the GRIB2 file, was not found in the Vtable'
		endif
         if (debug_level .gt. 50) write(6,*) 'my_field is now ',my_field
	      endif

              ! Level (eg. 10000 mb)
              if(gfld%ipdtmpl(10).eq.100) then
                 ! Pressure level (range from 1000mb to 0mb)
                 level=gfld%ipdtmpl(12)
              elseif(gfld%ipdtmpl(10).eq.105) then
                 ! Hybrid level (range from 1 to N)
                 level=gfld%ipdtmpl(12)
              elseif(gfld%ipdtmpl(10).eq.104) then
                 ! Sigma level (range from 10000 to 0)
                 level=gfld%ipdtmpl(12)
              elseif(gfld%ipdtmpl(10).eq.101) then
                 ! MSL
                 level=201300.
              elseif(gfld%ipdtmpl(10).eq.106.or.
     &               gfld%ipdtmpl(10).eq.1) then
                 ! Misc near ground/surface levels
                 level=200100.
              else
                 ! Misc near ground/surface levels
                 level=200100.
              endif
              iplvl = int(level)

              ! Store the unpacked 2D slab from the GRIB2 record
              allocate(hold_array(gfld%ngrdpts))
              do j=1,gfld%ngrdpts
                 hold_array(j)=gfld%fld(j)
              enddo

              ! When we have reached this point, we have a data array ARRAY 
              ! which has some data we want to save, with field name FIELD 
              ! at pressure level LEVEL (Pa).  Dimensions of this data are 
              ! map%nx and map%ny.  Put this data into storage.

              !print *,'call put_storage',iplvl,my_field,hold_array(55),ith
              !e.g. call put_storage(200100, 'RH', my_field, 1, ith)
              call put_storage(iplvl,my_field,
     &           reshape(hold_array(1:map%nx*map%ny),
     &           (/map%nx, map%ny/)), map%nx,map%ny)
              deallocate(hold_array)

              ! If Specific Humidity is present on hybrid levels AND 
              ! upper-air RH is missing, see if we can compute RH from 
              ! Specific Humidity.
              if (.not. is_there(iplvl, 'RH') .and.
     &            is_there(iplvl, 'SH') .and.
     &            is_there(iplvl, 'T') .and.
     &            is_there(iplvl, 'P')) then
                  call g2_compute_rh_spechumd_upa(map%nx,map%ny,iplvl)
                 !call llstor_remove(iplvl, 'SH') !We are done with SH
              endif

              ith=ith+1
              exit MATCH_LOOP

           endif ! Selected param.


         enddo MATCH_LOOP

         enddo ! 1,numfields


         ! Deallocate arrays decoding GRIB2 record.
         call gf_free(gfld)

      enddo VERSION ! skgb


      if ( debug_level .gt. 100 ) then
         print *, 'G2 total number of fields found = ',itot
         call summary()
      end if

      CALL BACLOSE(junit,IOS)

       ireaderr=1
      else 
       if (debug_level .gt. 50) print *,'open status failed because',ios
       hdate = '9999-99-99_99:99:99'
       ireaderr=2
      endif ! ireaderr check 

      END subroutine rd_grib2

!*****************************************************************************!
! Subroutine edition_num                                                      !
!                                                                             !
! Purpose:                                                                    !
!    Read one record from the input GRIB2 file.  Based on the information in  !
!    the GRIB2 header and the user-defined Vtable, decide whether the field in!
!    the GRIB2 record is one to process or to skip.  If the field is one we   !
!    want to keep, extract the data from the GRIB2 record, and pass the data  !
!    back to the calling routine.                                             !
!                                                                             !
! Argument list:                                                              !
!    Input:                                                                   !
!       JUNIT   : "Unit Number" to open and read from.  Not really a Fortran  !
!                 unit number, since we do not do Fortran I/O for the GRIB2   !
!                 files.  Nor is it a UNIX File Descriptor returned from a C  !
!                 OPEN statement.  It is really just an array index to the    !
!                 array (IUARR) where the UNIX File Descriptor values are     !
!                 stored.                                                     !
!       GRIB2FILE: File name to open, if it is not already open.              !
!                                                                             !
!    Output:                                                                  !
!       GRIB_EDITION: Set to 1 for GRIB and set to 2 for GRIB2                ! 
!       IERR     : Error flag: 0 - no error on read from GRIB2 file.          !
!                              1 - Hit the end of the GRIB2 file.             !
!                              2 - The file GRIBFLNM we tried to open does    !
!                                  not exist.                                 !
! Author: Paula McCaslin                                                      !
! NOAA/FSL                                                                    !
! Sept 2004                                                                   !
!*****************************************************************************!
      
      SUBROUTINE edition_num(junit, gribflnm, 
     &  grib_edition, ireaderr)

      use grib_mod
      use params

      parameter(msk1=32000,msk2=4000)
      character(len=1),allocatable,dimension(:) :: cgrib
      integer :: listsec0(3)
      integer :: listsec1(13)
      character(len=*)  :: gribflnm
      integer :: lskip, lgrib
      integer :: junit
      integer :: grib_edition
      integer :: i, j, ireaderr
      integer :: currlen

      character(len=4) :: ctemp
      character(len=4),parameter :: grib='GRIB',c7777='7777'

C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  SET ARGUMENTS

      call start()
      itot=0
      icount=0
      iseek=0
      lskip=0
      lgrib=0
      currlen=0

!/* IOS Return Codes from BACIO:  */
!/*  0    All was well                                   */
!/* -1    Tried to open read only _and_ write only       */
!/* -2    Tried to read and write in the same call       */
!/* -3    Internal failure in name processing            */
!/* -4    Failure in opening file                        */
!/* -5    Tried to read on a write-only file             */
!/* -6    Failed in read to find the 'start' location    */
!/* -7    Tried to write to a read only file             */
!/* -8    Failed in write to find the 'start' location   */
!/* -9    Error in close                                 */
!/* -10   Read or wrote fewer data than requested        */

!if ireaderr =1 we have hit the end of a file. 
!if ireaderr =2 we have hit the end of all the files. 
!if ireaderr =3 beginning characters 'GRIB' not found

      ! Open a byte-addressable file.
      CALL BAOPENR(junit,gribflnm,IOS)
      if (ios.eq.0) then 

         ! Search opend file for the next GRIB2 messege (record).
         call skgb(junit,iseek,msk1,lskip,lgrib)

         ! Check for EOF, or problem
         if (lgrib.eq.0) then
            STOP "Grib2 file or date problem, stopping in edition_num."
         endif
 
         ! Check size, if needed allocate more memory.
         if (lgrib.gt.currlen) then
            if (allocated(cgrib)) deallocate(cgrib)
            allocate(cgrib(lgrib),stat=is)
            currlen=lgrib
         endif

         ! Read a given number of bytes from unblocked file.
         call baread(junit,lskip,lgrib,lengrib,cgrib)

         ! Check for beginning of GRIB message in the first 100 bytes
         istart=0
         do j=1,100
            ctemp=cgrib(j)//cgrib(j+1)//cgrib(j+2)//cgrib(j+3)
            if (ctemp.eq.grib ) then
              istart=j
              exit
            endif
         enddo
         if (istart.eq.0) then
            ireaderr=3
            print*, "The beginning 4 characters >GRIB< were not found."
         endif
   
         ! Unpack Section 0 - Indicator Section to extract GRIB edition field
         iofst=8*(istart+5)
         call gbyte(cgrib,discipline,iofst,8)     ! Discipline
         iofst=iofst+8
         call gbyte(cgrib,grib_edition,iofst,8)   ! GRIB edition number

         print *, 'ungrib - grib edition num',  grib_edition
         call summary()
         CALL BACLOSE(junit,IOS)
         ireaderr=1
      else if (ios .eq. -4) then
        print *,'edition_num: unable to open ',gribflnm
	stop 'edition_num'
      else 
         print *,'edition_num: open status failed because',ios,gribflnm
         ireaderr=2
      endif ! ireaderr check 

      END subroutine edition_num

!*****************************************************************************!

      SUBROUTINE g2_compute_rh_spechumd_upa(ix, jx, iiplvl)
      ! Compute relative humidity from specific humidity in the upper air.
      use storage_module
      implicit none
      integer :: ix, jx
      integer :: iiplvl
      real :: lat1, lon1, dx, dy
      real, dimension(ix,jx) :: T, P, RH, Q
    
      real, parameter :: svp1=611.2
      real, parameter :: svp2=17.67
      real, parameter :: svp3=29.65
      real, parameter :: svpt0=273.15
      real, parameter :: eps = 0.622
    
      real startlat, startlon, deltalat, deltalon

      call get_storage(iiplvl, 'P', P, ix, jx)
      call get_storage(iiplvl, 'T', T, ix, jx)
      call get_storage(iiplvl, 'SH', Q, ix, jx)
    
      rh=1.E2*(p*q/(q*(1.-eps)+eps))/(svp1*exp(svp2*(t-svpt0)/(T-svp3)))
     
      call put_storage(iiplvl, 'RH', rh, ix, jx)
    
      end subroutine g2_compute_rh_spechumd_upa
