! Print information about a grib file.
!  Usage: "gribscan [-v] [-V] filename"
!  as of now, the filename cannot be a full path name
! 
program gribscan
  use module_grib
  interface
     subroutine parse_args(err, a1, h1, i1, l1, a2, h2, i2, l2,&
          a3, h3, i3, l3, hlast)
       integer :: err
       character(len=*) , optional :: a1, a2, a3
       character(len=*), optional :: h1, h2, h3
       integer , optional :: i1, i2, i3
       logical, optional :: l1, l2, l3
       character(len=*), optional :: hlast
     end subroutine parse_args
  end interface

  character(len=120) :: flnm
  character(len=30) :: hopt
  real, allocatable, dimension(:) :: datarray
  integer :: ierr, igherr
  integer :: cc
  character(len=100) :: fmt = '(I4,1X, &
       & I3,1x, A5,1x, &
       & I4, &
       & 2(1x,I4),2x,I4.4,2("-",I2.2),"_",I2.2,":",&
       & I2.2, 1x, "+", i2.2)'
  logical :: ivb = .FALSE.
  logical :: idb = .FALSE.
  integer :: year
  character(len=5) :: gc(255)
  data gc /'PRES','PRMSL','PTEND','PVORT','ICAHT','GP','HGT','DIST',&
       'HSTDV','TOZNE','TMP','VTMP','POT','EPOT','T MAX','T MIN','DPT',&
       'DEPR','LAPR','VIS','RDSP1','RDSP2','RDSP3','PLI','TMP A','PRESA',&
       'GP A','WVSP1','WVSP2','WVSP3','WDIR','WIND','U GRD','V GRD','STRM',&
       'V POT','MNTSF','SGCVV','V VEL','DZDT','ABS V','ABD D','REL V','REL D',&
       'VUCSH','VVCSG','DIR C','SP C','UOGRD','VOGRD','SPF H','R H','MIXR',&
       'P WAT','VAPP','SAT D','EVP','C ICE','PRATE','TSTM','A PCP','NCPCP',&
       'ACPCP','SRWEQ','WEASD','SNO D','MIXHT','TTHDP','MTHD','MTH A','T CDC',&
       'CDCON','L CDC','M CDC','H CDC','C WAT','BLI','SNO C','SNO L','WTMP',&
       'LAND','DSL M','SFC R','ALBDO','TSOIL','SOILM','VEG','SALTY','DEN',&
       'WATR','ICE C','ICETK','DICED','SICED','U ICE','V ICE','ICE G','ICE D',&
       'SNO M','HTSGW','WVDIR','WVHGT','WVPER','SWDIR','SWELL','SWPER','DIRPW',&
       'PERPW','DIRSW','PERSW','NSWRS','NLWRS','NSWRT','NLWRT','LWAVR','SWAVR',&
       'GRAD','BRTMP','LWRAD','SWRAD','LHTFL','SHTFL','BLYDP','U FLX','V FLX',&
       'WMIXE','IMG D',&
! 128-254 for use by originating center. NWS/NCEP usage is coded here.
       'MSLSA','MSLMA','MSLET','LFT X','4LFTX','K X','S X','MCONV','VW SH',&
       'TSLSA','BVF 2','PV MW','CRAIN','CFRZR','CICEP','CSNOW','SOILW',&
       'PEVPR','CWORK','U-GWD','V-GWD','PV','COVMZ','COVTZ','COVTM','CLWMR',&
       'O3MR','GFLUX','CIN','CAPE','TKE','CONDP','CSUSF','CSDSF','CSULF',&
       'CSDLF','CFNSF','CFNLF','VBDSF','VDDSF','NBDSF','NDDSF','RWMR',&
       'SNMR','M FLX','LMH','LMV','MLYNO','NLAT','ELON','ICMR','GRMR','GUST',&
       'LPS X','LPS Y','HGT X','HGT Y','TPFI','TIPD','LTNG','RDRIP','VPTMP','HLCY',&
       'PROB','PROBN','POP','CPOFP','CPOZP','USTM','VSTM','NCIP','EVBS','EVCW',&
       'ICWAT','CWDI','VAFTD','DSWRF','DLWRF','UVI','MSTAV','SFEXC','MIXLY','TRANS',&
       'USWRF','ULWRF','CDLYR','CPRAT','TTDIA','TTRAD','TTPHY','PREIX','TSD1D',&
       'NLGSP','HPBL','5WAVH','CNWAT','SOTYP','VGTYP','BMIXL','AMIXL','PEVAP',&
       'SNOHF','5WAVA','MFLUX','DTRF','UTRF','BGRUN','SSRUN','SIPD','O3TOT',&
       'SNOWC','SNOT','COVTW','LRGHR','CNVHR','CNVMR','SHAHR','SHAMR','VDFHR',&
       'VDFUA','VDFVA','VDFMR','SWHR','LWHR','CD','FRICV','RI','  '/

  flnm = ' '
  call parse_args(ierr, a1='v', l1=ivb, a2='V', l2=idb, hlast=flnm)
  if (ierr.ne.0) then
     call getarg(0, hopt)
     write(*,'(//,"Usage: ", A, " [-v] [-V] file",/)') trim(hopt)
     write(*,'("     -v   : Print more information about the GRIB records")')
     write(*,'("     -V   : Print way too much information about the GRIB&
          & records")')
     write(*,'("     file : GRIB file to read"//)')
      stop
!    stop
  endif

  if (idb) ivb = .TRUE.

  call c_open(idum, munit, flnm, 1, ierr, 1)

  if (.not. ivb) then
     write(*,'(52("-"))')
     write(*,'(" rec GRIB GRIB  Lvl  Lvl  Lvl         Time      Fcst")')
     write(*,'(" Num Code name  Code one  two                   hour")')
     write(*,'(52("-"))')
  endif 

  irec = 0
  call gribget(munit, ierr)
  do while (ierr.eq.0) 
     irec = irec + 1
     call gribheader(0,igherr)
     if (igherr /= 0) then
        call deallogrib
        call gribget(munit, ierr)
        cycle
     endif

     if ( sec1(3) .ne. 7 ) then  ! gc defined only for NCEP
       do cc = 128, 254
         gc(cc) = '     '
       enddo
     if ( sec1(3) .eq. 57 ) then  ! AFWA
       gc(144) = 'DNWLR'
       gc(145) = 'INSWR'
       gc(155) = 'GDHFX'
       gc(157) = 'XTRAJ'
       gc(158) = 'YTRAJ'
       gc(159) = 'PTRAJ'
       gc(160) = 'TERID'
       gc(161) = 'MDLTN'
       gc(174) = 'SNOWD'
       gc(175) = 'SNOAG'
       gc(176) = 'SNOCL'
       gc(177) = 'VSBLY'
       gc(178) = 'CURWX'
       gc(179) = 'CLAMT'
       gc(180) = 'CLBAS'
       gc(181) = 'CLTOP'
       gc(182) = 'CLTYP'
       gc(183) = 'UTIME'
       gc(184) = 'SRCDT'
       gc(196) = 'EPCDF'
       gc(197) = 'EPALL'
       gc(198) = 'EPGEO'
       gc(199) = 'EPVAL'
       gc(200) = 'SOILR'
       gc(201) = 'SOILW'
       gc(205) = 'TYPSL'
       gc(206) = 'VLASH'
       gc(207) = 'CANWT'
       gc(208) = 'PEVAP'
       gc(209) = 'WNDRN'
       gc(210) = 'RHTMN'
       gc(211) = 'SOILL'
       gc(212) = 'VEGTP'
       gc(213) = 'GREEN'
       gc(234) = 'BGRUN'
       gc(235) = 'SSRUN'
     endif
     endif

     if (ivb) then
        call gribprint(0)
        call gribprint(1)
        call gribprint(2)
        call gribprint(3)
        call gribprint(4)
           if (sec2(4).eq.50) then
              ndat = (infogrid(1)+1)*(infogrid(2)+1)
           else
              ndat = (infogrid(1)*infogrid(2))
           endif
           allocate(datarray(ndat))
           call gribdata(datarray, ndat)
	   fldmax = datarray(1)
	   fldmin = datarray(1)
	   do j = 1, ndat
	     if (datarray(j).gt.fldmax) fldmax=datarray(j)
	     if (datarray(j).lt.fldmin) fldmin=datarray(j)
	   enddo
	write(*,*) "  "
	write(*,*) "  ",gc(sec1(7))," : "
        write(*,'(5x,"Minimum Data Value ",t45,":",g14.5)') fldmin
        write(*,'(5x,"Maximum Data Value ",t45,":",g14.5)') fldmax
        write(*,'(//,70("*"))')
        if (idb) then
           print*, 'Datarray = ', Datarray
        endif
           deallocate(datarray)
     else
        CC = sec1(22)
        year = (cc-1)*100 + sec1(11)
        write(*,FMT) irec, sec1(7), gc(sec1(7)), sec1(8:10), year,sec1(12:15),sec1(17)
     endif

     call deallogrib

     call gribget(munit, ierr)
  enddo
  if (ierr.eq.1) write(*,'(/,"***** End-Of-File on C unit ", I3,/)') munit
  call c_close( munit, 0, ierr)

end program gribscan

subroutine parse_args(err, a1, h1, i1, l1, a2, h2, i2, l2, a3, h3, i3, l3, &
     hlast)
  integer :: err
  character(len=*) , optional :: a1, a2, a3
  character(len=*), optional :: h1, h2, h3
  integer , optional :: i1, i2, i3
  logical, optional :: l1, l2, l3
  character(len=*), optional :: hlast

  character(len=100) :: hold
  integer :: ioff = 0

  if (present(hlast)) then
     ioff = -1
  endif

  err = 0

  narg = iargc()
  numarg = narg + ioff

  i = 1
  LOOP : do while ( i <= numarg)

     ierr = 1
     if (present(i1)) then
        call checkiarg(i, a1, i1, ierr)
     elseif (present(h1)) then
        call checkharg(i, a1, h1, ierr)
     elseif (present(l1)) then
        call checklarg(i, a1, l1, ierr)
     endif
     if (ierr.eq.0) cycle LOOP

     if (present(i2)) then
        call checkiarg(i, a2, i2, ierr)
     elseif (present(h2)) then
        call checkharg(i, a2, h2, ierr)
     elseif (present(l2)) then
        call checklarg(i, a2, l2, ierr)
     endif
     if (ierr.eq.0) cycle LOOP

     if (present(i3)) then
        call checkiarg(i, a3, i3, ierr)
     elseif (present(h3)) then
        call checkharg(i, a3, h3, ierr)
     elseif (present(l3)) then
        call checklarg(i, a3, l3, ierr)
     endif
     if (ierr.eq.0) cycle LOOP

     err = 1
     call getarg(1, hold)
     write(*, '("arg = ", A)') trim(hold)

     exit LOOP

  enddo LOOP

  if (present(hlast)) then
     if (narg.eq.0) then
        err = 1
     else
        call getarg(narg, hlast)
     endif
  endif

contains
  subroutine checkiarg(c, a, i, ierr)
    integer :: c
    character(len=*) :: a
    integer :: i

    character(len=100) :: hold
    ierr = 1

    call getarg(c, hold)

    if ('-'//a.eq.trim(hold)) then
       c = c + 1
       call getarg(c, hold)
       read(hold, *) i
       c = c + 1
       ierr = 0
    elseif ('-'//a .eq. hold(1:len_trim(a)+1)) then
       hold = hold(len_trim(a)+2: len(hold))
       read(hold, *) i
       c = c + 1
       ierr = 0
    endif
        
  end subroutine checkiarg
  subroutine checkharg(c, a, h, ierr)
    integer :: c
    character(len=*) :: a
    character(len=*) :: h

    character(len=100) :: hold
    ierr = 1

    call getarg(c, hold)

    if ('-'//a.eq.trim(hold)) then
       c = c + 1
       call getarg(c, hold)
       h = trim(hold)
       c = c + 1
       ierr = 0
    elseif ('-'//a .eq. hold(1:len_trim(a)+1)) then
       hold = hold(len_trim(a)+2: len(hold))
       h = trim(hold)
       c = c + 1
       ierr = 0
    endif
        
  end subroutine checkharg

  subroutine checklarg(c, a, l, ierr)
    integer :: c
    character(len=*) :: a
    logical :: l

    character(len=100) :: hold
    ierr = 1

    call getarg(c, hold)
    if ('-'//a.eq.trim(hold)) then
       l = .TRUE.
       c = c + 1
       ierr = 0
    endif
        
  end subroutine checklarg

end subroutine parse_args


