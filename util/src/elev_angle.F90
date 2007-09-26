program elev_angle

   use write_met_module
   use netcdf

   implicit none

   integer, external :: iargc

   integer :: istatus
   integer :: i, j, n_bins
   integer :: ncid, topo_varid, we_dimid, we_dim, sn_dimid, sn_dim
   real, allocatable, dimension(:,:) :: hgt
   real, allocatable, dimension(:,:,:) :: topo_angle
   character (len=NF90_MAX_NAME) :: toponame, we_name, sn_name
   character (len=1024) :: filename
   type (met_data) :: met_angle

   if (iargc() /= 1) then
      write(6,*) ' '
      write(6,*) 'Usage: elev_angle.exe <geogrid output file>'
      write(6,*) ' '
      stop
   end if

   call getarg(1,filename)

   istatus = nf90_open(trim(filename), 0, ncid) 
   if (istatus /= NF90_NOERR) then
      write(6,*) ' '
      write(6,*) 'Error: Could not open file '//trim(filename)
      write(6,*) ' '
      stop
   end if

   sn_name = 'south_north'
   istatus = nf90_inq_dimid(ncid, sn_name, sn_dimid)
   if (istatus /= NF90_NOERR) then
      write(6,*) ' '
      write(6,*) 'Error: Could not get ID of dimension south_north'
      write(6,*) ' '
      stop
   end if

   istatus = nf90_inquire_dimension(ncid, sn_dimid, sn_name, sn_dim)
   if (istatus /= NF90_NOERR) then
      write(6,*) ' '
      write(6,*) 'Error: Could not get south_north dimension'
      write(6,*) ' '
      stop
   end if

   we_name = 'west_east'
   istatus = nf90_inq_dimid(ncid, we_name, we_dimid)
   if (istatus /= NF90_NOERR) then
      write(6,*) ' '
      write(6,*) 'Error: Could not get ID of dimension west_east'
      write(6,*) ' '
      stop
   end if

   istatus = nf90_inquire_dimension(ncid, we_dimid, we_name, we_dim)
   if (istatus /= NF90_NOERR) then
      write(6,*) ' '
      write(6,*) 'Error: Could not get west_east dimension'
      write(6,*) ' '
      stop
   end if

   toponame = 'HGT_M'
   istatus = nf90_inq_varid(ncid, toponame, topo_varid)
   if (istatus /= NF90_NOERR) then
      write(6,*) ' '
      write(6,*) 'Error: Could not get ID of variable HGT_M'
      write(6,*) ' '
      stop
   end if

   allocate(hgt(we_dim,sn_dim))

   istatus = nf90_get_var(ncid, topo_varid, hgt)
   if (istatus /= NF90_NOERR) then
      write(6,*) ' '
      write(6,*) 'Error: Could not read HGT_M field'
      write(6,*) ' '
      deallocate(hgt)
      stop
   end if

   write(6,*) 'Read HGT_M field dimensioned ',we_dim,sn_dim
   do j=1,sn_dim,10
   do i=1,we_dim,10
      write(6,'(a6,i3,a1,i3,a2,f13.5)') 'HGT_M(',i,',',j,')=',hgt(i,j)
   end do
   end do

   istatus = nf90_close(ncid)

   call write_met_init('ELEVANGLES', .true., '0000-00-00_00:00:00', istatus)

   do i=1,n_bins

!      call write_next_met_field(met_angle, istatus)

   end do

   call write_met_close()

   deallocate(hgt)

   stop

end program elev_angle
