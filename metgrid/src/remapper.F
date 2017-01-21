module remapper

    use mpas_mesh
    use scan_input, only : input_field_type, FIELD_TYPE_REAL, FIELD_TYPE_DOUBLE, FIELD_TYPE_INTEGER
    use target_mesh


    type remap_info_type
        integer :: method = -1
        type (mpas_mesh_type), pointer :: src_mesh
        type (target_mesh_type), pointer :: dst_mesh

        ! For nearest-neighbor
        integer, dimension(:,:), pointer :: nearestCell => null()
        integer, dimension(:,:), pointer :: nearestVertex => null()
        integer, dimension(:,:), pointer :: nearestEdge => null()

        ! For Wachspress interpolation
        real, dimension(:,:,:), pointer :: cellWeights => null()
        real, dimension(:,:,:), pointer :: vertexWeights => null()
        real, dimension(:,:,:), pointer :: edgeWeights => null()
        integer, dimension(:,:,:), pointer :: sourceCells => null()
        integer, dimension(:,:,:), pointer :: sourceVertices => null()
        integer, dimension(:,:,:), pointer :: sourceEdges => null()
    end type remap_info_type


    type target_field_type
        character (len=64) :: name
        integer :: ndims = -1
        integer :: xtype = -1
        logical :: isTimeDependent = .false.
        integer, dimension(:), pointer :: dimlens => null()
        character (len=64), dimension(:), pointer :: dimnames => null()

        !  Members to store field data
        real :: array0r
        real, dimension(:), pointer :: array1r => null()
        real, dimension(:,:), pointer :: array2r => null()
        real, dimension(:,:,:), pointer :: array3r => null()
        real, dimension(:,:,:,:), pointer :: array4r => null()
        double precision :: array0d
        double precision, dimension(:), pointer :: array1d => null()
        double precision, dimension(:,:), pointer :: array2d => null()
        double precision, dimension(:,:,:), pointer :: array3d => null()
        double precision, dimension(:,:,:,:), pointer :: array4d => null()
        integer :: array0i
        integer, dimension(:), pointer :: array1i => null()
        integer, dimension(:,:), pointer :: array2i => null()
        integer, dimension(:,:,:), pointer :: array3i => null()
        integer, dimension(:,:,:,:), pointer :: array4i => null()
    end type target_field_type


    private :: nearest_cell, &
               nearest_vertex, &
               sphere_distance, &
               mpas_arc_length, &
               mpas_triangle_signed_area_sphere, &
               mpas_wachspress_coordinates, &
               convert_lx, &
               index2d


    contains


    integer function remap_info_setup(src_mesh, dst_mesh, remap_info) result(stat)

        implicit none

        type (mpas_mesh_type), intent(in), target :: src_mesh
        type (target_mesh_type), intent(in), target :: dst_mesh
        type (remap_info_type), intent(out) :: remap_info

        integer :: idx
        integer :: j
        integer :: nn
        integer :: ix, iy
        integer :: irank
        real, dimension(:,:), allocatable :: vertCoords
        real, dimension(3) :: pointInterp

        stat = 0

        remap_info % method = 1
        remap_info % src_mesh => src_mesh
        remap_info % dst_mesh => dst_mesh

        !
        ! For nearest neighbor
        !
        allocate(remap_info % nearestCell(dst_mesh % nlon, dst_mesh % nlat))
        allocate(remap_info % nearestVertex(dst_mesh % nlon, dst_mesh % nlat))
        allocate(remap_info % nearestEdge(dst_mesh % nlon, dst_mesh % nlat))

        irank = dst_mesh % irank

        idx = 1
        do iy=1,dst_mesh % nlat
        do ix=1,dst_mesh % nlon
            idx = nearest_cell(dst_mesh % lats(index2d(irank,ix),iy), dst_mesh % lons(ix,index2d(irank,iy)), idx, &
                               src_mesh % nCells, src_mesh % maxEdges, &
                               src_mesh % nEdgesOnCell, src_mesh % cellsOnCell, src_mesh % latCell, src_mesh % lonCell)
            remap_info % nearestCell(ix, iy) = idx
        end do
        end do

        idx = 1
        do iy=1,dst_mesh % nlat
        do ix=1,dst_mesh % nlon
            idx = nearest_vertex(dst_mesh % lats(index2d(irank,ix),iy), dst_mesh % lons(ix,index2d(irank,iy)), idx, &
                                 src_mesh % nCells, src_mesh % nVertices, src_mesh % maxEdges, 3, &
                                 src_mesh % nEdgesOnCell, src_mesh % verticesOnCell, &
                                 src_mesh % cellsOnVertex, src_mesh % latCell, src_mesh % lonCell, &
                                 src_mesh % latVertex, src_mesh % lonVertex )
            remap_info % nearestVertex(ix, iy) = idx
        end do
        end do

        idx = 1
        do iy=1,dst_mesh % nlat
        do ix=1,dst_mesh % nlon
            idx = nearest_vertex(dst_mesh % lats(index2d(irank,ix),iy), dst_mesh % lons(ix,index2d(irank,iy)), idx, &
                                 src_mesh % nCells, src_mesh % nEdges, src_mesh % maxEdges, 2, &
                                 src_mesh % nEdgesOnCell, src_mesh % edgesOnCell, &
                                 src_mesh % cellsOnEdge, src_mesh % latCell, src_mesh % lonCell, &
                                 src_mesh % latEdge, src_mesh % lonEdge )
            remap_info % nearestEdge(ix, iy) = idx
        end do
        end do


        !
        ! For Wachspress interpolation
        !
        allocate(vertCoords(3,3))

        allocate(remap_info % cellWeights(3, dst_mesh % nlon, dst_mesh % nlat))
        allocate(remap_info % sourceCells(3, dst_mesh % nlon, dst_mesh % nlat))

        idx = 1
        do iy=1,dst_mesh % nlat
        do ix=1,dst_mesh % nlon
            idx = nearest_vertex(dst_mesh % lats(index2d(irank,ix),iy), dst_mesh % lons(ix,index2d(irank,iy)), idx, &
                                 src_mesh % nCells, src_mesh % nVertices, src_mesh % maxEdges, 3, &
                                 src_mesh % nEdgesOnCell, src_mesh % verticesOnCell, &
                                 src_mesh % cellsOnVertex, src_mesh % latCell, src_mesh % lonCell, &
                                 src_mesh % latVertex, src_mesh % lonVertex )
            remap_info % sourceCells(:,ix,iy) = src_mesh % cellsOnVertex(:,idx)
            pointInterp(:) = convert_lx(dst_mesh % lats(index2d(irank,ix),iy), dst_mesh % lons(ix,index2d(irank,iy)), 6371229.0)
            do j=1,3
                vertCoords(:,j) = convert_lx(src_mesh % latCell(src_mesh % cellsOnVertex(j,idx)), &
                                             src_mesh % lonCell(src_mesh % cellsOnVertex(j,idx)), &
                                             6371229.0)
            end do
            remap_info % cellWeights(:,ix,iy) = mpas_wachspress_coordinates(3, vertCoords, pointInterp)
        end do
        end do

        deallocate(vertCoords)


        allocate(vertCoords(3,src_mesh % maxEdges))

        allocate(remap_info % vertexWeights(src_mesh % maxEdges, dst_mesh % nlon, dst_mesh % nlat))
        allocate(remap_info % sourceVertices(src_mesh % maxEdges, dst_mesh % nlon, dst_mesh % nlat))

        idx = 1
        do iy=1,dst_mesh % nlat
        do ix=1,dst_mesh % nlon
            idx = nearest_cell(dst_mesh % lats(index2d(irank,ix),iy), dst_mesh % lons(ix,index2d(irank,iy)), idx, &
                               src_mesh % nCells, src_mesh % maxEdges, &
                               src_mesh % nEdgesOnCell, src_mesh % cellsOnCell, src_mesh % latCell, src_mesh % lonCell)
            nn = src_mesh % nEdgesOnCell(idx)
            remap_info % sourceVertices(:,ix,iy) = 1
            remap_info % sourceVertices(1:nn,ix,iy) = src_mesh % verticesOnCell(1:nn,idx)
            pointInterp(:) = convert_lx(dst_mesh % lats(index2d(irank,ix),iy), dst_mesh % lons(ix,index2d(irank,iy)), 6371229.0)
            do j=1,nn
                vertCoords(:,j) = convert_lx(src_mesh % latVertex(src_mesh % verticesOnCell(j,idx)), &
                                             src_mesh % lonVertex(src_mesh % verticesOnCell(j,idx)), &
                                             6371229.0)
            end do
            remap_info % vertexWeights(:,ix,iy) = 0.0
            remap_info % vertexWeights(1:nn,ix,iy) = mpas_wachspress_coordinates(3, vertCoords(:,1:nn), pointInterp)
        end do
        end do

        deallocate(vertCoords)


        allocate(vertCoords(3,src_mesh % maxEdges))

        allocate(remap_info % edgeWeights(src_mesh % maxEdges, dst_mesh % nlon, dst_mesh % nlat))
        allocate(remap_info % sourceEdges(src_mesh % maxEdges, dst_mesh % nlon, dst_mesh % nlat))

        idx = 1
        do iy=1,dst_mesh % nlat
        do ix=1,dst_mesh % nlon
            idx = nearest_cell(dst_mesh % lats(index2d(irank,ix),iy), dst_mesh % lons(ix,index2d(irank,iy)), idx, &
                               src_mesh % nCells, src_mesh % maxEdges, &
                               src_mesh % nEdgesOnCell, src_mesh % cellsOnCell, src_mesh % latCell, src_mesh % lonCell)
            nn = src_mesh % nEdgesOnCell(idx)
            remap_info % sourceEdges(:,ix,iy) = 1
            remap_info % sourceEdges(1:nn,ix,iy) = src_mesh % edgesOnCell(1:nn,idx)
            pointInterp(:) = convert_lx(dst_mesh % lats(index2d(irank,ix),iy), dst_mesh % lons(ix,index2d(irank,iy)), 6371229.0)
            do j=1,nn
                vertCoords(:,j) = convert_lx(src_mesh % latEdge(src_mesh % edgesOnCell(j,idx)), &
                                             src_mesh % lonEdge(src_mesh % edgesOnCell(j,idx)), &
                                             6371229.0)
            end do
            remap_info % edgeWeights(:,ix,iy) = 0.0
            remap_info % edgeWeights(1:nn,ix,iy) = mpas_wachspress_coordinates(3, vertCoords(:,1:nn), pointInterp)
        end do
        end do

        deallocate(vertCoords)

    end function remap_info_setup


    integer function remap_info_free(remap_info) result(stat)

        implicit none

        type (remap_info_type), intent(inout) :: remap_info


        stat = 0

        remap_info % method = -1
        nullify(remap_info % src_mesh)
        nullify(remap_info % dst_mesh)

        if (associated(remap_info % nearestCell)) then
            deallocate(remap_info % nearestCell)
        end if
        if (associated(remap_info % nearestVertex)) then
            deallocate(remap_info % nearestVertex)
        end if
        if (associated(remap_info % nearestEdge)) then
            deallocate(remap_info % nearestEdge)
        end if
        if (associated(remap_info % cellWeights)) then
            deallocate(remap_info % cellWeights)
        end if
        if (associated(remap_info % vertexWeights)) then
            deallocate(remap_info % vertexWeights)
        end if
        if (associated(remap_info % edgeWeights)) then
            deallocate(remap_info % edgeWeights)
        end if
        if (associated(remap_info % sourceCells)) then
            deallocate(remap_info % sourceCells)
        end if
        if (associated(remap_info % sourceVertices)) then
            deallocate(remap_info % sourceVertices)
        end if
        if (associated(remap_info % sourceEdges)) then
            deallocate(remap_info % sourceEdges)
        end if

    end function remap_info_free


    logical function can_remap_field(field)

        implicit none

        type (input_field_type), intent(in) :: field

        integer :: decompDim


        can_remap_field = .true.

        if (field % xtype /= FIELD_TYPE_INTEGER .and. &
            field % xtype /= FIELD_TYPE_REAL .and. &
            field % xtype /= FIELD_TYPE_DOUBLE) then
            can_remap_field = .false.
            return
        end if

        if (field % ndims == 0 .or. &
            (field % ndims == 1 .and. field % isTimeDependent)) then
            can_remap_field = .false.
            return
        end if

        decompDim = field % ndims
        if (field % isTimeDependent) then
            decompDim = decompDim - 1
        end if

        if (trim(field % dimnames(decompDim)) /= 'nCells' .and. &
            trim(field % dimnames(decompDim)) /= 'nVertices' .and. &
            trim(field % dimnames(decompDim)) /= 'nEdges') then

            can_remap_field = .false.
            return
        end if

    end function can_remap_field


    integer function remap_field_dryrun(remap_info, src_field, dst_field) result(stat)

        implicit none

        type (remap_info_type), intent(in) :: remap_info
        type (input_field_type), intent(in) :: src_field
        type (target_field_type), intent(out) :: dst_field

        integer :: idim

        stat = 0

        dst_field % name = src_field % name
        dst_field % xtype = src_field % xtype
        if (src_field % isTimeDependent) then
            ! Single horizontal dimension becomes two horizontal dimensions, nlat and nlon,
            ! but the time dimension is not counted in the target field
            dst_field % ndims = src_field % ndims
        else
            ! Single horizontal dimension becomes two horizontal dimensions, nlat and nlon
            dst_field % ndims = src_field % ndims + 1
        end if
        allocate(dst_field % dimnames(dst_field % ndims))
        allocate(dst_field % dimlens(dst_field % ndims))
        dst_field % isTimeDependent = src_field % isTimeDependent
        do idim=1,dst_field % ndims-2
            dst_field % dimlens(idim) = src_field % dimlens(idim)
            dst_field % dimnames(idim) = src_field % dimnames(idim)
        end do

        dst_field % dimlens(dst_field % ndims-1) = remap_info % dst_mesh % nlon
        dst_field % dimnames(dst_field % ndims-1) = 'nLons'
        dst_field % dimlens(dst_field % ndims) = remap_info % dst_mesh % nlat
        dst_field % dimnames(dst_field % ndims) = 'nLats'

    end function remap_field_dryrun


    integer function remap_field(remap_info, src_field, dst_field) result(stat)

        implicit none

        type (remap_info_type), intent(in) :: remap_info
        type (input_field_type), intent(in) :: src_field
        type (target_field_type), intent(out) :: dst_field

        integer :: decompDim
        integer :: idim
        integer :: j
        integer :: iy, ix
        integer, dimension(:,:), pointer :: nearestIndex
        integer, dimension(:,:,:), pointer :: sourceNodes
        real, dimension(:,:,:), pointer :: nodeWeights

        stat = 0

        decompDim = src_field % ndims
        if (src_field % isTimeDependent) then
            decompDim = decompDim - 1
        end if

        if (trim(src_field % dimnames(decompDim)) == 'nCells') then
            nearestIndex => remap_info % nearestCell
            sourceNodes => remap_info % sourceCells
            nodeWeights => remap_info % cellWeights
        else if (trim(src_field % dimnames(decompDim)) == 'nVertices') then
            nearestIndex => remap_info % nearestVertex
            sourceNodes => remap_info % sourceVertices
            nodeWeights => remap_info % vertexWeights
        else if (trim(src_field % dimnames(decompDim)) == 'nEdges') then
            nearestIndex => remap_info % nearestEdge
            sourceNodes => remap_info % sourceEdges
            nodeWeights => remap_info % edgeWeights
        else
            write(0,*) 'Remap exception: unhandled decomposed dim'
            stat = 1
            return
        end if

        dst_field % name = src_field % name
        dst_field % xtype = src_field % xtype
        if (src_field % isTimeDependent) then
            ! Single horizontal dimension becomes two horizontal dimensions, nlat and nlon,
            ! but the time dimension is not counted in the target field
            dst_field % ndims = src_field % ndims
        else
            ! Single horizontal dimension becomes two horizontal dimensions, nlat and nlon
            dst_field % ndims = src_field % ndims + 1
        end if
        allocate(dst_field % dimnames(dst_field % ndims))
        allocate(dst_field % dimlens(dst_field % ndims))
        dst_field % isTimeDependent = src_field % isTimeDependent
        do idim=1,dst_field % ndims-2
            dst_field % dimlens(idim) = src_field % dimlens(idim)
            dst_field % dimnames(idim) = src_field % dimnames(idim)
        end do
        dst_field % dimlens(dst_field % ndims-1) = remap_info % dst_mesh % nlon
        dst_field % dimnames(dst_field % ndims-1) = 'nLons'
        dst_field % dimlens(dst_field % ndims) = remap_info % dst_mesh % nlat
        dst_field % dimnames(dst_field % ndims) = 'nLats'

        if (src_field % xtype == FIELD_TYPE_REAL) then
            if (dst_field % ndims == 2) then
                allocate(dst_field % array2r(dst_field % dimlens(1), &
                                             dst_field % dimlens(2)))
#if NEAREST_NEIGHBOR
                do iy=1,size(nearestIndex, dim=2)
                do ix=1,size(nearestIndex, dim=1)
                    dst_field % array2r(ix,iy) = src_field % array1r(nearestIndex(ix,iy))
                end do
                end do
#endif
                do iy=1,size(nearestIndex, dim=2)
                do ix=1,size(nearestIndex, dim=1)
                    dst_field % array2r(ix,iy) = 0.0
                    do j=1,size(sourceNodes, dim=1)
                        dst_field % array2r(ix,iy) = dst_field % array2r(ix,iy) + &
                                                      nodeWeights(j,ix,iy) &
                                                      * src_field % array1r(sourceNodes(j,ix,iy))
                    end do
                end do
                end do
            else if (dst_field % ndims == 3) then
                allocate(dst_field % array3r(dst_field % dimlens(1), &
                                             dst_field % dimlens(2), &
                                             dst_field % dimlens(3)))
#if NEAREST_NEIGHBOR
                do iy=1,size(nearestIndex, dim=2)
                do ix=1,size(nearestIndex, dim=1)
                    dst_field % array3r(:,ix,iy) = src_field % array2r(:,nearestIndex(ix,iy))
                end do
                end do
#endif
                do iy=1,size(nearestIndex, dim=2)
                do ix=1,size(nearestIndex, dim=1)
                    dst_field % array3r(:,ix,iy) = 0.0
                    do j=1,3
                        dst_field % array3r(:,ix,iy) = dst_field % array3r(:,ix,iy) + &
                                                        remap_info % cellWeights(j,ix,iy) &
                                                        * src_field % array2r(:,remap_info % sourceCells(j,ix,iy))
                    end do
                end do
                end do
            else if (dst_field % ndims == 4) then
                allocate(dst_field % array4r(dst_field % dimlens(1), &
                                             dst_field % dimlens(2), &
                                             dst_field % dimlens(3), &
                                             dst_field % dimlens(4)))
#if NEAREST_NEIGHBOR
                do iy=1,size(nearestIndex, dim=2)
                do ix=1,size(nearestIndex, dim=1)
                    dst_field % array4r(:,:,ix,iy) = src_field % array3r(:,:,nearestIndex(ix,iy))
                end do
                end do
#endif
                do iy=1,size(nearestIndex, dim=2)
                do ix=1,size(nearestIndex, dim=1)
                    dst_field % array4r(:,:,ix,iy) = 0.0
                    do j=1,3
                        dst_field % array4r(:,:,ix,iy) = dst_field % array4r(:,:,ix,iy) + &
                                                          remap_info % cellWeights(j,ix,iy) &
                                                          * src_field % array3r(:,:,remap_info % sourceCells(j,ix,iy))
                    end do
                end do
                end do
            else
                write(0,*) 'Remap exception: unhandled dimension for real ', dst_field % ndims
            end if
        else if (src_field % xtype == FIELD_TYPE_DOUBLE) then
            if (dst_field % ndims == 2) then
                allocate(dst_field % array2d(dst_field % dimlens(1), &
                                             dst_field % dimlens(2)))
#if NEAREST_NEIGHBOR
                do iy=1,size(nearestIndex, dim=2)
                do ix=1,size(nearestIndex, dim=1)
                    dst_field % array2d(ix,iy) = src_field % array1d(nearestIndex(ix,iy))
                end do
                end do
#endif
                do iy=1,size(nearestIndex, dim=2)
                do ix=1,size(nearestIndex, dim=1)
                    dst_field % array2d(ix,iy) = 0.0
                    do j=1,size(sourceNodes, dim=1)
                        dst_field % array2d(ix,iy) = dst_field % array2d(ix,iy) + &
                                                      nodeWeights(j,ix,iy) &
                                                      * src_field % array1d(sourceNodes(j,ix,iy))
                    end do
                end do
                end do
            else if (dst_field % ndims == 3) then
                allocate(dst_field % array3d(dst_field % dimlens(1), &
                                             dst_field % dimlens(2), &
                                             dst_field % dimlens(3)))
#if NEAREST_NEIGHBOR
                do iy=1,size(nearestIndex, dim=2)
                do ix=1,size(nearestIndex, dim=1)
                    dst_field % array3d(:,ix,iy) = src_field % array2d(:,nearestIndex(ix,iy))
                end do
                end do
#endif
                do iy=1,size(nearestIndex, dim=2)
                do ix=1,size(nearestIndex, dim=1)
                    dst_field % array3d(:,ix,iy) = 0.0
                    do j=1,3
                        dst_field % array3d(:,ix,iy) = dst_field % array3d(:,ix,iy) + &
                                                        remap_info % cellWeights(j,ix,iy) &
                                                        * src_field % array2d(:,remap_info % sourceCells(j,ix,iy))
                    end do
                end do
                end do
            else if (dst_field % ndims == 4) then
                allocate(dst_field % array4d(dst_field % dimlens(1), &
                                             dst_field % dimlens(2), &
                                             dst_field % dimlens(3), &
                                             dst_field % dimlens(4)))
#if NEAREST_NEIGHBOR
                do iy=1,size(nearestIndex, dim=2)
                do ix=1,size(nearestIndex, dim=1)
                    dst_field % array4d(:,:,ix,iy) = src_field % array3d(:,:,nearestIndex(ix,iy))
                end do
                end do
#endif
                do iy=1,size(nearestIndex, dim=2)
                do ix=1,size(nearestIndex, dim=1)
                    dst_field % array4d(:,:,ix,iy) = 0.0
                    do j=1,3
                        dst_field % array4d(:,:,ix,iy) = dst_field % array4d(:,:,ix,iy) + &
                                                          remap_info % cellWeights(j,ix,iy) &
                                                          * src_field % array3d(:,:,remap_info % sourceCells(j,ix,iy))
                    end do
                end do
                end do
            else
                write(0,*) 'Remap exception: unhandled dimension for dbl ', dst_field % ndims
            end if
        else if (src_field % xtype == FIELD_TYPE_INTEGER) then
            if (dst_field % ndims == 2) then
                allocate(dst_field % array2i(dst_field % dimlens(1), &
                                             dst_field % dimlens(2)))
                do iy=1,size(nearestIndex, dim=2)
                do ix=1,size(nearestIndex, dim=1)
                    dst_field % array2i(ix,iy) = src_field % array1i(nearestIndex(ix,iy))
                end do
                end do
            else if (dst_field % ndims == 3) then
                allocate(dst_field % array3i(dst_field % dimlens(1), &
                                             dst_field % dimlens(2), &
                                             dst_field % dimlens(3)))
                do iy=1,size(nearestIndex, dim=2)
                do ix=1,size(nearestIndex, dim=1)
                    dst_field % array3i(:,ix,iy) = src_field % array2i(:,nearestIndex(ix,iy))
                end do
                end do
            else if (dst_field % ndims == 4) then
                allocate(dst_field % array4i(dst_field % dimlens(1), &
                                             dst_field % dimlens(2), &
                                             dst_field % dimlens(3), &
                                             dst_field % dimlens(4)))
                do iy=1,size(nearestIndex, dim=2)
                do ix=1,size(nearestIndex, dim=1)
                    dst_field % array4i(:,:,ix,iy) = src_field % array3i(:,:,nearestIndex(ix,iy))
                end do
                end do
            else
                write(0,*) 'Remap exception: unhandled dimension for int ', dst_field % ndims
            end if
        else
            write(0,*) 'Remap exception: unhandled type'
        end if

    end function remap_field


    integer function remap_get_target_latitudes(remap_info, lat_field) result(stat)

        implicit none

        type (remap_info_type), intent(in) :: remap_info
        type (target_field_type), intent(out) :: lat_field

        real, parameter :: rad2deg = 90.0 / asin(1.0)

        stat = 0


        lat_field % name = 'lat'
        lat_field % xtype = FIELD_TYPE_REAL
        lat_field % ndims = 1
        lat_field % isTimeDependent = .false.

        allocate(lat_field % dimnames(lat_field % ndims))
        allocate(lat_field % dimlens(lat_field % ndims))

        lat_field % dimnames(1) = 'nLats'
        lat_field % dimlens(1) = remap_info % dst_mesh % nlat

        allocate(lat_field % array1r(lat_field % dimlens(1)))
        lat_field % array1r(:) = remap_info % dst_mesh % lats(1,:) * rad2deg

    end function remap_get_target_latitudes


    integer function remap_get_target_longitudes(remap_info, lon_field) result(stat)

        implicit none

        type (remap_info_type), intent(in) :: remap_info
        type (target_field_type), intent(out) :: lon_field

        real, parameter :: rad2deg = 90.0 / asin(1.0)

        stat = 0


        lon_field % name = 'lon'
        lon_field % xtype = FIELD_TYPE_REAL
        lon_field % ndims = 1
        lon_field % isTimeDependent = .false.

        allocate(lon_field % dimnames(lon_field % ndims))
        allocate(lon_field % dimlens(lon_field % ndims))

        lon_field % dimnames(1) = 'nLons'
        lon_field % dimlens(1) = remap_info % dst_mesh % nlon

        allocate(lon_field % array1r(lon_field % dimlens(1)))
        lon_field % array1r(:) = remap_info % dst_mesh % lons(:,1) * rad2deg

    end function remap_get_target_longitudes


    integer function free_target_field(field) result(stat)

        implicit none

        type (target_field_type), intent(inout) :: field

        stat = 0

        if (associated(field % dimlens)) then
            deallocate(field % dimlens)
        end if
        if (associated(field % dimnames)) then
            deallocate(field % dimnames)
        end if

        if (associated(field % array1r)) then
            deallocate(field % array1r)
        end if
        if (associated(field % array2r)) then
            deallocate(field % array2r)
        end if
        if (associated(field % array3r)) then
            deallocate(field % array3r)
        end if
        if (associated(field % array4r)) then
            deallocate(field % array4r)
        end if

        if (associated(field % array1d)) then
            deallocate(field % array1d)
        end if
        if (associated(field % array2d)) then
            deallocate(field % array2d)
        end if
        if (associated(field % array3d)) then
            deallocate(field % array3d)
        end if
        if (associated(field % array4d)) then
            deallocate(field % array4d)
        end if

        if (associated(field % array1i)) then
            deallocate(field % array1i)
        end if
        if (associated(field % array2i)) then
            deallocate(field % array2i)
        end if
        if (associated(field % array3i)) then
            deallocate(field % array3i)
        end if
        if (associated(field % array4i)) then
            deallocate(field % array4i)
        end if

    end function free_target_field


    integer function nearest_cell(target_lat, target_lon, start_cell, nCells, maxEdges, &
                                  nEdgesOnCell, cellsOnCell, latCell, lonCell)

        implicit none

        real, intent(in) :: target_lat, target_lon
        integer, intent(in) :: start_cell
        integer, intent(in) :: nCells, maxEdges
        integer, dimension(nCells), intent(in) :: nEdgesOnCell
        integer, dimension(maxEdges,nCells), intent(in) :: cellsOnCell
        real, dimension(nCells), intent(in) :: latCell, lonCell

        integer :: i
        integer :: iCell
        integer :: current_cell
        real :: current_distance, d
        real :: nearest_distance

        nearest_cell = start_cell
        current_cell = -1

        do while (nearest_cell /= current_cell)
            current_cell = nearest_cell
            current_distance = sphere_distance(latCell(current_cell), lonCell(current_cell), target_lat, &
                                               target_lon, 1.0)
            nearest_cell = current_cell
            nearest_distance = current_distance
            do i = 1, nEdgesOnCell(current_cell)
                iCell = cellsOnCell(i,current_cell)
                if (iCell <= nCells) then
                    d = sphere_distance(latCell(iCell), lonCell(iCell), target_lat, target_lon, 1.0)
                    if (d < nearest_distance) then
                        nearest_cell = iCell
                        nearest_distance = d
                    end if
                end if
            end do
        end do

    end function nearest_cell


    integer function nearest_vertex( target_lat, target_lon, &
                                     start_vertex, &
                                     nCells, nVertices, maxEdges, vertexDegree, &
                                     nEdgesOnCell, verticesOnCell, &
                                     cellsOnVertex, latCell, lonCell, &
                                     latVertex, lonVertex )

        implicit none

        real, intent(in) :: target_lat, target_lon
        integer, intent(in) :: start_vertex
        integer, intent(in) :: nCells, nVertices, maxEdges, vertexDegree
        integer, dimension(nCells), intent(in) :: nEdgesOnCell
        integer, dimension(maxEdges,nCells), intent(in) :: verticesOnCell
        integer, dimension(vertexDegree,nVertices), intent(in) :: cellsOnVertex
        real, dimension(nCells), intent(in) :: latCell, lonCell
        real, dimension(nVertices), intent(in) :: latVertex, lonVertex


        integer :: i, cell1, cell2, cell3, iCell
        integer :: iVtx
        integer :: current_vertex
        real :: cell1_dist, cell2_dist, cell3_dist
        real :: current_distance, d
        real :: nearest_distance

        nearest_vertex = start_vertex
        current_vertex = -1

        do while (nearest_vertex /= current_vertex)
            current_vertex = nearest_vertex
            current_distance = sphere_distance(latVertex(current_vertex), lonVertex(current_vertex), &
                                               target_lat,                target_lon,                1.0)
            nearest_vertex = current_vertex
            nearest_distance = current_distance
            cell1 = cellsOnVertex(1,current_vertex)
            cell1_dist = sphere_distance(latCell(cell1), lonCell(cell1), target_lat, target_lon, 1.0)
            cell2 = cellsOnVertex(2,current_vertex)
            cell2_dist = sphere_distance(latCell(cell2), lonCell(cell2), target_lat, target_lon, 1.0)
            if (vertexDegree == 3) then
               cell3 = cellsOnVertex(3,current_vertex)
               cell3_dist = sphere_distance(latCell(cell3), lonCell(cell3), target_lat, target_lon, 1.0)
            end if
            if (vertexDegree == 3) then
                if (cell1_dist < cell2_dist) then
                    if (cell1_dist < cell3_dist) then
                        iCell = cell1
                    else
                        iCell = cell3
                    end if
                else
                    if (cell2_dist < cell3_dist) then
                        iCell = cell2
                    else
                        iCell = cell3
                    end if
                end if
            else
                if (cell1_dist < cell2_dist) then
                    iCell = cell1
                else
                    iCell = cell2
                end if
            end if
            do i = 1, nEdgesOnCell(iCell)
                iVtx = verticesOnCell(i,iCell)
                d = sphere_distance(latVertex(iVtx), lonVertex(iVtx), target_lat, target_lon, 1.0)
                if (d < nearest_distance) then
                    nearest_vertex = iVtx
                    nearest_distance = d
                end if
            end do
        end do

    end function nearest_vertex


    real function sphere_distance(lat1, lon1, lat2, lon2, radius)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Compute the great-circle distance between (lat1, lon1) and (lat2, lon2) on a
    !   sphere with given radius.
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        implicit none

        real, intent(in) :: lat1, lon1, lat2, lon2, radius

        real :: arg1

        arg1 = sqrt( sin(0.5*(lat2-lat1))**2 +  &
                     cos(lat1)*cos(lat2)*sin(0.5*(lon2-lon1))**2 )
        sphere_distance = 2.*radius*asin(arg1)

    end function sphere_distance


    !***********************************************************************
    !
    !  function mpas_wachspress_coordinates
    !
    !> \brief Compute the barycentric Wachspress coordinates for a polygon
    !> \author  Phillip Wolfram
    !> \date    01/26/2015
    !> \details
    !>  Computes the barycentric Wachspress coordinates for a polygon with nVertices
    !>  points in R3, vertCoords for a particular pointInterp with normalized radius.
    !>  Follows Gillette, A., Rand, A., Bajaj, C., 2011.
    !>  Error estimates for generalized barycentric interpolation.
    !>  Advances in computational mathematics 37 (3), 417–439.
    !>  Optimized version of mpas_wachspress_coordinates uses optional cached B_i areas
    !------------------------------------------------------------------------
    function mpas_wachspress_coordinates(nVertices, vertCoords, pointInterp, areaBin)

        implicit none

        ! input points
        integer, intent(in) :: nVertices
        real, dimension(3, nVertices), intent(in) :: vertCoords
        real, dimension(3), intent(in) :: pointInterp
        real, dimension(nVertices), optional, intent(in) :: areaBin

        ! output
        real, dimension(nVertices) :: mpas_wachspress_coordinates

        ! computational intermediates
        real, dimension(nVertices) :: wach       ! The wachpress area-product
        real :: wach_total                       ! The wachpress total weight
        integer :: i, j                                       ! Loop indices
        integer :: im1, i0, ip1                               ! im1 = (i-1), i0 = i, ip1 = (i+1)
  
        ! triangle areas to compute wachspress coordinate
        real, dimension(nVertices) :: areaA
        real, dimension(nVertices) :: areaB

        real :: radiusLocal

        radiusLocal = sqrt(sum(vertCoords(:,1)**2))

        if (.not. present(areaBin)) then
            ! compute areas
            do i = 1, nVertices
                ! compute first area B_i
                ! get vertex indices
                im1 = mod(nVertices + i - 2, nVertices) + 1
                i0  = mod(nVertices + i - 1, nVertices) + 1
                ip1 = mod(nVertices + i    , nVertices) + 1
   
                ! precompute B_i areas
                ! always the same because B_i independent of xp,yp,zp
                ! (COULD CACHE AND USE RESULT FROM ARRAY FOR FURTHER OPTIMIZATION)
                areaB(i) = mpas_triangle_signed_area_sphere(vertCoords(:, im1), vertCoords(:, i0), vertCoords(:, ip1), radiusLocal)
            end do
        else
            ! assign areas
            do i = 1, nVertices
                areaB(i) = areaBin(i)
            end do
        end if

        ! compute areas
        do i = 1, nVertices
            ! compute first area B_i
            ! get vertex indices
            im1 = mod(nVertices + i - 2, nVertices) + 1
            i0  = mod(nVertices + i - 1, nVertices) + 1
            ip1 = mod(nVertices + i    , nVertices) + 1
   
            ! compute A_ij areas
            ! must be computed each time
            areaA(i0) = mpas_triangle_signed_area_sphere(pointInterp, vertCoords(:, i0), vertCoords(:, ip1), radiusLocal)
   
            ! precomputed B_i areas, cached
        end do


        ! for each vertex compute wachpress coordinate
        do i = 1, nVertices
            wach(i) = areaB(i)
            do j = (i + 1), (i + nVertices - 2)
                i0  = mod(nVertices + j - 1, nVertices) + 1
                ! accumulate products for A_ij subareas
                wach(i) = wach(i) * areaA(i0)
            end do
        end do

        ! get summed weights for normalization
        wach_total = 0
        do i = 1, nVertices
            wach_total = wach_total + wach(i)
        end do

        ! compute lambda
        mpas_wachspress_coordinates= 0.0
        do i = 1, nVertices
            mpas_wachspress_coordinates(i) = wach(i)/wach_total
        end do

    end function mpas_wachspress_coordinates


    !***********************************************************************
    !
    !  routine mpas_triangle_signed_area_sphere
    !
    !> \brief   Calculates area of a triangle on a sphere
    !> \author  Matthew Hoffman
    !> \date    13 January 2015
    !> \details
    !>  This routine calculates the area of a triangle on the surface of a sphere.
    !>  Uses the spherical analog of Heron's formula.
    !>  Copied from mesh generator.  A CCW winding angle is positive.
    !-----------------------------------------------------------------------
    real function mpas_triangle_signed_area_sphere(a, b, c, radius)

        implicit none

        !-----------------------------------------------------------------
        ! input variables
        !-----------------------------------------------------------------
        real, dimension(3), intent(in) :: a, b, c  !< Input: 3d (x,y,z) points forming the triangle in which to calculate the bary weights
        real, intent(in) :: radius  !< sphere radius
 
        !-----------------------------------------------------------------
        ! local variables
        !-----------------------------------------------------------------
        real :: ab, bc, ca, semiperim, tanqe
        real, dimension(3) :: ablen, aclen, Dlen
 
        ab = mpas_arc_length(a(1), a(2), a(3), b(1), b(2), b(3))/radius
        bc = mpas_arc_length(b(1), b(2), b(3), c(1), c(2), c(3))/radius
        ca = mpas_arc_length(c(1), c(2), c(3), a(1), a(2), a(3))/radius
        semiperim = 0.5 * (ab + bc + ca)
 
        tanqe = sqrt(max(0.0,tan(0.5 * semiperim) * tan(0.5 * (semiperim - ab)) &
                     * tan(0.5 * (semiperim - bc)) * tan(0.5 * (semiperim - ca))))
 
        mpas_triangle_signed_area_sphere = 4.0 * radius * radius * atan(tanqe)
 
        ! computing correct signs (in similar fashion to mpas_sphere_angle)
        ablen(1) = b(1) - a(1)
        ablen(2) = b(2) - a(2)
        ablen(3) = b(3) - a(3)
 
        aclen(1) = c(1) - a(1)
        aclen(2) = c(2) - a(2)
        aclen(3) = c(3) - a(3)
 
        dlen(1) =   (ablen(2) * aclen(3)) - (ablen(3) * aclen(2))
        dlen(2) = -((ablen(1) * aclen(3)) - (ablen(3) * aclen(1)))
        dlen(3) =   (ablen(1) * aclen(2)) - (ablen(2) * aclen(1))
 
        if ((Dlen(1)*a(1) + Dlen(2)*a(2) + Dlen(3)*a(3)) < 0.0) then
            mpas_triangle_signed_area_sphere = -mpas_triangle_signed_area_sphere
        end if
 
    end function mpas_triangle_signed_area_sphere


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! FUNCTION MPAS_ARC_LENGTH
    !
    ! Returns the length of the great circle arc from A=(ax, ay, az) to
    !    B=(bx, by, bz). It is assumed that both A and B lie on the surface of the
    !    same sphere centered at the origin.
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    real function mpas_arc_length(ax, ay, az, bx, by, bz)
 
        implicit none
 
        real, intent(in) :: ax, ay, az, bx, by, bz
 
        real :: r, c
        real :: cx, cy, cz
 
        cx = bx - ax
        cy = by - ay
        cz = bz - az
  
        r = sqrt(ax*ax + ay*ay + az*az)
        c = sqrt(cx*cx + cy*cy + cz*cz)

        mpas_arc_length = r * 2.0 * asin(c/(2.0*r))
 
    end function mpas_arc_length


    function convert_lx(lat, lon, radius) result(vec)

        implicit none

        real, intent(in) :: lat, lon, radius

        real, dimension(3) :: vec

        vec(1) = radius * cos(lon) * cos(lat)
        vec(2) = radius * sin(lon) * cos(lat)
        vec(3) = radius * sin(lat)

    end function convert_lx


    function index2d(irank, idx) result(i)

        implicit none

        integer, intent(in) :: irank, idx

        integer :: i

        i = irank * (idx - 1) + 1

    end function index2d

end module remapper
