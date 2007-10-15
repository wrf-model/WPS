include ../../configure.wps

OBJS = cio.o wrf_debug.o bitarray_module.o constants_module.o despace.o geogrid.o gridinfo_module.o hash_module.o interp_module.o list_module.o llxy_module.o misc_definitions_module.o module_debug.o module_map_utils.o output_module.o parallel_module.o process_tile_module.o proc_point_module.o queue_module.o read_geogrid.o smooth_module.o source_data_module.o

all:
	clear ;
	@echo " "
	@echo "go up two directories and type compile to build WPS"
	@echo " "
	@echo " "

geogrid.exe: $(OBJS)
	$(FC) $(LDFLAGS) -o $@ $(OBJS) \
		$(WRF_DIR)/frame/pack_utils.o $(WRF_DIR)/frame/module_machine.o \
		$(WRF_DIR)/frame/module_internal_header_util.o \
		-I$(WRF_DIR)/external/io_grib1 \
		-I$(WRF_DIR)/external/io_grib_share \
		-I$(WRF_DIR)/external/io_int \
		-I$(NETCDF)/include \
		-I$(WRF_DIR)/external/io_netcdf \
		-L$(WRF_DIR)/external/io_grib1 -lio_grib1 \
		-L$(WRF_DIR)/external/io_grib_share -lio_grib_share \
		-L$(WRF_DIR)/external/io_int -lwrfio_int \
		-L$(WRF_DIR)/external/io_netcdf -lwrfio_nf \
		-L$(NETCDF)/lib -lnetcdf

bitarray_module.o: module_debug.o

cio.o:

constants_module.o:

despace.o:

geogrid.o: bitarray_module.o constants_module.o hash_module.o gridinfo_module.o llxy_module.o misc_definitions_module.o module_debug.o output_module.o parallel_module.o process_tile_module.o proc_point_module.o queue_module.o smooth_module.o source_data_module.o list_module.o

gridinfo_module.o: constants_module.o misc_definitions_module.o module_debug.o

hash_module.o:

interp_module.o: bitarray_module.o misc_definitions_module.o module_debug.o queue_module.o

list_module.o: module_debug.o

llxy_module.o: gridinfo_module.o module_debug.o module_map_utils.o misc_definitions_module.o list_module.o

misc_definitions_module.o:

module_debug.o: parallel_module.o

module_map_utils.o: constants_module.o misc_definitions_module.o module_debug.o

output_module.o: gridinfo_module.o llxy_module.o misc_definitions_module.o module_debug.o parallel_module.o source_data_module.o

parallel_module.o:

process_tile_module.o: bitarray_module.o constants_module.o hash_module.o gridinfo_module.o interp_module.o llxy_module.o misc_definitions_module.o module_debug.o module_map_utils.o output_module.o parallel_module.o proc_point_module.o queue_module.o smooth_module.o source_data_module.o

proc_point_module.o: bitarray_module.o hash_module.o interp_module.o llxy_module.o misc_definitions_module.o module_debug.o source_data_module.o

queue_module.o: module_debug.o

read_geogrid.o:

smooth_module.o: parallel_module.o

source_data_module.o: despace.o gridinfo_module.o hash_module.o list_module.o llxy_module.o misc_definitions_module.o module_debug.o

wrf_debug.o: cio.o

clean:
	$(RM) $(OBJS) *.mod
	$(RM) geogrid.exe 

superclean:	clean
