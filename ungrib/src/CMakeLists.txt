add_subdirectory( ngl )


# Special cases, maybe one day these will get fixed
set_source_files_properties( 
                            ${CMAKE_CURRENT_SOURCE_DIR}/rd_grib2.F
                            TARGET_DIRECTORY ungrib
                            PROPERTIES
                              Fortran_FORMAT FIXED
                            )

target_sources(
                pgu
                PRIVATE
                  build_hdate.F
                  geth_newdate.F
                  geth_idts.F
                  swap.F
                  table.F
                  module_stringutil.F
                  parse_table.F
                  gbytesys.F
                  gribcode.F
                  read_namelist.F
                  cio.c
                  module_debug.F
                  misc_definitions_module.F
                )

target_sources(
                ungrib
                PRIVATE
                  debug_cio.c
                  module_stringutil.F
                  table.F
                  module_datarray.F
                  gridinfo.F
                  new_storage.F
                  filelist.F
                  ungrib.F
                  output.F
                  rrpr.F
                  rd_grib1.F
                  file_delete.F
                  datint.F
                  rd_grib2.F
                )

target_sources(
                g1print
                PRIVATE
                  g1print.F
                  gribcode.F
                  module_debug.F
                  debug_cio.c
                )

target_sources(
                g2print
                PRIVATE
                  filelist.F
                  gridinfo.F
                  g2print.F
                )
