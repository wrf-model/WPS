get_filename_component( FOLDER_COMPILE_TARGET ${CMAKE_CURRENT_SOURCE_DIR} NAME )

add_library( ${FOLDER_COMPILE_TARGET} STATIC )

set_target_properties( 
                      ${FOLDER_COMPILE_TARGET}
                        PROPERTIES
                          Fortran_MODULE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/${FOLDER_COMPILE_TARGET}
                          # Fortran_FORMAT           FREE
                      )

target_compile_definitions(
                            ${FOLDER_COMPILE_TARGET}
                            PRIVATE
                              __64BIT__
                          )
# target_link_libraries(  ${FOLDER_COMPILE_TARGET}
#                           PRIVATE
#                             ${netCDF_LIBRARIES}
#                             $<$<BOOL:${USE_MPI}>:$<TARGET_NAME_IF_EXISTS:MPI::MPI_Fortran>>
#                             $<$<BOOL:${USE_OPENMP}>:$<TARGET_NAME_IF_EXISTS:OpenMP::OpenMP_Fortran>>
#                             $<TARGET_NAME_IF_EXISTS:Jasper::Jasper>
#                         )

target_sources( 
                ${FOLDER_COMPILE_TARGET}
                PRIVATE
                  gridtemplates.f
                  pdstemplates.f
                  drstemplates.f
                  gribmod.f
                  realloc.f
                  intmath.f
                  addfield.F
                  addgrid.f
                  addlocal.f
                  getfield.f
                  gb_info.f
                  gf_getfld.f
                  gf_free.f
                  gf_unpack1.f
                  gf_unpack2.f
                  gf_unpack3.f
                  gf_unpack4.f
                  gf_unpack5.f
                  gf_unpack6.f
                  gf_unpack7.F
                  gettemplates.f
                  getlocal.f
                  getdim.f
                  getpoly.f
                  gribcreate.f
                  gribend.f
                  gribinfo.f
                  mkieee.f
                  rdieee.f
                  simunpack.f
                  reduce.f
                  comunpack.f
                  specunpack.f
                  jpcunpack.F
                  dec_jpeg2000.c
                  pngunpack.F
                  dec_png.c
                  gbytesc.f
                  skgb.f
                  ixgb2.f
                  getidx.f
                  getg2i.f
                  getg2ir.f
                  getgb2s.f
                  getgb2r.f
                  getgb2l.f
                  getgb2.f
                  getgb2p.f
                  getgb2rp.f
                  putgb2.f
                  g2grids.f
                  gdt2gds.f
                  params.f
                  params_ecmwf.f
                  mova2i.c
                )

install(
        TARGETS ${FOLDER_COMPILE_TARGET}
        EXPORT ${EXPORT_NAME}Targets
        RUNTIME DESTINATION bin/
        ARCHIVE DESTINATION lib/
        LIBRARY DESTINATION lib/
        )
