get_filename_component( FOLDER_COMPILE_TARGET ${CMAKE_CURRENT_SOURCE_DIR} NAME )

add_library( ${FOLDER_COMPILE_TARGET} STATIC )

set_target_properties( 
                      ${FOLDER_COMPILE_TARGET}
                        PROPERTIES
                          Fortran_MODULE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/${FOLDER_COMPILE_TARGET}
                          # Fortran_FORMAT           FREE
                      )
target_sources( 
                ${FOLDER_COMPILE_TARGET}
                PRIVATE
                  getgb.f
                  getgbmh.f
                  putgbex.f
                  w3fi73.f
                  getgb1r.f
                  getgbmp.f
                  putgbn.f
                  w3fi74.f
                  getgb1re.f
                  getgbp.f
                  r63w72.f
                  w3fi75.f
                  getgb1s.f
                  getgi.f
                  sbyte.f
                  w3fi76.f
                  getgbe.f
                  getgir.f
                  sbytes.f
                  w3fi82.f
                  getgbeh.f
                  idsdef.f
                  w3fi83.f
                  getgbem.f
                  iw3jdn.f
                  w3difdat.f
                  w3fs26.f
                  getgbemh.f
                  ixgb.f
                  w3doxdat.f
                  w3log.f
                  getgbemp.f
                  lengds.f
                  w3fi01.f
                  w3movdat.f
                  getgbens.f
                  pdsens.f
                  w3fi58.f
                  w3reddat.f
                  getgbep.f
                  pdseup.f
                  w3fi59.f
                  w3tagb.f
                  errmsg.f
                  getgbex.f
                  putgb.f
                  w3fi63.f
                  gbytes.f
                  getgbexm.f
                  putgbe.f
                  w3fi68.f
                  gbytes_char.f
                  getgbh.f
                  putgben.f
                  getbit.f
                  getgbm.f
                  putgbens.f
                  w3fi72.f
                  fparsei.f
                  fparser.f
                  instrument.f
                  start.f
                  summary.f
                  w3utcdat.f
                  w3fs21.f
                  w3locdat.f
                  w3fi71.f
                  baciof.f
                  bacio.v1.3.c
                  errexit.c
                )

install(
        TARGETS ${FOLDER_COMPILE_TARGET}
        EXPORT ${EXPORT_NAME}Targets
        RUNTIME DESTINATION bin/
        ARCHIVE DESTINATION lib/
        LIBRARY DESTINATION lib/
        )
