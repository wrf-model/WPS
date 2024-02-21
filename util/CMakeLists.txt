add_library( util_common STATIC )
# todo - Find how to compile this with NCAR Graphics, will need to write a find_package() module
# add_executable( plotgrids )
add_executable( rd_intermediate )
add_executable( avg_tsfc )
# Find out if this needs compiling, ./compile suggests no
# add_executable( elev_angle )
add_executable( calc_ecmwf_p )
# Same thing as plotgrids
# add_executable( plotfmt )
add_executable( mod_levs )
if ( DEFINED WRF_DIR )
  add_executable( int2nc )
endif()
add_executable( height_ukmo )

set(
    ALL_UTIL_TARGETS
      # plotgrids
      rd_intermediate
      avg_tsfc
      # elev_angle
      calc_ecmwf_p
      # plotfmt
      mod_levs
      height_ukmo
      )

if ( DEFINED WRF_DIR )
  list( APPEND ALL_UTIL_TARGETS int2nc )
endif()

add_subdirectory( src )

# Specific linking
# target_link_libraries(
#                       elev_angle
#                       PRIVATE
#                         ${netCDF_LIBRARIES}
#                       )
# target_include_directories(
#                             elev_angle
#                             PRIVATE
#                               ${netCDF_INCLUDE_DIRS}
#                             )

if ( DEFINED WRF_DIR )
  target_link_libraries(
                        int2nc
                        PRIVATE
                          WRF::WRF_Core
                          ${netCDF_LIBRARIES}
                        )
  target_include_directories(
                            int2nc
                            PRIVATE
                              ${netCDF_INCLUDE_DIRS}
                            )
endif()

# Every single target
foreach( TARGET ${ALL_UTIL_TARGETS} util_common )

  # Set the mod dir
  set_target_properties( 
                        ${TARGET}
                        PROPERTIES
                          # Just dump everything in here
                          Fortran_MODULE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/modules/util/
                          Fortran_FORMAT           FREE
                        )

  # Everything except util_common
  if ( NOT "${TARGET}" STREQUAL util_common )
    target_link_libraries(
                          ${TARGET}
                          PRIVATE
                            util_common
                          )
  endif()

  # Add these to the export targets
  install(
          TARGETS ${TARGET}
          EXPORT ${EXPORT_NAME}Targets
          RUNTIME DESTINATION bin/
          ARCHIVE DESTINATION lib/
          LIBRARY DESTINATION lib/
          )

endforeach()

