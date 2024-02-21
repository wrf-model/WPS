add_executable( ungrib )
add_executable( g1print )
add_executable( g2print )
add_library   ( pgu )

add_subdirectory( src )

set_target_properties( 
                      pgu
                      PROPERTIES
                        # Just dump everything in here
                        Fortran_MODULE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/modules/ungrib/
                        Fortran_FORMAT           FREE
                      )

set_target_properties( 
                      ungrib
                      PROPERTIES
                        # Just dump everything in here
                        Fortran_MODULE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/modules/ungrib/
                        Fortran_FORMAT           FREE
                      )

set_target_properties( 
                      g1print
                      PROPERTIES
                        # Just dump everything in here
                        Fortran_MODULE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/modules/ungrib/
                        Fortran_FORMAT           FREE
                      )

set_target_properties( 
                      g2print
                      PROPERTIES
                        # Just dump everything in here
                        Fortran_MODULE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/modules/ungrib/
                        Fortran_FORMAT           FREE
                      )

# Control dependencies and linking
target_link_libraries( 
                      ungrib
                      PRIVATE
                        w3
                        g2
                        pgu
                        Jasper::Jasper
                        PNG::PNG
                        ZLIB::ZLIB
                      )

target_link_libraries(
                      g1print
                      PRIVATE
                        pgu
                      )

target_link_libraries(
                      g2print
                      PRIVATE
                        w3
                        g2
                        Jasper::Jasper
                        PNG::PNG
                        ZLIB::ZLIB
                        pgu
                      )


target_include_directories(
                            ungrib
                            PRIVATE
                              $<TARGET_PROPERTY:g2,Fortran_MODULE_DIRECTORY>
                              $<TARGET_PROPERTY:w3,Fortran_MODULE_DIRECTORY>
                            )


target_include_directories(
                            g2print
                            PRIVATE
                              $<TARGET_PROPERTY:g2,Fortran_MODULE_DIRECTORY>
                              $<TARGET_PROPERTY:w3,Fortran_MODULE_DIRECTORY>
                            )
# $<$<BOOL:${USE_MPI}>:$<TARGET_NAME_IF_EXISTS:MPI::MPI_Fortran>>
# $<$<BOOL:${USE_OPENMP}>:$<TARGET_NAME_IF_EXISTS:OpenMP::OpenMP_Fortran>>
# ${netCDF_INCLUDE_DIRS}


# Add these to the export targets
install(
        TARGETS ungrib g1print g2print pgu
        EXPORT ${EXPORT_NAME}Targets
        RUNTIME DESTINATION bin/
        ARCHIVE DESTINATION lib/
        LIBRARY DESTINATION lib/
        )
