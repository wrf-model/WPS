add_executable( geogrid )

add_subdirectory( src )

set_target_properties( 
                      geogrid
                      PROPERTIES
                        # Just dump everything in here
                        Fortran_MODULE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/geogrid/
                        Fortran_FORMAT           FREE
                      )


# Control dependencies and linking
target_link_libraries( 
                      geogrid
                      PRIVATE
                        $<$<BOOL:${USE_MPI}>:$<TARGET_NAME_IF_EXISTS:MPI::MPI_Fortran>>
                        $<$<BOOL:${USE_OPENMP}>:$<TARGET_NAME_IF_EXISTS:OpenMP::OpenMP_Fortran>>
                        $<$<BOOL:${USE_OPENMP}>:$<TARGET_NAME_IF_EXISTS:OpenMP::OpenMP_C>>
                        ${netCDF_LIBRARIES}
                        WRF::WRF_Core
                        WRF::io_netcdf
                        WRF::io_grib1
                        WRF::io_grib_share
                      )

target_compile_definitions(
                            geogrid
                            PRIVATE
                              _GEOGRID
                            )

target_include_directories(
                            geogrid
                            PRIVATE
                              ${netCDF_INCLUDE_DIRS}
                            )

# Add these to the export targets
install(
        TARGETS geogrid
        EXPORT ${EXPORT_NAME}Targets
        RUNTIME DESTINATION bin/
        ARCHIVE DESTINATION lib/
        LIBRARY DESTINATION lib/
        )
