add_executable( metgrid )

add_subdirectory( src )

set_target_properties( 
                      metgrid
                      PROPERTIES
                        # Just dump everything in here
                        Fortran_MODULE_DIRECTORY ${CMAKE_INSTALL_PREFIX}/modules/metgrid/
                        Fortran_FORMAT           FREE
                      )


# Control dependencies and linking
target_link_libraries( 
                      metgrid
                      PRIVATE
                        $<$<BOOL:${USE_MPI}>:$<TARGET_NAME_IF_EXISTS:MPI::MPI_Fortran>>
                        $<$<BOOL:${USE_OPENMP}>:$<TARGET_NAME_IF_EXISTS:OpenMP::OpenMP_Fortran>>
                        $<$<BOOL:${USE_OPENMP}>:$<TARGET_NAME_IF_EXISTS:OpenMP::OpenMP_C>>
                        ${netCDF_LIBRARIES}
                        WRF::io_netcdf
                        WRF::io_int
                        WRF::io_grib1
                        WRF::io_grib_share
                        WRF::WRF_Core
                      )

target_compile_definitions(
                            metgrid
                            PRIVATE
                              _METGRID
                            )

target_include_directories(
                            metgrid
                            PRIVATE
                              ${netCDF_INCLUDE_DIRS}
                            )

# Add these to the export targets
install(
        TARGETS metgrid
        EXPORT ${EXPORT_NAME}Targets
        RUNTIME DESTINATION bin/
        ARCHIVE DESTINATION lib/
        LIBRARY DESTINATION lib/
        )
