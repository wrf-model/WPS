cmake_minimum_required( VERSION 3.20 )
cmake_policy( SET CMP0118 NEW )

enable_language( C )
enable_language( CXX )
enable_language( Fortran )

project( WPS )
set( EXPORT_NAME          ${PROJECT_NAME} )
set( INTERNAL_GRIB2_PATH  ${CMAKE_INSTALL_PREFIX}/grib2 )

# if ( DEFINED CMAKE_TOOLCHAIN_FILE )
#   set( WPS_CONFIG ${CMAKE_TOOLCHAIN_FILE} )
#   message( STATUS "Loading configuration file... : ${WPS_CONFIG}" )
#   include( ${WPS_CONFIG} )
# endif()

# list( APPEND CMAKE_MODULE_PATH         )
list( APPEND CMAKE_MODULE_PATH 
      ${PROJECT_SOURCE_DIR}/cmake/
      ${PROJECT_SOURCE_DIR}/cmake/modules
      )

# I'd love to be able to do something like this, but this would place a depedency
# on WRF which would be bad since who would expect the *WRF Preprocessing System*
# to have a strict dependency on WRF. Silly me
# if ( DEFINED WRF_DIR )
#   list( APPEND CMAKE_MODULE_PATH 
#       ${WRF_DIR}/share
#       )
# endif()

# Use link paths as rpaths 
set( CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE )
set( CMAKE_Fortran_PREPROCESS          ON )

include( CMakePackageConfigHelpers )
# include( confcheck   )
# include( gitinfo     )
include( wrf_get_version )

# Grab version info
wrf_get_version( ${PROJECT_SOURCE_DIR}/README )

################################################################################
##
## MPI & OpenMP
##
################################################################################
if ( ${USE_MPI} )
  # Through ***MUCH*** debugging, if utilizing MPI_<LANG>_COMPILER
  # https://cmake.org/cmake/help/latest/module/FindMPI.html#variables-for-locating-mpi
  # the find logic makes a mess of things by utilizing <mpi> -show[me]
  # Which may or may not get polluted by the environment
  # It still technically finds MPI but the output is nonintuitive 
  # saying things like hdf5 or pthread
  find_package( MPI REQUIRED COMPONENTS Fortran C )
  add_compile_definitions(
                          USE_MPI=1
                          DM_PARALLEL
                          )

  if ( DEFINED WRF_MPI_Fortran_FLAGS AND NOT "${WRF_MPI_Fortran_FLAGS}" STREQUAL "" )
    add_compile_options(
                        $<$<COMPILE_LANGUAGE:Fortran>,${WRF_MPI_Fortran_FLAGS}>
                        )
  endif()

  if ( DEFINED WRF_MPI_C_FLAGS AND NOT "${WRF_MPI_C_FLAGS}" STREQUAL "" )
    add_compile_options(
                        $<$<COMPILE_LANGUAGE:C>,${WRF_MPI_C_FLAGS}>
                        )
  endif()
endif()

if ( ${USE_OPENMP} )
  find_package( OpenMP REQUIRED COMPONENTS Fortran C )
  add_compile_definitions( USE_OPENMP=1 SM_PARALLEL )
endif()


# First do externals
if ( BUILD_EXTERNALS )
  add_subdirectory( external )
  # If we got here everything built, we are safe to add find paths for libs
  set( ZLIB_ROOT   ${INTERNAL_GRIB2_PATH} ) # This may get overridden by HDF5 if zlib is packaged with that
  set( PNG_ROOT    ${INTERNAL_GRIB2_PATH} )
  set( Jasper_ROOT ${INTERNAL_GRIB2_PATH} )
endif()


# Now find required libraries, which may have been affected by externals
if ( DEFINED WRF_DIR )
  find_package( WRF COMPONENTS WRF_Core io_netcdf io_grib1 io_int HINTS ${WRF_DIR}/lib )
endif()

# Find externals now -- this is a little different than normal since we want WRF to 
# take precedence if it is present on which libraries to use to avoid library splicing
# rather than finding our external thirdparty libraries first
find_package( ZLIB   REQUIRED )
find_package( PNG    REQUIRED )
find_package( Jasper REQUIRED )

# Add config definitions
message( STATUS "Adding configuration specific definitions : ${WPS_DEFINITIONS}" )
# In CMake 2.26 this will not be necessary
string( REPLACE " " ";" WPS_DEFINITIONS_LIST ${WPS_DEFINITIONS} )
list( TRANSFORM WPS_DEFINITIONS_LIST REPLACE "^-D(.*)" "\\1" )
add_compile_definitions( 
                        ${WPS_DEFINITIONS_LIST}
                        $<$<COMPILE_LANGUAGE:Fortran>:USE_JPEG2000>
                        $<$<COMPILE_LANGUAGE:Fortran>:USE_PNG>
                        )

# Always build whatever we can
add_subdirectory( ungrib )

if ( DEFINED WRF_DIR )
  # WRF Specific things
  add_subdirectory( metgrid )
  add_subdirectory( geogrid )
endif()


################################################################################
##
## Install and export
##
################################################################################

# Install to namespace
install(
        EXPORT      ${EXPORT_NAME}Targets
        DESTINATION lib/cmake/
        FILE        ${EXPORT_NAME}Targets.cmake
        NAMESPACE   ${EXPORT_NAME}::
        )

configure_package_config_file(
                              ${PROJECT_SOURCE_DIR}/cmake/template/${EXPORT_NAME}Config.cmake.in
                              ${CMAKE_BINARY_DIR}/${EXPORT_NAME}Config.cmake
                              INSTALL_DESTINATION lib/cmake
                              )

write_basic_package_version_file(
                                  ${CMAKE_BINARY_DIR}/${EXPORT_NAME}ConfigVersion.cmake
                                  VERSION ${PROJECT_VERSION}
                                  #!TODO Check if this is the type of versioning support we want to use
                                  COMPATIBILITY SameMinorVersion
                                  )

install(
        FILES
          ${CMAKE_BINARY_DIR}/${EXPORT_NAME}Config.cmake
          ${CMAKE_BINARY_DIR}/${EXPORT_NAME}ConfigVersion.cmake
        DESTINATION lib/cmake
        )

# Install some helper files for anyone using this build as part of their code
install(
        DIRECTORY
          # Trailing / is important
          ${PROJECT_SOURCE_DIR}/cmake/modules/
        COMPONENT       helpers
        DESTINATION     share
        FILES_MATCHING  
          PATTERN       "*.cmake"
        )
# install(
#         FILES
#           ${PROJECT_SOURCE_DIR}/cmake/confcheck.cmake
#           ${PROJECT_SOURCE_DIR}/cmake/gitinfo.cmake
#           ${PROJECT_SOURCE_DIR}/cmake/printOption.cmake
#           ${PROJECT_SOURCE_DIR}/cmake/wrf_get_version.cmake
#         COMPONENT     helpers
#         DESTINATION   share
#         )
