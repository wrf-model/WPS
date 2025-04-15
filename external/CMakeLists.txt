message( STATUS "Building external libraries. This may take awhile..." )
list(APPEND CMAKE_MESSAGE_INDENT "  ")
# Set up basic environment variables

set( ENV{CC}         ${CMAKE_C_COMPILER}       )
set( ENV{CXX}        ${CMAKE_CXX_COMPILER}     )
set( ENV{FC}         ${CMAKE_Fortran_COMPILER} )
set( ENV{FCFLAGS}    ${CMAKE_Fortran_FLAGS}    )
# set( ENV{F77}        gfortran                  )
set( ENV{FFLAGS}     ${CMAKE_Fortran_FLAGS}    )
# set( ENV{LDFLAGS}    -L$DIR/grib2/lib          )
# set( ENV{CPPFLAGS}   -I$DIR/grib2/include      )

################################################################################
##
## ZLib
##
################################################################################
set( TARGET_FOLDER zlib-1.2.11 )
set( TAG_FILE      ${INTERNAL_GRIB2_PATH}/include/zlib.h )
if ( NOT EXISTS "${TAG_FILE}" )
  set( LOG_FILE      ${CMAKE_CURRENT_BINARY_DIR}/${TARGET_FOLDER}-configure.log )
  message( STATUS "Configuring ${TARGET_FOLDER}...")
  execute_process( 
                  COMMAND 
                    ./configure --prefix=${INTERNAL_GRIB2_PATH} --static
                  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${TARGET_FOLDER}
                  OUTPUT_FILE       ${LOG_FILE}
                  ERROR_FILE        ${LOG_FILE}
                  # COMMAND_ECHO      STDOUT
                  RESULT_VARIABLE   ${TARGET_FOLDER}_STATUS_CONF
                  )
  if ( ${TARGET_FOLDER}_STATUS_CONF AND NOT ${TARGET_FOLDER}_STATUS_CONF EQUAL 0 )
    message( FATAL_ERROR "Failed to configure ${TARGET_FOLDER}, see log file ${LOG_FILE} for more details" )
  else()
    message( STATUS "Finished configuring ${TARGET_FOLDER}" )
  endif()

  set( LOG_FILE      ${CMAKE_CURRENT_BINARY_DIR}/${TARGET_FOLDER}-build.log )
  message( STATUS "Building ${TARGET_FOLDER}...")
  execute_process(
                  COMMAND 
                    make install
                  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${TARGET_FOLDER}
                  OUTPUT_FILE       ${LOG_FILE}
                  ERROR_FILE        ${LOG_FILE}
                  # COMMAND_ECHO      STDOUT
                  RESULT_VARIABLE   ${TARGET_FOLDER}_STATUS
                  )
  if ( ${TARGET_FOLDER}_STATUS AND NOT ${TARGET_FOLDER}_STATUS EQUAL 0 AND NOT EXISTS "${TAG_FILE}" )
    message( FATAL_ERROR "Failed to build ${TARGET_FOLDER}, see log file ${LOG_FILE} for more details" )
  else()
    message( STATUS "Finished building ${TARGET_FOLDER}" )
  endif()
else()
  message( STATUS "External ${TARGET_FOLDER} already built, skipping" )
endif()

################################################################################
##
## libPNG
##
################################################################################
set( TARGET_FOLDER libpng-1.6.37 )
set( TAG_FILE      ${INTERNAL_GRIB2_PATH}/include/png.h )
if ( NOT EXISTS "${TAG_FILE}" )
  set( LOG_FILE      ${CMAKE_CURRENT_BINARY_DIR}/${TARGET_FOLDER}-configure.log )
  message( STATUS "Configuring ${TARGET_FOLDER}...")
  execute_process( 
                  COMMAND 
                    ./configure --prefix=${INTERNAL_GRIB2_PATH} --disable-shared --with-zlib-prefix=${INTERNAL_GRIB2_PATH}/
                  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${TARGET_FOLDER}
                  OUTPUT_FILE       ${LOG_FILE}
                  ERROR_FILE        ${LOG_FILE}
                  # COMMAND_ECHO      STDOUT
                  RESULT_VARIABLE   ${TARGET_FOLDER}_STATUS_CONF
                  )
  if ( ${TARGET_FOLDER}_STATUS_CONF AND NOT ${TARGET_FOLDER}_STATUS_CONF EQUAL 0 )
    message( FATAL_ERROR "Failed to configure ${TARGET_FOLDER}, see log file ${LOG_FILE} for more details" )
  else()
    message( STATUS "Finished configuring ${TARGET_FOLDER}" )
  endif()

  set( LOG_FILE      ${CMAKE_CURRENT_BINARY_DIR}/${TARGET_FOLDER}-build.log )
  message( STATUS "Building ${TARGET_FOLDER}...")
  execute_process(
                  COMMAND
                    make install
                  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${TARGET_FOLDER}
                  OUTPUT_FILE       ${LOG_FILE}
                  ERROR_FILE        ${LOG_FILE}
                  # COMMAND_ECHO      STDOUT
                  RESULT_VARIABLE   ${TARGET_FOLDER}_STATUS
                  )
  if ( ${TARGET_FOLDER}_STATUS AND NOT ${TARGET_FOLDER}_STATUS EQUAL 0 AND NOT EXISTS "${TAG_FILE}" )
    message( FATAL_ERROR "Failed to build ${TARGET_FOLDER}, see log file ${LOG_FILE} for more details" )
  else()
    message( STATUS "Finished building ${TARGET_FOLDER}" )
  endif()
else()
  message( STATUS "External ${TARGET_FOLDER} already built, skipping" )
endif()

################################################################################
##
## Jasper
##!TODO This is "failing" due to automake-1.14 missing. Make this a clean build
##
################################################################################
set( TARGET_FOLDER jasper-1.900.29 )
set( TAG_FILE      ${INTERNAL_GRIB2_PATH}/include/jasper/jasper.h )
if ( NOT EXISTS "${TAG_FILE}" )
  set( LOG_FILE      ${CMAKE_CURRENT_BINARY_DIR}/${TARGET_FOLDER}-configure.log )
  message( STATUS "Configuring ${TARGET_FOLDER}...")
  execute_process( 
                  COMMAND 
                    ./configure --prefix=${INTERNAL_GRIB2_PATH} --disable-shared
                  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${TARGET_FOLDER}
                  OUTPUT_FILE       ${LOG_FILE}
                  ERROR_FILE        ${LOG_FILE}
                  # COMMAND_ECHO      STDOUT
                  RESULT_VARIABLE   ${TARGET_FOLDER}_STATUS_CONF
                  )
  if ( ${TARGET_FOLDER}_STATUS_CONF AND NOT ${TARGET_FOLDER}_STATUS_CONF EQUAL 0 )
    message( FATAL_ERROR "Failed to configure ${TARGET_FOLDER}, see log file ${LOG_FILE} for more details" )
  else()
    message( STATUS "Finished configuring ${TARGET_FOLDER}" )
  endif()

  set( LOG_FILE      ${CMAKE_CURRENT_BINARY_DIR}/${TARGET_FOLDER}-build.log )
  message( STATUS "Building ${TARGET_FOLDER}...")
  execute_process(
                  COMMAND
                    make -r -i install
                  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${TARGET_FOLDER}
                  OUTPUT_FILE       ${LOG_FILE}
                  ERROR_FILE        ${LOG_FILE}
                  # COMMAND_ECHO      STDOUT
                  RESULT_VARIABLE   ${TARGET_FOLDER}_STATUS
                  )
  if ( ${TARGET_FOLDER}_STATUS AND NOT ${TARGET_FOLDER}_STATUS EQUAL 0 AND NOT EXISTS "${TAG_FILE}" )
    message( FATAL_ERROR "Failed to build ${TARGET_FOLDER}, see log file ${LOG_FILE} for more details" )
  else()
    message( STATUS "Finished building ${TARGET_FOLDER}" )
  endif()
else()
  message( STATUS "External ${TARGET_FOLDER} already built, skipping" )
endif()
list(POP_BACK CMAKE_MESSAGE_INDENT)