# https://cmake.org/cmake/help/latest/module/FindMPI.html#variables-for-locating-mpi
set( MPI_Fortran_COMPILER "{DM_FC}" )
set( MPI_C_COMPILER       "{DM_CC}" )

set( CMAKE_Fortran_COMPILER "{SFC}" )
set( CMAKE_C_COMPILER       "{SCC}" )

set( CMAKE_Fortran_FLAGS_INIT    "{SFC_FLAGS} {FFLAGS}" )
set( CMAKE_C_FLAGS_INIT          "{SCC_FLAGS} {CFLAGS}" )

# set( CMAKE_Fortran_FLAGS_DEBUG_INIT    "{{FCDEBUG}}" )
# set( CMAKE_Fortran_FLAGS_RELEASE_INIT  "{{FCOPTIM}}" )

# Project specifics now
set( WPS_MPI_Fortran_FLAGS  "{DM_FC_FLAGS}" )
set( WPS_MPI_C_FLAGS        "{DM_CC_FLAGS}" )
set( WPS_DEFINITIONS        "{CPPFLAGS}"    )
# set( WPS_M4_FLAGS           "{{M4_FLAGS}}"    )
# set( WPS_FCOPTIM            "{{FCOPTIM}}"     )
# set( WPS_FCNOOPT            "{{FCNOOPT}}"     )