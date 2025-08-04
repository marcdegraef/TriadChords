# cmake version, project name, language
if(${CMAKE_MAJOR_VERSION} VERSION_GREATER_EQUAL "3.1.0")
  cmake_policy(SET CMP0002 NEW)
  cmake_policy(SET CMP0054 NEW)
  cmake_policy(SET CMP0048 NEW)
endif()

if(${CMAKE_MAJOR_VERSION} VERSION_GREATER_EQUAL "3.12.0")
  cmake_policy(SET CMP0074 NEW)
endif()

if(${CMAKE_MAJOR_VERSION} VERSION_GREATER_EQUAL "3.13.0")
  cmake_policy(SET CMP0079 NEW)
endif()

cmake_minimum_required(VERSION 3.14.5)


if (NOT DEFINED CMP_SOURCE_DIR)
  set(CMP_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/Support/cmp)
endif()
include (${CMP_SOURCE_DIR}/cmpCMakeMacros.cmake )

# Next up, Set the version of EMsoftOO that is used through out the project.
# we continue the numbering from the original EMsoft package, so 6.0.0 is the
# first version of the object oriented code.
set(TriadChords_VER_MAJOR "3")
set(TriadChords_VER_MINOR "0")
set(TriadChords_VER_PATCH "0")

project(TriadChords LANGUAGES Fortran VERSION ${TriadChords_VER_MAJOR}.${TriadChords_VER_MINOR}.${TriadChords_VER_PATCH}.0)

cmpGenerateBuildDate(PROJECT_NAME TriadChords)
if(NOT "${TriadChords_RELEASE_TYPE}" STREQUAL "Official")
  string(REPLACE "/" "" TriadChords_VER_PATCH ${TriadChords_BUILD_DATE})
endif()

# --------------------------------------------------------------------
# Print out the current Git Infomation. This is useful to help debug the dashboards
execute_process(COMMAND ${GIT_EXECUTABLE} rev-parse --verify HEAD
                OUTPUT_VARIABLE TriadChords_GIT_HASH
                RESULT_VARIABLE did_run
                ERROR_VARIABLE git_error
                WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
                OUTPUT_STRIP_TRAILING_WHITESPACE
)
string(REPLACE "\n" "" TriadChords_GIT_HASH "${TriadChords_GIT_HASH}")

execute_process( # get abbreviated hash
  COMMAND ${GIT_EXECUTABLE} rev-parse --short HEAD
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  OUTPUT_VARIABLE TriadChords_SHORT_GIT_HASH
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

string(REPLACE "\n" "" TriadChords_SHORT_GIT_HASH "${TriadChords_SHORT_GIT_HASH}")

execute_process(COMMAND ${GIT_EXECUTABLE} log -1 --pretty='%cd' --date=format:%Y-%m-%d-%H:%M:%S
                OUTPUT_VARIABLE GVS_GIT_COMMIT_DATE
                RESULT_VARIABLE did_run
                ERROR_VARIABLE git_error
                WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
)
string(REPLACE "\n" "" GVS_GIT_COMMIT_DATE "${GVS_GIT_COMMIT_DATE}")
message(STATUS "${PROJECT_NAME}   Git Hash:${TriadChords_GIT_HASH} committed on ${GVS_GIT_COMMIT_DATE}")

# ---------- Setup output Directories -------------------------
if(NOT DEFINED CMAKE_LIBRARY_OUTPUT_DIRECTORY)
  set(CMAKE_LIBRARY_OUTPUT_DIRECTORY
    ${TriadChords_BINARY_DIR}/Bin
    CACHE PATH
    "Single Directory for all Libraries"
    )
endif()

# --------- Setup the Executable output Directory -------------
if(NOT DEFINED CMAKE_RUNTIME_OUTPUT_DIRECTORY)
  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY
    ${TriadChords_BINARY_DIR}/Bin
    CACHE PATH
    "Single Directory for all Executables."
    )
endif()

# --------- Setup the Executable output Directory -------------
if(NOT DEFINED CMAKE_ARCHIVE_OUTPUT_DIRECTORY)
  set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY
    ${TriadChords_BINARY_DIR}/Bin
    CACHE PATH
    "Single Directory for all static libraries."
    )
endif()

# set output paths for modules, archives, and executables
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/include)

# if build type not specified, default to release
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE "release")
endif()

# handle integer size
if(INT)
  message(STATUS "Configuring build for ${INT}-bit integers")
  add_definitions(-DINT${INT})
else()
  message(STATUS "Configuring build for 32-bit integers")
  add_definitions(-DINT32)
endif()

# handle real size
if(REAL)
  message(STATUS "Configuring build for ${REAL}-bit reals")
  add_definitions(-DREAL${REAL})
else()
  message(STATUS "Configuring build for 64-bit reals")
  add_definitions(-DREAL64)
endif()

message(STATUS "CMAKE_Fortran_COMPILER: ${CMAKE_Fortran_COMPILER}")

# Get the name of the Fortran compiler for easier decisions
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)


# compiler flags for gfortran
if(CMAKE_Fortran_COMPILER MATCHES "gfortran.*")

  if(BLAS)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fexternal-blas ${BLAS}")
    set(LIBS "${LIBS} blas")
    message(STATUS "Configuring build to use BLAS from ${BLAS}")
  endif()

  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -cpp -fopenmp")
  set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -C -fbacktrace")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -ffast-math")
endif()

# compiler flags for ifort
if(CMAKE_Fortran_COMPILER MATCHES Intel)

  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fpp -assume byterecl,realloc_lhs -heap-arrays -qopenmp")
  set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -C -traceback")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")

endif()

# compiler flags for Cray ftn
if(CMAKE_Fortran_COMPILER MATCHES Cray)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -h noomp")
  set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -g")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O3")
endif()

# library to archive (libtriads.a)
# add_library(triads src/TriadLib/mod_kinds.f90 src/TriadLib/mod_global.f90 src/TriadLib/mod_parameters.f90 src/TriadLib/mod_io.f90 src/TriadLib/program_mods/mod_TriadProgression.f90 src/TriadLib/mod_triads.f90)

# Remove leading or trailing whitespace
string(REGEX REPLACE "^ | $" "" LIBS "${LIBS}")

include(${PROJECT_SOURCE_DIR}/src/Source.cmake)

string(TIMESTAMP TriadChords_BUILD_TIMESTAMP "%Y-%m-%d %H:%M:%SZ" UTC)

message(STATUS "*******************************************************************************")
message(STATUS "* TriadChords Configuration Summary")
message(STATUS "* BUILD_TYPE: ${CMAKE_BUILD_TYPE}")
message(STATUS "* System: ${CMAKE_SYSTEM_NAME}")
message(STATUS "* Version: ${CMAKE_SYSTEM_VERSION}")
message(STATUS "* Processor: ${CMAKE_SYSTEM_PROCESSOR}")
message(STATUS "* TriadChords_VER_MAJOR: ${TriadChords_VER_MAJOR}")
message(STATUS "* TriadChords_VER_MINOR: ${TriadChords_VER_MINOR}")
message(STATUS "* TriadChords_VER_PATCH: ${TriadChords_VER_PATCH}")
message(STATUS "* TriadChords_VERSION_TWEAK: ${EMsoftOO_VERSION_TWEAK}")
message(STATUS "* TriadChords_VERSION: ${TriadChords_VERSION}")
message(STATUS "* CMAKE_Fortran_COMPILER: ${CMAKE_Fortran_COMPILER}")
message(STATUS "* TriadChords_SHORT_GIT_HASH: ${TriadChords_SHORT_GIT_HASH}")
message(STATUS "* TriadChords Git committed: ${GVS_GIT_COMMIT_DATE}")
message(STATUS "* TriadChords_BUILD_TIMESTAMP: ${TriadChords_BUILD_TIMESTAMP}")
