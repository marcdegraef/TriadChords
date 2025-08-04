include("${CMAKE_CURRENT_LIST_DIR}/TriadLibTargets.cmake")


set(Triad_INCLUDE_DIRS "${CMAKE_CURRENT_LIST_DIR}/../../../include")
set(Triad_LIB_DIRS "${CMAKE_CURRENT_LIST_DIR}/../../../lib;${CMAKE_CURRENT_LIST_DIR}/../../../bin")

set(Triad_Fortran_COMPILER_NAME @Fortran_COMPILER_NAME@)
set(Triad_BUILD_SHARED_LIBS "@BUILD_SHARED_LIBS@")

if (Triad_Fortran_COMPILER_NAME MATCHES "gfortran.*")
  set(Triad_Fortran_RT_Libs gfortran @Triad_FORTRAN_SUPPORT_LIBS@)
  set(Triad_Fortran_Lib_Dir @GFortran_LIB_DIR@)
endif()
