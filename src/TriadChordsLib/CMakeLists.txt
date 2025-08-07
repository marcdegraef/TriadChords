project(TriadLib)

# include (${CMP_SOURCE_DIR}/cmpCMakeMacros.cmake)

#------------------------------------------------------------------------------
# Setup all the source files
set(TriadLib_SRCS

  ${TriadLib_SOURCE_DIR}/mod_kinds.f90
  ${TriadLib_SOURCE_DIR}/mod_global.f90
  ${TriadLib_SOURCE_DIR}/mod_parameters.f90
  ${TriadLib_SOURCE_DIR}/mod_io.f90
  ${TriadLib_SOURCE_DIR}/mod_triads.f90
  ${TriadLib_SOURCE_DIR}/mod_bmp.f90
  ${TriadLib_SOURCE_DIR}/mod_tif.f90
  ${TriadLib_SOURCE_DIR}/mod_lzw.f90
  ${TriadLib_SOURCE_DIR}/mod_image.f90
  ${TriadLib_SOURCE_DIR}/mod_image_io.f90

  ${TriadLib_SOURCE_DIR}/program_mods/mod_TriadProgression.f90
  ${TriadLib_SOURCE_DIR}/program_mods/mod_TriadPlots.f90

)

if (Fortran_COMPILER_NAME MATCHES "ifort.*")
  set(CMAKE_USE_IFORT "") 
  set(CMAKE_USE_GFORTRAN "! ") 
else()
  set(CMAKE_USE_IFORT "! ") 
  set(CMAKE_USE_GFORTRAN "") 
endif()

add_library(TriadLib ${LIB_TYPE} ${TriadLib_SRCS})

# target_link_libraries(EMsoftOOLib
# )

if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  target_link_libraries(TriadLib
                  gomp
  )
endif()

set_target_properties (TriadLib PROPERTIES
                  LINKER_LANGUAGE Fortran
                  FOLDER TriadPublic
                  )

# On Windows we need a few extra compile and linker flags. By making these PUBLIC they should
# flow down to every dependent target
if(WIN32)
  target_compile_options(TriadLib PUBLIC
                          $<$<COMPILE_LANGUAGE:Fortran>:/assume:byterecl>
                          $<$<COMPILE_LANGUAGE:Fortran>:/assume:nounderscore>
                          $<$<COMPILE_LANGUAGE:Fortran>:/Qopenmp>
                          $<$<COMPILE_LANGUAGE:Fortran>:/Qdiag-disable:11082>
                          $<$<COMPILE_LANGUAGE:Fortran>:/Qdiag-disable:7025>
                          $<$<COMPILE_LANGUAGE:Fortran>:/Qip>
                          $<$<COMPILE_LANGUAGE:Fortran>:/traceback>
                          #$<$<COMPILE_LANGUAGE:Fortran>:/warn:all>
                        #  $<$<COMPILE_LANGUAGE:Fortran>:/heap-arrays>
                          )
  target_link_options(TriadLib PUBLIC /STACK:2000000000 /machine:x64)
else()
  if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
    # First, Flags that should be used under any build condition
    target_compile_options(TriadLib PUBLIC $<$<COMPILE_LANGUAGE:Fortran>:-fopenmp>
        $<$<COMPILE_LANGUAGE:Fortran>:-fno-sign-zero>
        $<$<COMPILE_LANGUAGE:Fortran>:-fno-f2c>
        $<$<COMPILE_LANGUAGE:Fortran>:-std=gnu>
        $<$<COMPILE_LANGUAGE:Fortran>:-fbacktrace>
    )
    # Next, DEBUG compiler options.
    target_compile_options(TriadLib PUBLIC $<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Debug>>:-g>
                          $<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Debug>>:-fcheck=bounds>
                          $<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Debug>>:-O0>
    )
    # Next, RELEASE compiler options.
    target_compile_options(TriadLib PUBLIC $<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Release>>:-funroll-all-loops>
                          $<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Release>>:-O3>
    )

  elseif (Fortran_COMPILER_NAME MATCHES "ifort.*")
  # First, Flags that should be used under any build condition
    target_compile_options(TriadLib PUBLIC
                                            #$<$<COMPILE_LANGUAGE:Fortran>:-heap-arrays>
                                            $<$<COMPILE_LANGUAGE:Fortran>:-qopenmp>
                                            $<$<COMPILE_LANGUAGE:Fortran>:-assume byterecl>
                                            $<$<COMPILE_LANGUAGE:Fortran>:/assume:nounderscore>
                                            $<$<COMPILE_LANGUAGE:Fortran>:-stand f08 -diag-disable=7025>
                                            $<$<COMPILE_LANGUAGE:Fortran>:/traceback>
    )
    # Next, DEBUG compiler options.
    target_compile_options(TriadLib PUBLIC $<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Debug>>:-debug all>
                                           $<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Debug>>:-O0>
                                           $<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Debug>>:-g>
    )
    # Next, RELEASE compiler options.
    target_compile_options(TriadLib PUBLIC $<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Release>>:-O3>)
  else()
    message(WARNING "Fortran compiler not recognized. ${Fortran_COMPILER_NAME}")
  endif()
endif()


#-------------------------------------------------------------------------------
# This sets up the two variables install_dir and lib_install_dir
TriadChords_SetupInstallDirs()

#if(BUILD_SHARED_LIBS)
  INSTALL(TARGETS TriadLib
    COMPONENT Applications
    EXPORT TriadLibTargets
    RUNTIME DESTINATION ${install_dir}
    LIBRARY DESTINATION ${lib_install_dir}
    ARCHIVE DESTINATION ${lib_install_dir}
    BUNDLE DESTINATION "."
  )
#endif()

if(APPLE AND BUILD_SHARED_LIBS)
  set(QAB_INSTALL_DEST "lib")
  set(osx_app_name "libTriadLib.dylib")
  get_property(EMsoftOOBinaryDir GLOBAL PROPERTY TriadBinaryDir)
  set(OSX_MAKE_STANDALONE_BUNDLE_CMAKE_SCRIPT "${Triad_BINARY_DIR}/TriadLib/TriadLib_CompleteTool.cmake")
  set(OPTIMIZE_BUNDLE_SHELL_SCRIPT            "${Triad_BINARY_DIR}/TriadLib/TriadLib_OptimizeTool.sh")
  set(PROJECT_INSTALL_DIR "lib")
  configure_file("${CMP_OSX_TOOLS_SOURCE_DIR}/CompleteTool.cmake.in"
                "${OSX_MAKE_STANDALONE_BUNDLE_CMAKE_SCRIPT}" @ONLY IMMEDIATE)
  configure_file("${CMP_OSX_TOOLS_SOURCE_DIR}/CompleteTool.sh.in"
                "${OPTIMIZE_BUNDLE_SHELL_SCRIPT}" @ONLY IMMEDIATE)
  #install(SCRIPT "${OSX_MAKE_STANDALONE_BUNDLE_CMAKE_SCRIPT}" COMPONENT ${QAB_COMPONENT})
endif()


# --------------------------------------------------------------------
include(CMakePackageConfigHelpers)

write_basic_package_version_file(
  "${CMAKE_CURRENT_BINARY_DIR}/TriadChords/TriadLibTargetsConfigVersion.cmake"
  VERSION ${TriadChords_VERSION}
  COMPATIBILITY AnyNewerVersion
)
#if(BUILD_SHARED_LIBS)
  export(EXPORT TriadLibTargets
    FILE "${CMAKE_CURRENT_BINARY_DIR}/TriadChords/TriadLibTargets.cmake"
    NAMESPACE Triad::
  )
#endif()

configure_file(cmake/TriadChordsConfig.cmake
  "${CMAKE_CURRENT_BINARY_DIR}/TriadChords/TriadChordsConfig.cmake"
  @ONLY
)

set(ConfigPackageLocation ${lib_install_dir}/cmake/TriadChords)

#if(BUILD_SHARED_LIBS)
  install(EXPORT TriadLibTargets
    FILE
      TriadLibTargets.cmake
    NAMESPACE
      Triad::
    DESTINATION
      ${ConfigPackageLocation}
  )

#endif()

install(
  FILES
    "${CMAKE_CURRENT_BINARY_DIR}/TriadChords/TriadChordsConfig.cmake"
    "${CMAKE_CURRENT_BINARY_DIR}/TriadChords/TriadLibTargetsConfigVersion.cmake"
  DESTINATION
    ${ConfigPackageLocation}
  COMPONENT
    Devel
)
