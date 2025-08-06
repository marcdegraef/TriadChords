set(APP_DIR "${TriadChords_SOURCE_DIR}/src/Programs")
set(TMPLT_DIR "${TriadChords_SOURCE_DIR}/NamelistTemplates")
set(LIB_SEARCH_DIRS ${CMAKE_LIBRARY_OUTPUT_DIRECTORY} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})

  set(EXE_LINK_LIBRARIES
          TriadLib
  )

  Add_TriadChords_Executable(TARGET TriadProgression
                             SOURCES ${APP_DIR}/TriadProgression.f90
                             LINK_LIBRARIES ${EXE_LINK_LIBRARIES}
                             TEMPLATE ${TMPLT_DIR}/TriadProgression.template
                             SOLUTION_FOLDER src/Programs
                             INSTALL_PROGRAM TRUE
                             INCLUDE_DIRS ${TriadChordsLib_BINARY_DIR}
                             )

  Add_TriadChords_Executable(TARGET Triad
                             SOURCES ${APP_DIR}/Triad.f90
                             LINK_LIBRARIES ${EXE_LINK_LIBRARIES}
                             SOLUTION_FOLDER src/Programs
                             INSTALL_PROGRAM TRUE
                             INCLUDE_DIRS ${TriadChordsLib_BINARY_DIR}
                             )
