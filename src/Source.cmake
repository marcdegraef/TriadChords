

set_property(GLOBAL PROPERTY TriadChords_PACKAGE_DEST_PREFIX ".")
# -----------------------------------------------------------------------
#
# -----------------------------------------------------------------------

get_property(TriadChords_PACKAGE_DEST_PREFIX GLOBAL PROPERTY TriadChords_PACKAGE_DEST_PREFIX)

include("${TriadChords_SOURCE_DIR}/src/TriadChords_Functions.cmake")

message(STATUS "TriadChords_SOURCE_DIR: ${TriadChords_SOURCE_DIR} ")

add_subdirectory(${PROJECT_SOURCE_DIR}/src/TriadChordsLib ${PROJECT_BINARY_DIR}/TriadChordsLib)

set(MODALITY_DIRS
    Programs
)

# -----------------------------------------------------------------------
# Establish which modalities are going to be compiled
# -----------------------------------------------------------------------
foreach(MODALITY ${MODALITY_DIRS})
  option(TriadChords_ENABLE_${MODALITY} "Build sources and programs related to ${MODALITY}" ON)
endforeach()

# -----------------------------------------------------------------------
# Add the executables
# -----------------------------------------------------------------------
foreach(MODALITY ${MODALITY_DIRS})
  if( "${TriadChords_ENABLE_${MODALITY}}" STREQUAL "ON" )
    message(STATUS "TriadChords: Enabling public ${MODALITY} Modality")
    add_subdirectory( ${PROJECT_SOURCE_DIR}/src/${MODALITY} ${PROJECT_BINARY_DIR}/${MODALITY})
  endif()
endforeach()

