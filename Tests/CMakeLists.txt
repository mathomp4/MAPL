set (srcs
    ExtDataDriverGridComp.F90
    ExtDataRoot_GridComp.F90
    ExtDataDriver.F90 
    ExtDataDriverMod.F90 
    VarspecDescription.F90
    )

if (BUILD_WITH_FLAP)
  ecbuild_add_executable (TARGET ExtDataDriver.x SOURCES ${srcs})
  target_link_libraries (ExtDataDriver.x MAPL MAPL.gridcomps FLAP::FLAP ${MKL_LIBRARIES} ${OpenMP_Fortran_LIBRARIES})
  set_target_properties(ExtDataDriver.x PROPERTIES LINK_FLAGS "${OpenMP_Fortran_FLAGS}")
endif ()
