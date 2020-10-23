esma_set_this (OVERRIDE MAPL.gridcomps)

set (srcs
     ExtDataFileStream.F90
     ExtDataRule.F90
     ExtDataDerived.F90
     ExtDataYamlConfig.F90
     ExtDataGridCompNG.F90
     TimeStringConversion.F90
     ExtDataTypeDef.F90
     ExtDataOldTypesCreator.F90
     ExtDataBracket.F90
     ExtDataUpdatePointer.F90
     ExtDataAbstractFileHandler.F90
     ExtDataClimFileHandler.F90
     ExtDataSimpleFileHandler.F90
     ExtDataNode.F90
    )

esma_add_library (${this} SRCS ${srcs} DEPENDENCIES MAPL.shared MAPL.base)
target_link_libraries (${this} PUBLIC gftl gftl-shared ${ESMF_LIBRARIES}
                               PRIVATE OpenMP::OpenMP_Fortran MPI::MPI_Fortran)
target_include_directories (${this} PUBLIC ${INC_ESMF} ${INC_NETCDF}
          $<BUILD_INTERFACE:${MAPL_SOURCE_DIR}/include>)

set_target_properties (${this} PROPERTIES Fortran_MODULE_DIRECTORY ${include_${this}})
