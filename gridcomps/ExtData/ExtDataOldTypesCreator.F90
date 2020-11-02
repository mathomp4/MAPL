#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"

module MAPL_ExtDataOldTypesCreator
   use ESMF
   use MAPL_BaseMod
   use yafYaml
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use MAPL_ExtDataTypeDef
   use MAPL_ExtDataYamlConfig
   use MAPL_ExtDataFileStream
   use MAPL_ExtDataFileStreamMap
   use MAPL_ExtDataRule
   use MAPL_ExtDataRuleMap
   use MAPL_ExtDataDerived
   use MAPL_ExtDataDerivedMap
   use MAPL_RegridderSpecMod
   use MAPL_ExtDataAbstractFileHandler
   use MAPL_ExtDataSimpleFileHandler
   use MAPL_ExtDataClimFileHandler
   implicit none
   public :: ExtDataOldTypesCreator

   type, extends(ExtDataYamlConfig) :: ExtDataOldTypesCreator
      private
      contains
         procedure :: fillin_primary
         procedure :: fillin_derived
   end type ExtDataOldTypesCreator

   interface ExtDataOldTypesCreator
      module procedure :: new_ExtDataOldTypesCreator
   end interface

   contains

   function new_ExtDataOldTypesCreator(config_file,current_time,unusable,rc ) result(ExtDataObj)
      character(len=*), intent(in) :: config_file
      type(ESMF_Time), intent(in) :: current_time
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(ExtDataOldTypesCreator) :: ExtDataObj

      integer :: status

      _UNUSED_DUMMY(unusable)
      call ExtDataYamlConfig(ExtDataObj%ExtDataYamlConfig,config_file,current_time,rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)
   end function new_ExtDataOldTypesCreator

   
   subroutine fillin_primary(this,item_name,primary_item,time,clock,unusable,rc)
      class(ExtDataOldTypesCreator), intent(inout) :: this
      character(len=*), intent(in) :: item_name
      type(PrimaryExport), intent(inout) :: primary_item
      type(ESMF_Time), intent(inout) :: time
      type(ESMF_Clock), intent(inout) :: clock
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(ExtDataRule), pointer :: rule
      type(ExtDataFileStream),  pointer :: dataset
      type(ExtDataSimpleFileHandler) :: simple_handler
      type(ExtDataClimFileHandler) :: clim_handler
      integer :: status, semi_pos
      logical :: disable_interpolation

      rule => this%rule_map%at(trim(item_name))
      primary_item%isVector = allocated(rule%vector_partner)
      ! name and file var
      primary_item%name = trim(item_name)
      if (primary_item%isVector) then
         primary_item%vartype = MAPL_VectorField
         primary_item%vcomp1 = trim(item_name)
         primary_item%vcomp2 = trim(rule%vector_partner)
         primary_item%var = rule%file_var
         primary_item%fcomp1 = rule%file_var
         primary_item%fcomp2 = rule%vector_file_partner
         primary_item%fileVars%itemType = ItemTypeVector
         primary_item%fileVars%xname  = trim(rule%file_var)
         primary_item%fileVars%yname  = trim(rule%vector_file_partner)
      else
         primary_item%vartype = MAPL_FieldItem
         primary_item%vcomp1 = trim(item_name)
         primary_item%var = rule%file_var
         primary_item%fcomp1 = rule%file_var
         primary_item%fileVars%itemType = ItemTypeScalar
         primary_item%fileVars%xname  = trim(rule%file_var)
      end if
      
      ! units
      primary_item%units = ''
      ! climatology
      primary_item%cyclic = 'n' !ESMF_UtilStringLowerCase(trim(rule%climatology))
      ! regrid method
      if (trim(rule%regrid_method) == "REGRID_METHOD_BILINEAR") then
         primary_item%trans = REGRID_METHOD_BILINEAR
      else if (trim(rule%regrid_method) == "REGRID_METHOD_CONSERVE") then
         primary_item%trans = REGRID_METHOD_CONSERVE
      else if (trim(rule%regrid_method) == "REGRID_METHOD_VOTE") then
         primary_item%trans = REGRID_METHOD_VOTE
      else if (index(rule%regrid_method,"REGRID_METHOD_FRACTION;")>0) then
         semi_pos = index(rule%regrid_method,";")
         read(rule%regrid_method(semi_pos+1:),*) primary_item%fracVal
         primary_item%trans = REGRID_METHOD_FRACTION
      else 
         _ASSERT(.false.,"Invalid regridding method")
      end if

      ! newstuff
      if (trim(rule%extrap_outside) =="clim") then
         primary_item%cycling=.true.
      else if (trim(rule%extrap_outside) == "persist_closest") then
         primary_item%persist_closest=.true.
      else if (trim(rule%extrap_outside) == "none") then
         primary_item%cycling=.false.
         primary_item%persist_closest=.false.
      end if
      allocate(primary_item%source_time,source=rule%source_time)
      ! new refresh
      call primary_item%update_freq%create_from_parameters(rule%refresh_time, &
           rule%refresh_frequency, rule%refresh_offset, time, clock, __RC__)

      disable_interpolation =  .not.rule%time_interpolation 

      call primary_item%modelGridFields%comp1%set_parameters(offset=rule%shift,scale_factor=rule%scaling,disable_interpolation=disable_interpolation)
      call primary_item%modelGridFields%comp2%set_parameters(offset=rule%shift,scale_factor=rule%scaling,disable_interpolation=disable_interpolation)
      call primary_item%modelGridFields%auxiliary1%set_parameters(offset=rule%shift,scale_factor=rule%scaling, disable_interpolation=disable_interpolation)
      call primary_item%modelGridFields%auxiliary2%set_parameters(offset=rule%shift,scale_factor=rule%scaling, disable_interpolation=disable_interpolation)

      ! file_template
      primary_item%isConst = .false.
      if (index(rule%file_template_key,"/dev/null")==0) then
         dataset => this%file_stream_map%at(trim(rule%file_template_key))
         primary_item%file = dataset%file_template
      else
         primary_item%file = rule%file_template_key
      end if
      if (index(primary_item%file,'/dev/null') /= 0) then
         primary_item%isConst = .true.
         semi_pos = index(primary_item%file,':')
         if (semi_pos > 0) then
            read(primary_item%file(semi_pos+1:),*)primary_item%const
         else
            primary_item%const=0.0
         end if
      else
         if (primary_item%cycling) then
            call clim_handler%initialize(dataset,__RC__)
            allocate(primary_item%filestream,source=clim_handler)
         else
            call simple_handler%initialize(dataset,persist_closest=primary_item%persist_closest,__RC__)
            allocate(primary_item%filestream,source=simple_handler)
         end if
      end if

      !legacy
      if (.not.primary_item%isConst) then
         primary_item%frequency=dataset%frequency
         primary_item%reff_time=dataset%reff_time
      end if

      _RETURN(_SUCCESS)

   end subroutine fillin_primary

   subroutine fillin_derived(this,item_name,derived_item,time,clock,unusable,rc)
      class(ExtDataOldTypesCreator), intent(inout) :: this
      character(len=*), intent(in) :: item_name
      type(DerivedExport), intent(inout) :: derived_item
      type(ESMF_Time), intent(inout) :: time
      type(ESMF_Clock), intent(inout) :: clock
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(ExtDataDerived), pointer :: rule
      integer :: status

      rule => this%derived_map%at(trim(item_name))
      derived_item%name = trim(item_name)
      derived_item%expression = rule%expression
      call derived_item%update_freq%create_from_parameters(rule%refresh_time, &
           rule%refresh_frequency, rule%refresh_offset, time, clock,__RC__)
      !derived_item%refresh_template = rule%refresh_template
      derived_item%masking=.false.
      if (index(derived_item%expression,"mask") /= 0 ) then
         derived_item%masking=.true.
      end if

      _RETURN(_SUCCESS)
 
   end subroutine fillin_derived

end module MAPL_ExtDataOldTypesCreator
