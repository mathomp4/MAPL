#include "MAPL_ErrLog.h"
module MAPL_ExtDataConfig
   use ESMF
   use yaFyaml
   use gFTL_StringVector
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use MAPL_ExtDataFileStream
   use MAPL_ExtDataFileStreamMap
   use MAPL_ExtDataRule
   use MAPL_ExtDataRuleMap
   use MAPL_ExtDataDerived
   use MAPL_ExtDataDerivedMap
   implicit none
   private

   type, public :: ExtDataConfig
      integer :: debug
      type(ExtDataRuleMap) :: rule_map
      type(ExtDataDerivedMap) :: derived_map
      type(ExtDataFileStreamMap) :: file_stream_map
      
      contains
         procedure :: name_in_config
         procedure :: count_number
         procedure :: get_debug_flag
         procedure :: new_ExtDataConfig_from_yaml
   end type

contains

   recursive subroutine new_ExtDataConfig_from_yaml(ext_config,config_file,current_time,unusable,rc) 
      !type(ExtDataConfig), intent(inout), target :: ext_config
      class(ExtDataConfig), intent(inout), target :: ext_config
      character(len=*), intent(in) :: config_file
      type(ESMF_Time), intent(in) :: current_time
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(Parser)              :: p
      type(Configuration) :: config,subcfg, ds_config, rule_config, derived_config
      type(ConfigurationIterator) :: iter
      character(:), pointer :: key
      type(ExtDataFileStream) :: ds
      type(ExtDataDerived) :: derived
      type(ExtDataRule) :: rule,ucomp,vcomp
      integer :: status, semi_pos
      character(len=:), allocatable :: uname,vname
      type(FileStream) :: fstream
      type(StringVector) :: subconfigs
      logical :: is_present
      integer :: i

      _UNUSED_DUMMY(unusable)

      p = Parser('core')
      fstream=FileStream(config_file)
      config = p%load(fstream)
      call fstream%close()

      subconfigs = config%at("subconfigs")
      do i=1,subconfigs%size()
         call new_ExtDataConfig_from_yaml(ext_config,subconfigs%at(i),current_time,rc=status)
         _VERIFY(status)
      enddo

      ds_config = config%at("data_sets")
      !_ASSERT(.not.ds_config%is_none(),"data_sets key not found in ExtData rc file")
      rule_config = config%at("rules")
      !_ASSERT(.not.rule_config%is_none(),"rules key not found in ExtData rc file")
      derived_config = config%at("derived")

      iter = ds_config%begin()
      do while (iter /= ds_config%end())
         key => iter%key()
         subcfg = iter%value()
         ds = ExtDataFileStream(subcfg,current_time)
         call ext_config%file_stream_map%insert(trim(key),ds)
         call iter%next()
      enddo

      iter = rule_config%begin()
      do while (iter /= rule_config%end())
         key => iter%key()
         subcfg = iter%value()
         rule = ExtDataRule(subcfg,rc=status)
         _VERIFY(status)
         semi_pos = index(key,";")
         if (semi_pos > 0) then
            call rule%split_vector(key,ucomp,vcomp,rc=status)
            uname = key(1:semi_pos-1)
            vname = key(semi_pos+1:len_trim(key))
            call ext_config%rule_map%insert(trim(uname),ucomp)
            call ext_config%rule_map%insert(trim(vname),vcomp)
         else
            call ext_config%rule_map%insert(trim(key),rule)
         end if
         call iter%next()
      enddo

      if (.not.derived_config%is_none()) then
         iter = derived_config%begin()
         do while (iter /= derived_config%end())
            key => iter%key()
            subcfg = iter%value()
            derived = ExtDataDerived(subcfg,rc=status)
            _VERIFY(status)
            call ext_config%derived_map%insert(trim(key),derived)
            call iter%next()
         enddo
      end if

      call config%get(ext_config%debug,"debug",default=0,rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)
   end subroutine new_ExtDataConfig_from_yaml

   logical function name_in_config(this,item_name,unusable,rc)
      class(ExtDataConfig), intent(inout) :: this
      character(len=*), intent(in) :: item_name
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      logical :: rule_present
      logical :: derived_present
      type(ExtDataRule), pointer :: rule
      type(ExtDataDerived), pointer :: derived

      rule => this%rule_map%at(trim(item_name))
      if (associated(rule)) then
         name_in_config = .true.
         _RETURN(_SUCCESS)
      end if
      derived => this%derived_map%at(trim(item_name))
      if (associated(derived)) then
         name_in_config = .true.
         _RETURN(_SUCCESS)
      end if
      name_in_config = .false.
      _RETURN(_SUCCESS)
   end function name_in_config

   subroutine count_number(this, item_name,primary_number,derived_number,unusable,rc) 
      class(ExtDataConfig), intent(inout) :: this
      integer, intent(out) :: primary_number,derived_number
      character(len=*), intent(in) :: item_name
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      type(ExtDataRule), pointer :: rule
      type(ExtDataDerived), pointer :: derived

      primary_number=0
      derived_number=0
      rule => this%rule_map%at(trim(item_name))
      if (associated(rule)) then
         if (allocated(rule%vector_component)) then
            if (rule%vector_component=='EW') then
               primary_number=2
            else if (rule%vector_component=='NS') then
               primary_number=0
            end if
         else
            primary_number=1
         end if
      end if
      derived => this%derived_map%at(trim(item_name))
      if (associated(derived)) then
         derived_number = 1
      end if
      _RETURN(_SUCCESS)
   end subroutine count_number
 
   integer function get_debug_flag(this)
      class(ExtDataConfig), intent(inout) :: this
      get_debug_flag=this%debug
   end function get_debug_flag 

end module MAPL_ExtDataConfig
