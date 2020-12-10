#include "MAPL_ErrLog.h"
module MAPL_ExtDataRule
   use yaFyaml
   use ESMF
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use MAPL_TimeStringConversion
   implicit none
   private

   type, public :: ExtDataRule
      character(:), allocatable :: file_template_key
      character(:), allocatable :: file_var
      real :: scaling
      real :: shift
      logical :: time_interpolation
      type(ESMF_Time), allocatable :: source_time(:)
      character(:), allocatable :: extrap_outside
      character(:), allocatable :: regrid_method
      character(:), allocatable :: refresh_time
      character(:), allocatable :: refresh_frequency
      character(:), allocatable :: refresh_offset
      character(:), allocatable :: vector_partner
      character(:), allocatable :: vector_component
      character(:), allocatable :: vector_file_partner
      contains
         procedure :: set_defaults
         procedure :: split_vector
         procedure :: append_from_yaml
   end type

contains

   subroutine set_defaults(this,unusable,rc)
      class(ExtDataRule), intent(inout), target :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status 
      _UNUSED_DUMMY(unusable)
      this%file_template_key=''
      this%file_var='missing_variable'
      this%scaling=0.0
      this%shift=0.0
      this%time_interpolation=.true.
      this%extrap_outside='none'
      this%regrid_method='BILINEAR'
      this%refresh_time="00"
      this%refresh_frequency="PT0S"
      this%refresh_offset="PT0S"
      if (allocated(this%source_time)) then 
         deallocate(this%source_time,stat=status)
         _VERIFY(status)
         allocate(this%source_time(0),stat=status)
         _VERIFY(status)
      end if
      _RETURN(_SUCCESS)
   end subroutine set_defaults

   recursive  subroutine append_from_yaml(rule,config,key,unusable,rc)
      class(ExtDataRule), intent(inout), target :: rule
      type(Configuration), intent(in) :: config
      character(len=*), intent(in) :: key
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      logical :: is_present
      integer :: status
      character(len=:), allocatable :: source_str
      integer :: idx
      type(Configuration) :: subcfg
      character(len=:), allocatable :: override_key, tempc
      logical :: templ
      real :: tempr
      _UNUSED_DUMMY(unusable)

      idx=index(key,"%")
      if (idx == 0) then
         subcfg=config%at(trim(key))
      else
         subcfg=config%at(trim(key(:idx-1)),trim(key(idx+1:)))
      end if

      call subcfg%get(override_key,"opts",default='',rc=status)
      _VERIFY(status)
      if (override_key/='') then
         call rule%append_from_yaml(config,override_key,rc=status)
         _VERIFY(status)
      end if

      if (allocated(tempc)) deallocate(tempc)
      call subcfg%get(tempc,"file_key",is_present=is_present,rc=status)
      _VERIFY(status)
      if (is_present) rule%file_template_key=tempc

      if (allocated(tempc)) deallocate(tempc)
      call subcfg%get(tempc,"var",is_present=is_present,rc=status)
      _VERIFY(status)
      if (is_present) rule%file_var=tempc

      if (allocated(tempc)) deallocate(tempc)
      call subcfg%get(tempc,"extrap",is_present=is_present,rc=status)
      _VERIFY(status)
      if (is_present) rule%extrap_outside=tempc

      call subcfg%get(tempr,"scale",is_present=is_present,rc=status)
      _VERIFY(status)
      if (is_present) rule%scaling=tempr

      call subcfg%get(tempr,"shift",is_present=is_present,rc=status)
      _VERIFY(status)
      if (is_present) rule%shift=tempr

      call subcfg%get(templ,"tinterp",is_present=is_present,rc=status)
      _VERIFY(status)
      if (is_present) rule%time_interpolation=templ

      if (allocated(tempc)) deallocate(tempc)
      call subcfg%get(tempc,"regrid",is_present=is_present,rc=status)
      _VERIFY(status)
      if (is_present) rule%regrid_method=tempc

      if (allocated(tempc)) deallocate(tempc)
      call subcfg%get(tempc,"upd_ref_time",is_present=is_present,rc=status)
      _VERIFY(status)
      if (is_present) rule%refresh_time=tempc

      if (allocated(tempc)) deallocate(tempc)
      call subcfg%get(tempc,"upd_freq",is_present=is_present,rc=status)
      _VERIFY(status)
      if (is_present) rule%refresh_frequency=tempc

      if (allocated(tempc)) deallocate(tempc)
      call subcfg%get(tempc,"upd_offset",is_present=is_present,rc=status)
      _VERIFY(status)
      if (is_present) rule%refresh_offset=tempc

      call subcfg%get(source_str,"source_time",is_present=is_present,rc=status)
      _VERIFY(status)
      if (is_present) then
         if (allocated(rule%source_time)) deallocate(rule%source_time)
         idx = index(source_str,',')
         _ASSERT(idx/=0,'invalid specification of source_time')
         allocate(rule%source_time(2))
         rule%source_time(1)=string_to_esmf_time(source_str(:idx-1))
         rule%source_time(2)=string_to_esmf_time(source_str(idx+1:))
      else 
         if (.not.allocated(rule%source_time)) allocate(rule%source_time(0))
      end if
     
      _RETURN(_SUCCESS)
   end subroutine append_from_yaml


   subroutine split_vector(this,original_key,ucomp,vcomp,unusable,rc)
      class(ExtDataRule), intent(in) :: this
      character(len=*), intent(in) :: original_key
      type(ExtDataRule), intent(inout) :: ucomp,vcomp
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: semi_pos
      character(len=:),allocatable :: uname,vname
    
      _UNUSED_DUMMY(unusable)

      semi_pos = index(this%file_var,";")
      _ASSERT(semi_pos > 0,"vector rule does not have 2 variables in the file_var")
      uname = this%file_var(1:semi_pos-1)
      vname = this%file_var(semi_pos+1:len_trim(this%file_var))
      ucomp = this
      vcomp = this
      semi_pos = index(original_key,";")
      ucomp%vector_partner = original_key(semi_pos+1:len_trim(original_key))
      vcomp%vector_partner = original_key(1:semi_pos-1)
      ucomp%file_var = uname
      vcomp%file_var = vname
      ucomp%vector_file_partner = vname
      vcomp%vector_file_partner = uname
      ucomp%vector_component = "EW"
      vcomp%vector_component = "NS"
      _RETURN(_SUCCESS)

   end subroutine split_vector

end module MAPL_ExtDataRule

module MAPL_ExtDataRuleMap
   use MAPL_ExtDataRule

#include "types/key_deferredLengthString.inc"
#define _value type(ExtDataRule)
#define _alt

#define _map ExtDataRuleMap
#define _iterator ExtDataRuleMapIterator

#include "templates/map.inc"

#undef _iterator
#undef _map

#undef _alt
#undef _value

end module MAPL_ExtDataRuleMap
