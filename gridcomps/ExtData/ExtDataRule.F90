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
      !logical :: allow_extrap
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
         procedure :: split_vector
   end type

   interface ExtDataRule
      module procedure new_ExtDataRule_from_yaml
   end interface

contains

   function new_ExtDataRule_from_yaml(config,unusable,rc) result(rule)
      type(Configuration), intent(in) :: config
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(ExtDataRule), target :: rule
      logical :: is_present
      integer :: status
      character(len=:), allocatable :: source_str
      integer :: idx
    
      _UNUSED_DUMMY(unusable)

      call config%get(rule%file_template_key,"file_key",default='',is_present=is_present,rc=status)
      _VERIFY(status)
      _ASSERT(is_present,"Missing file_template_key in ExtDataRule")

      call config%get(rule%file_var,"var",default='missing_variable',rc=status)
      _VERIFY(status)

      call config%get(rule%extrap_outside,"extrap",default='none',rc=status)
      _VERIFY(status)

      call config%get(rule%scaling,"scale",default=0.0,rc=status) 
      _VERIFY(status)

      call config%get(rule%shift,"shift",default=0.0,rc=status)
      _VERIFY(status)

      call config%get(rule%time_interpolation,"tinterp",default=.true.,rc=status)
      _VERIFY(status)

      call config%get(rule%regrid_method,"regrid",default='BILINEAR',rc=status)
      _VERIFY(status)

      call config%get(rule%refresh_time,"upd_ref_time",default='00',rc=status)
      _VERIFY(status)

      call config%get(rule%refresh_frequency,"upd_freq",default='PT0S',rc=status)
      _VERIFY(status)

      call config%get(rule%refresh_offset,"upd_offset",default='PT0S',rc=status)
      _VERIFY(status)

      call config%get(source_str,"source_time",default='',rc=status)
      _VERIFY(status)
      if (source_str /= '') then
         idx = index(source_str,',')
         _ASSERT(idx/=0,'invalid specification of source_time')
         allocate(rule%source_time(2))
         rule%source_time(1)=string_to_esmf_time(source_str(:idx-1))
         rule%source_time(2)=string_to_esmf_time(source_str(idx+1:))
      else 
         allocate(rule%source_time(0))
      end if

      _RETURN(_SUCCESS)
   end function new_ExtDataRule_from_yaml

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
