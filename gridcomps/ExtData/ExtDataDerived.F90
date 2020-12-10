#include "MAPL_ErrLog.h"
module MAPL_ExtDataDerived
   use yaFyaml
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   implicit none
   private

   type, public :: ExtDataDerived
      character(:), allocatable :: expression
      character(:), allocatable :: refresh_time
      character(:), allocatable :: refresh_frequency
      character(:), allocatable :: refresh_offset
      character(:), allocatable :: refresh_template !temporary to get working
      contains
         procedure :: display
         procedure :: append_from_yaml
         procedure :: set_defaults
   end type

contains

   subroutine set_defaults(this,unusable,rc)
      class(ExtDataDerived), intent(inout), target :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      _UNUSED_DUMMY(unusable)
      this%expression=''
      this%refresh_time='0'
      this%refresh_frequency='PT0S'
      this%refresh_offset='PT0S'
      _RETURN(_SUCCESS)
   end subroutine set_defaults

   recursive  subroutine append_from_yaml(rule,config,key,unusable,rc)
      class(ExtDataDerived), intent(inout), target :: rule
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
      call subcfg%get(tempc,"function",is_present=is_present,rc=status)
      _VERIFY(status)
      if (is_present) rule%expression=tempc

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

      _RETURN(_SUCCESS)
   end subroutine append_from_yaml

   subroutine display(this)
      class(ExtDataDerived) :: this
      write(*,*)"function: ",trim(this%expression)
   end subroutine display
 
end module MAPL_ExtDataDerived

module MAPL_ExtDataDerivedMap
   use MAPL_ExtDataDerived

#include "types/key_deferredLengthString.inc"
#define _value type(ExtDataDerived)
#define _alt

#define _map ExtDataDerivedMap
#define _iterator ExtDataDerivedMapIterator

#include "templates/map.inc"

#undef _iterator
#undef _map

#undef _alt
#undef _value

end module MAPL_ExtDataDerivedMap
