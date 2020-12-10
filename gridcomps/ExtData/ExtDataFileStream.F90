#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module MAPL_ExtDataFileStream
   use ESMF
   use yaFyaml
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use MAPL_TimeStringConversion
   use MAPL_ExtDataCollectionMod
   use MAPL_CollectionVectorMod
   use MAPL_ExtDataCollectionManagerMod
   use MAPL_FileMetadataUtilsMod
   use MAPL_StringTemplate
   implicit none
   private

   type, public :: ExtDataFileStream
      character(:), allocatable :: file_template
      type(ESMF_TimeInterval) :: frequency
      type(ESMF_Time) :: reff_time
      integer :: collection_id
      type(ESMF_Time), allocatable :: valid_range(:)
      type(FileMetaData) :: metadata
      contains
         procedure :: detect_metadata
         procedure :: fill_from_yaml
   end type

contains

   subroutine fill_from_yaml(this,config,current_time,unusable,rc) 
      class(ExtDataFileStream), target, intent(inout) :: this
      type(Configuration), intent(in) :: config
      type(ESMF_Time), intent(in) :: current_time
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      !type(ExtDataFileStream), target :: data_set
      integer :: status
      integer :: last_token
      integer :: iyy,imm,idd,ihh,imn,isc,idx
      character(len=2) :: token
      character(len=:), allocatable :: file_frequency, file_reff_time,range_str
      logical :: is_present

      _UNUSED_DUMMY(unusable)

      call config%get(this%file_template,"tmpl",default='',is_present=is_present,rc=status)
      _VERIFY(status)
      call config%get(file_frequency,"freq",default='',rc=status)
      _VERIFY(status)
      if (file_frequency /= '') then
         this%frequency = string_to_esmf_timeinterval(file_frequency)
      else
         last_token = index(this%file_template,'%',back=.true.)
         if (last_token.gt.0) then
            token = this%file_template(last_token+1:last_token+2)
            select case(token)
            case("y4")
               call ESMF_TimeIntervalSet(this%frequency,yy=1,__RC__)
            case("m2")
               call ESMF_TimeIntervalSet(this%frequency,mm=1,__RC__)
            case("d2")
               call ESMF_TimeIntervalSet(this%frequency,d=1,__RC__)
            case("h2")
               call ESMF_TimeIntervalSet(this%frequency,h=1,__RC__)
            case("n2")
               call ESMF_TimeIntervalSet(this%frequency,m=1,__RC__)
            end select
         else
            ! couldn't find any tokens so all the data must be on one file
            call ESMF_TimeIntervalSet(this%frequency,__RC__)
         end if
      end if

      call config%get(file_reff_time,"ref_time",default='',rc=status)
      _VERIFY(status)
      if (file_reff_time /= '') then
         this%reff_time = string_to_esmf_time(file_reff_time)
      else
         last_token = index(this%file_template,'%',back=.true.)
         if (last_token.gt.0) then
            call ESMF_TimeGet(current_time, yy=iyy, mm=imm, dd=idd,h=ihh, m=imn, s=isc  ,__RC__)
            token = this%file_template(last_token+1:last_token+2)
            select case(token)
            case("y4")
               call ESMF_TimeSet(this%reff_time,yy=iyy,mm=1,dd=1,h=0,m=0,s=0,__RC__)
            case("m2")
               call ESMF_TimeSet(this%reff_time,yy=iyy,mm=imm,dd=1,h=0,m=0,s=0,__RC__)
            case("d2")
               call ESMF_TimeSet(this%reff_time,yy=iyy,mm=imm,dd=idd,h=0,m=0,s=0,__RC__)
            case("h2")
               call ESMF_TimeSet(this%reff_time,yy=iyy,mm=imm,dd=idd,h=ihh,m=0,s=0,__RC__)
            case("n2")
               call ESMF_TimeSet(this%reff_time,yy=iyy,mm=imm,dd=idd,h=ihh,m=imn,s=0,__RC__)
            end select
         else
            this%reff_time = current_time
         end if
      end if

      call config%get(range_str,"valid_range",default='',rc=status)
      _VERIFY(status)
      if (range_str /= '') then
         idx = index(range_str,',')
         _ASSERT(idx/=0,'invalid specification of time range')
         if (allocated(this%valid_range)) deallocate(this%valid_range)
         allocate(this%valid_range(2))
         this%valid_range(1)=string_to_esmf_time(range_str(:idx-1))
         this%valid_range(2)=string_to_esmf_time(range_str(idx+1:))
         call ESMF_TimeGet(this%reff_time,yy=iyy,mm=imm,dd=idd,h=ihh,m=imn,__RC__)
         call ESMF_TimeGet(this%valid_range(1),yy=iyy,__RC__)
         call ESMF_TimeSet(this%reff_time,yy=iyy,mm=imm,dd=idd,h=ihh,m=imn,__RC__)
      end if
      this%collection_id = MAPL_ExtDataAddCollection(this%file_template)

      _RETURN(_SUCCESS)

   end subroutine fill_from_yaml

   subroutine detect_metadata(this,metadata_out,time,get_range,rc)
      class(ExtDataFileStream), intent(inout) :: this
      type(FileMetadataUtils), intent(inout) :: metadata_out
      type(ESMF_Time),          intent(in)  :: time
      logical, optional, intent(in)  :: get_range
      integer, optional, intent(out) :: rc

      logical :: get_range_      
      type(MAPLExtDataCollection), pointer :: collection
      type(FileMetadataUtils), pointer :: metadata
      type(ESMF_Time), allocatable :: time_series(:)
      integer :: status
      character(len=ESMF_MAXPATHLEN) :: filename

      if (present(get_range)) then
         get_range_ = get_range
      else
         get_range_ = .false.
      end if

      collection => ExtDataCollections%at(this%collection_id)
      if (get_range_ .and. (.not.allocated(this%valid_range))) then
         if (index('%',this%file_template) == 0) then
            metadata => collection%find(this%file_template)
            call metadata%get_time_info(timeVector=time_series,__RC__)
            allocate(this%valid_range(2))
            this%valid_range(1)=time_series(1)
            this%valid_range(2)=time_series(size(time_series))
         end if
      end if

      if (get_range_) then
         call fill_grads_template(filename,this%file_template,time=this%valid_range(1),__RC__)
      else
         call fill_grads_template(filename,this%file_template,time=time,__RC__)
      end if
      metadata => collection%find(filename,__RC__)
      metadata_out = metadata
      _RETURN(_SUCCESS)

   end subroutine detect_metadata

end module MAPL_ExtDataFileStream

module MAPL_ExtDataFileStreamMap
   use MAPL_ExtDataFileStream

#include "types/key_deferredLengthString.inc"
#define _value type(ExtDataFileStream)
#define _alt

#define _map ExtDataFileStreamMap
#define _iterator ExtDataFileStreamMapIterator

#include "templates/map.inc"

#undef _iterator
#undef _map

#undef _alt
#undef _value

end module MAPL_ExtDataFileStreamMap
