#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module MAPL_ExtdataAbstractFileHandler
   use ESMF
   use yafYaml
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use MAPL_ExtDataBracket
   use MAPL_ExtDataFileStream
   use MAPL_ExtDataFileStreamMap
   use MAPL_ExtDataCollectionMod
   use MAPL_CollectionVectorMod
   use MAPL_ExtDataCollectionManagerMod
   use MAPL_FileMetadataUtilsMod
   use MAPL_TimeStringConversion
   use MAPL_StringTemplate
   implicit none
   private
   public :: ExtDataAbstractFileHandler

   type, abstract :: ExtDataAbstractFileHandler
      character(:), allocatable :: file_template
      type(ESMF_TimeInterval) :: frequency
      type(ESMF_Time) :: reff_time
      integer :: collection_id
      type(ESMF_Time), allocatable :: valid_range(:)
      logical :: persist_closest
      contains
         procedure :: initialize 
         procedure :: make_metadata
         procedure :: get_time_on_file
         procedure(get_file_bracket), deferred :: get_file_bracket
   end type

   abstract interface
      subroutine get_file_bracket(this, input_time, source_time, bracket, rc)
         use ESMF
         use MAPL_ExtDataBracket
         import ExtDataAbstractFileHandler
         class(ExtDataAbstractFileHandler), intent(inout)  :: this
         type(ESMF_Time), intent(in) :: input_time
         type(ESMF_Time), intent(in) :: source_time(:)
         type(ExtDataBracket), intent(inout) :: bracket
         integer, optional, intent(out) :: rc
      end subroutine get_file_bracket

   end interface
         

contains

   subroutine initialize(this,file_series,persist_closest,unusable,rc)
      class(ExtDataAbstractFileHandler), intent(inout)  :: this
      type(ExtDataFileStream), intent(in) :: file_series
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(in) :: persist_closest
      integer, optional, intent(out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)

      this%file_template = file_series%file_template
      this%frequency = file_series%frequency
      this%reff_time = file_series%reff_time
      allocate(this%valid_range,source=file_series%valid_range)
      this%collection_id = file_series%collection_id
      if (present(persist_closest)) then
         this%persist_closest = persist_closest
      else
         this%persist_closest = .false.
      end if

   end subroutine initialize

   subroutine get_time_on_file(this,filename,target_time,bracketside,time_index,output_time,wrap,rc)
      class(ExtdataAbstractFileHandler), intent(inout) :: this
      character(len=*), intent(inout) :: filename
      type(ESMF_Time), intent(in) :: target_time
      character(len=*), intent(in) :: bracketside
      integer, intent(Out) :: time_index
      type(ESMF_Time), intent(out) :: output_time
      integer, optional, intent(inout) :: wrap
      integer, optional, intent(out) :: rc
      integer :: status

      type(FileMetadataUtils), pointer :: file_metadata
      type(ESMF_Time), allocatable :: time_series(:)
      logical :: in_bounds, found_time, wrap_
      integer :: i,num_times
    
      if (present(wrap)) then
         wrap_= .true.
      else
         wrap_=.false.
      end if

      call this%make_metadata(filename,file_metadata,__RC__) 
      call file_metadata%get_time_info(timeVector=time_series,__RC__)
      num_times = size(time_series)
      found_time = .false.
      if (bracketside == 'L') then
         in_bounds = .not.(target_time < time_series(1))
         if (in_bounds) then
            do i=num_times,1,-1
               if (target_time >= time_series(i)) then
                  output_time = time_series(i)
                  time_index = i
                  found_time = .true.
                  exit
               end if
            enddo
         else 
            if (wrap_) then
               output_time=time_series(num_times)
               time_index = num_times
               found_time = .true.
               wrap = -1   
            end if
         end if         
      else if (bracketside == 'R') then
         in_bounds = .not.(target_time >= time_series(num_times))
         if (in_bounds) then
            do i=1,num_times
               if (target_time < time_series(i)) then
                  output_time = time_series(i)
                  time_index = i
                  found_time = .true.
                  exit
               end if
            enddo
         else 
            if (wrap_) then
               output_time=time_series(1)
               time_index = 1
               found_time = .true.
               wrap = 1   
            end if
         end if
      else
         _ASSERT(.false.,"unknown bracket side")
      end if

      !if (found_time .and. in_bounds) then
      if (found_time) then
         _RETURN(_SUCCESS)
      else
         _RETURN(_FAILURE)
      end if 

   end subroutine get_time_on_file

   subroutine make_metadata(this,file,metadata,rc)
      class(ExtdataAbstractFileHandler), intent(inout) :: this
      character(len=*), intent(in   ) :: file
      type(FileMetadataUtils), pointer, intent(inout)   :: metadata
      integer, optional,          intent(out  ) :: rc
      type(MAPLExtDataCollection), pointer :: collection => null()
 
      Collection => ExtDataCollections%at(this%collection_id)
      metadata => collection%find(file)
     _RETURN(_SUCCESS)

  end subroutine make_metadata
 

end module MAPL_ExtdataAbstractFileHandler
