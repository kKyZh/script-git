!-------------------------------------------------------------------------------
module jmk_readline_module
   use ISO_C_Binding
   implicit none
   private
   public readline
!-------------------------------------------------------------------------------
! The structure used to store a history entry.
  type, bind(C) :: hist_entry_c
    type(C_ptr) :: line, timestamp, data
  end type hist_entry_c

! A structure used to pass the current state of the history stuff around.
  type, bind(C) :: hist_state_c
    type(C_ptr) :: entries;     ! (hist_entry**)  Pointer to the entries themselves.
    integer(C_int) :: offset    ! The location pointer within this array.
    integer(C_int) :: length    ! Number of elements within this array.
    integer(C_int) :: size      ! Number of slots allocated to this array.
    integer(C_int) :: flags
  end type hist_state_c
!-------------------------------------------------------------------------------
  interface
    function readline_c(prompt) result(line_ptr) bind(C,name="readline")
      import
      type(C_ptr) :: line_ptr
      character(kind=C_char), intent(in) :: prompt(*)
    end function readline_c
    subroutine using_history() bind(C)
      import
    end subroutine using_history
    subroutine add_history_c(line_ptr) bind(C,name="add_history")
      import
      type(C_ptr), value, intent(in) :: line_ptr
    end subroutine add_history_c
    function history_get_history_state() result(state_ptr) bind(C)
      import
      type(C_ptr) :: state_ptr
    end function history_get_history_state
    subroutine free(ptr) bind(C)
      import
      type(C_ptr), value, intent(in) :: ptr
    end subroutine free
    function strlen(s) bind(C)
      import
      integer(C_size_t) :: strlen
      type(C_ptr), value, intent(in)  :: s
    end function strlen
  end interface
!-------------------------------------------------------------------------------
contains
  subroutine readline(line,prompt)
    implicit none
    character(len=*), intent(out) :: line
    character(len=*), intent(in)  :: prompt
  ! local
    type(C_ptr) :: line_ptr
    character(kind=C_char), pointer :: char_ptr(:)
    integer i
  ! begin
    call using_history()
  ! trim to last non-blank character and append null for C, and
  ! call readline(3c) to read a line of input in edit mode
    line_ptr = readline_c(trim(prompt)//C_NULL_char)
    call add_history_c(line_ptr)
  ! Copy the result to 'line'
    line=' '
    call C_F_pointer(line_ptr,char_ptr,(/len(line)/))
    do i=1,len(line)
      if (iachar(char_ptr(i))==0) exit
      line(i:i)=char_ptr(i)
    end do
  ! if the "h" command is on a line by itself, show history
    if (line=='h') call show_history_list()
  ! free allocated result line returned from readline(3c)
    call free(line_ptr)
  end subroutine readline
  subroutine show_history_list()
    type(C_ptr) :: ptr
    type(hist_state_c), pointer :: state_ptr
    type(C_ptr), pointer :: entries(:)
    type(hist_entry_c), pointer :: entry
    character(kind=C_char,len=huge(0)), pointer :: line
    integer :: i, line_len
    ptr = history_get_history_state()
    call C_F_pointer(ptr,state_ptr)
    call C_F_pointer(state_ptr%entries,entries,(/state_ptr%length/))
    write(*,*) 'History list now:'
    do i=1,size(entries,1)
      call C_F_pointer(entries(i),entry)
      call C_F_pointer(entry%line,line)
      line_len=strlen(entry%line)
      write(*,'(I0,4A)') i,": '",line(:line_len),"'",merge('*',' ',i==state_ptr%offset)
    end do
  end subroutine show_history_list
!-------------------------------------------------------------------------------
end module jmk_readline_module
!-------------------------------------------------------------------------------