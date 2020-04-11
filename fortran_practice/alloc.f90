      module m_alloc
     
      implicit none

      interface alloc
      module procedure alloc_i1

      endinterface

      interface dealloc
      module procedure dealloc_i1

      endinterface

      contains

      subroutine alloc_i1(ma,arr_na)
      ! max array and array name
      implicit none
      integer, intent(in) :: ma
      integer, allocatable :: arr_na(:)
      
      allocate(arr_na(ma))
      endsubroutine alloc_i1

      subroutine dealloc_i1(arr_na)
      implicit none
      !integer, intent(in) :: ma
      integer, allocatable :: arr_na(:)

      deallocate(arr_na)
      endsubroutine dealloc_i1
      endmodule
