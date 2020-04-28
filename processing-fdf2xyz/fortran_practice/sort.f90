      module m_sort
      
      use m_swap
      
      implicit none
      
      interface sort
      
      module procedure burble_sort
      
      endinterface

      contains
      
      subroutine burble_sort(nu,array)
      
      implicit none
      integer i, j, k, l, lo, hi
      integer, intent(in) :: nu
      integer, intent(inout) :: array(:)
      character(32) i_k,i_j 
      
      !-----------------------------burble sort-------
      k=0
      lo=1
      hi=nu-1
      do j=lo,hi
      l=0
      do i=lo,hi
      if(array(i)>array(i+1))then
      call swap(array(i),array(i+1))
      k=k+1
      l=l+1
      else
      endif
      enddo
      if(l.eq.0)exit
      hi=hi-1
      ! ---------------------------------------
      !write(*,'(a,<nu>(i4,1x))')'the sorted numbers are: ',&
      !(array(i), i=1,nu)
      ! ---------------------output check options
      enddo

      !---------------------------------- other output options
      write(*,*)
      !write(*,*)j-1 ! sorted loops/times
      write(i_j,'(i0)')j-1
      write(*,'(3a)')'we sorted ',trim(i_j)," loops' times"
      write(*,*)
      write(i_k,'(i0)')k
      write(*,'(3a)')'we swap ',trim(i_k),' times'
      write(*,'(a,<nu>(i0,1x))')'the sorted numbers are: ',&
      (array(i), i=1, nu)
      ! ---------------------------------------------------

      endsubroutine burble_sort
      
      endmodule
