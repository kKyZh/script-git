      module m_random_number

      implicit none

      interface rd_nu
      module procedure rd_nu_i
      endinterface rd_nu

      contains

      !-----------------random integer output----

      subroutine rd_nu_i(nu,mi,ma,array)
      implicit none
      integer i, j, k
      real  r
      integer, intent(in) :: nu, mi, ma
      integer, intent(out) :: array(:)

      k=0
      do i=1, nu
100   continue
      call random_seed()
      call random_number(r)
      array(i)=mi+floor((ma+1-mi)*r)
      if(i>1)then
      do j=1, i-1
      if(array(i)==array(j))then
      k=k+1 ! only for check how many times repeated
      goto 100
      else
      endif
      enddo
      else
      endif
      enddo
      write(*,'(a,i,a)')'we have ',k,' repeated numbers'
      write(*,'(a,<nu>(i0,1x))')'the random numbers are: ',&
          (array(i), i=1, nu)
      endsubroutine rd_nu_i
      endmodule
