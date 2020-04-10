      module m_swap
      implicit none
      interface swap
      module procedure swapreal,swapint
      endinterface

      contains
      
      subroutine swapreal(a,b)
      implicit none
      real, intent(inout) :: a,b
      real temp
      temp=a
      a=b
      b=temp
      endsubroutine swapreal

      subroutine swapint(a,b)
      implicit none
      integer, intent(inout) :: a,b
      integer temp
      temp=a
      a=b
      b=temp
      endsubroutine swapint
      endmodule
