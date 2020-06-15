      program periodicalsupercellz
        implicit none
        integer i,j,totat,re_cell
        real  pos_z
        real  z
        character(128) dummy
        character(32) dummy1,dummy2


      open(10,file='position.fdf', status='old',&
        form='formatted', access='sequential')
      open(20,file='newposition.fdf', status='unknown',&
        form='formatted', access='sequential')

      re_cell=3
      totat=340
      pos_z=60.497271
      read(10,'(a)')dummy
      write(20,'(a)')dummy
      do j=1, re_cell
      do i=1, totat
      read(10,'(a24,f12.7,(a))')dummy1,z,dummy2
      z=z+pos_z*(j-1)
      write(20,'(a24,f12.7,(a))')dummy1,z,dummy2
      enddo
      if(j.eq.re_cell)goto 1000
      rewind(10)
      read(10,'(a)')dummy
      enddo
1000  continue
      read(10,'(a)')dummy
      write(20,'(a)')dummy

      endprogram periodicalsupercellz
