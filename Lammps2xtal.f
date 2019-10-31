      program Lammps2xtal

      integer*4 i, j, k, ll, mm, types, id(100000), num, no, ptype
      real*8    new_cell(3), cell(3), pos(100000,3), min(3), max(3)
      real*8    new_pos(100000,3), mass(16), px, py, pz
      character*64 dummy, filename
      character*2 name(20)

      call getarg(1,filename)
      open(20,FILE=filename, form='formatted', status='old',
     $     err=99,access='sequential')
      open(30,FILE='xtal.mdy', form='formatted', 
     $     status='unknown',access='sequential')

      read(20,'(A64)')dummy
      read(20,'(A64)')dummy
      read(20,'(I8)')num
      read(20,'(A64)')dummy
      read(20,'(I3)')types
      read(20,'(A64)')dummy
      read(20,*)min(1), max(1)
      read(20,*)min(2), max(2)
      read(20,*)min(3), max(3)
      write(*,*)min(1),max(1)
      read(20,'(A64)')dummy
      read(20,'(A64)')dummy
      read(20,'(A64)')dummy
      do i=1, types
         read(20,'(3X,F11.7)')mass(i)
      end do
      read(20,'(A64)')dummy
      read(20,'(A64)')dummy
      read(20,'(A64)')dummy
      ele_num = 0
      do i=1, num
         read(20,*)no,id(i),(pos(i,j),j=1,3)
         if(id(i) > ele_num) then
            ele_num = id(i)
         endif
      end do

      do i=1, ele_num
         write(*,'(A26,I3,1X,A7)')
     $        'Please enter the name of #',i,'element'
         read(5,*) name(i)
      end do
      write(*,*)'Which types atomic data of positions ?'
      write(*,*)'(1) absolute value or (2) partial value'
      read(5,*) ptype

      write(30,'(A5)')'TITLE'
      write(30,'(A63)') 
     $ 'Charge  0.0  Spin 0.0  VSIP 2  Ratio 100  Smear 0.10000
     $ MoEne 0'
      cell(1) = abs(max(1)-min(1))
      cell(2) = abs(max(2)-min(2))
      cell(3) = abs(max(3)-min(3))
      write(30,'(3(F10.5))')cell(1), cell(2), cell(3)
      do i=1, num
C         if(min(1) < 0.0d0) pos(i,1) = pos(i,1)+abs(min(1))
C         if(min(2) < 0.0d0) pos(i,2) = pos(i,2)+abs(min(2))
C         if(min(3) < 0.0d0) pos(i,3) = pos(i,3)+abs(min(3))
         if(ptype == 2) then
            pos(i,1) = pos(i,1)/cell(1)
            pos(i,2) = pos(i,2)/cell(2)
            pos(i,3) = pos(i,3)/cell(3)
            if(pos(i,1) < 0.0d0) pos(i,1) = pos(i,1)+1.0d0
            if(pos(i,2) < 0.0d0) pos(i,2) = pos(i,2)+1.0d0
            if(pos(i,3) < 0.0d0) pos(i,3) = pos(i,3)+1.0d0
            if(pos(i,1) >= 1.0d0) pos(i,1) = pos(i,1)-1.0d0
            if(pos(i,2) >= 1.0d0) pos(i,2) = pos(i,2)-1.0d0
            if(pos(i,3) >= 1.0d0) pos(i,3) = pos(i,3)-1.0d0
         endif
         write(30,'(1X,A2,1X,I2,4X,3(F10.5))')
     $        name(id(i)),id(i),(pos(i,j),j=1,3)
      end do

      close(20)
      close(30)
 99   stop
      end
