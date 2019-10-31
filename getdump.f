      program getdump

      parameter(maxa=1000000,maxr=100)
      integer*4 num, num_dat, num_at(20), num_ele, id(maxa)
      integer*4 i, step, at, get_num, nx, ny, nz, type
      integer*4 order, numv
      real*8    xpos, ypos, zpos
      real*8    cell_lo(3,maxr), cell_hi(3,maxr)
      real*8    val(11,maxa,maxr)
      character*128 filename, dummy

      call getarg(1,filename)
      open(10,FILE=filename, form='formatted', status='old',
     $     err=99, access='sequential')
      open(30,FILE='getdata.lammps', form='formatted',
     $     status='unknown',access='sequential') 

      val = 0.0d0
      num_dat = 0

      write(*,*)'How many variables in one row ?'
      read(5,*) order
      numv = order-2

 50   read(10,'(A64)',end=100)dummy
      num_dat = num_dat+1
      num_at = 0

      read(10,'(I6)')step
      read(10,'(A64)')dummy
      read(10,'(I6)')num
      read(10,'(A64)')dummy
      do i=1, 3
         read(10,*)cell_lo(i,num_dat), cell_hi(i,num_dat)
      end do
      read(10,'(A128)')dummy
      do i=1, num
         read(10,*)at,id(at),(val(j,at,num_dat),j=1,numv)
         num_at(id(at)) = num_at(id(at))+1
      end do
      go to 50

 100  continue

      write(*,*)'Number of data =',num_dat
      write(*,*)' id   number of atoms'
      do i=1, 20
         if(num_at(i) /= 0)then
            write(*,'(1X,I2,4X,I7)')i, num_at(i)
         else
            num_ele = i-1
            exit
         endif
      end do

      write(*,*)'Format of coordination of atom position'
      write(*,*)'(1) absolute (2) partial'
      read(5,*) type
      write(*,*)'Which number of data do you want to get'
      read(5,*) get_num
      write(*,*)'Where is the data of position x in the data row'
      read(5,*) order
      nx = order - 2
      write(*,*)'Where is the data of position y in the data row'
      read(5,*) order
      ny = order - 2
      write(*,*)'Where is the data of position z in the data row'
      read(5,*) order
      nz = order - 2

      write(30,*)'# Position data'
      write(30,'(I8,1X,A5)')num, 'atoms'
      write(30,'(I3,1X,A10)')num_ele, 'atom types'
      write(30,'(F11.7,1X,F11.7,1X,A7)')
     $     cell_lo(1,get_num), cell_hi(1,get_num),'xlo xhi'
      cellx = abs(cell_hi(1,get_num)-cell_lo(1,get_num))
      write(30,'(F11.7,1X,F11.7,1X,A7)')
     $     cell_lo(2,get_num), cell_hi(2,get_num),'ylo yhi'
      celly = abs(cell_hi(2,get_num)-cell_lo(2,get_num))
      write(30,'(F11.7,1X,F11.7,1X,A7)')
     $     cell_lo(3,get_num), cell_hi(3,get_num),'zlo zhi'
      cellz = abs(cell_hi(3,get_num)-cell_lo(3,get_num))
      write(30,*)' '
      write(30,*)'Atoms'
      write(30,*)' '
      do i=1, num
         if(type == 2) then
            posx = val(nx,i,get_num)*cellx + cell_lo(1,get_num)
            posy = val(ny,i,get_num)*celly + cell_lo(2,get_num)
            posz = val(nz,i,get_num)*cellz + cell_lo(3,get_num)
         else
            posx = val(nx,i,get_num)
            posy = val(ny,i,get_num)
            posz = val(nz,i,get_num)
         endif
         write(30,'(1X,I7,1X,I2,3(F12.7))')
     $        i, id(i), posx, posy, posz
      end do
      
 99   stop
      end



