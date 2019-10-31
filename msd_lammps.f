      program msd_lammps

      integer*4 start, final, period, type, t_ele
      parameter(maxa=150000)
      integer*4 num, num_dat, num_at(20), num_ele, id(maxa)
      character*64 filename

      call getarg(1,filename)
      open(10,FILE=filename, form='formatted', status='old',
     $     err=99, access='sequential')
      
      call read_data(num,num_dat,num_at,num_ele,id)

      write(*,*)'Please input start step of MSD PERIOD'
      read(5,*) start
      write(*,*)'Please input final step of MSD PERIOD'
      read(5,*) final
      write(*,*)'Please input MSD PERIOD'
      read(5,*) period
      write(*,*)'Do you want to get the detail information ?'
      write(*,*)'yes(1) or no(0)'
      read(5,*) type
C      if(type == 1) then
C         write(*,*)'Which element?'
C         read(5,*) t_ele
C      else
C         t_ele = 0
C      endif
      
      call calc(num,num_dat,start,final,period)
      call output(num,num_at,num_ele,id,period,type)
      
 99   stop
      end

C-----Read data-----C      

      subroutine read_data(num,num_dat,num_at,num_ele,id)

      integer*4 start, final, period, type, t_ele, i_type
      parameter (maxa=150000, maxr=2501)
      
      integer*4 i, step, num, at, id(maxa), num_dat
      integer*4 org, cal_st, cst, num_at(20), num_ele
      real*8    min, max, inter1, inter2
      real*8    xpos0(maxa), ypos0(maxa), zpos0(maxa)
      real*8    xpos(maxa,maxr), ypos(maxa,maxr)
      real*8    zpos(maxa,maxr)
      real*8    cell0(3), cell(3,maxr)
      character*64 filename, dummy
      
      common /pos/ xpos0, ypos0, zpos0, xpos, ypos, zpos, cell0, cell

      do i=1, 20
         num_at(i) = 0
      end do

 50   read(10,'(A64)',end=100)dummy
      read(10,'(I6)')step
      read(10,'(A64)')dummy
      read(10,'(I6)')num
      read(10,'(A64)')dummy
      if(step == 0) then
         num_dat = 0
         do i=1, 3
            read(10,*)min, max
            cell0(i) = abs(max-min)
         end do
         read(10,'(A64)')dummy
         do i=1, num
            read(10,*)at,id(at),xpos0(at),ypos0(at),zpos0(at)
C            write(*,*)i,at, xpos0(at),ypos0(at),zpos0(at)
            num_at(id(at)) = num_at(id(at))+1
         end do
      else
         num_dat = num_dat+1
         do i=1, 3
            read(10,*)min, max
            cell(i,num_dat) = abs(max-min)
C            write(*,*)num_dat,cell(i,num_dat)
         end do
         read(10,'(A64)')dummy
         do i=1, num
            read(10,*)at,id(at),xpos(at,num_dat),ypos(at,num_dat),
     $           zpos(at,num_dat)
C            write(*,*)i,at, xpos(at,num_data),ypos(at,num_data),
C     $           zpos(at,num_data)
         end do
      endif
      go to 50

 100  continue

      write(*,*)'Number of data =',num_dat+1
      write(*,*)' id   number of atoms'
      do i=1, 20
         if(num_at(i) /= 0)then
            write(*,'(1X,I2,4X,I7)')i, num_at(i)
         else
            num_ele = i-1
            exit
         endif
      end do

      return
      end

C------MSD calculation-----C

      subroutine calc(num,num_dat,start,final,period)

      integer*4 num, num_dat, start, final, period, type, t_ele
      integer*4 i, j, k
      parameter(maxa=150000, maxr=2501)
      real*8    dx, dy, dz, diff_x, diff_y, diff_z
      real*8    xpos0(maxa), ypos0(maxa), zpos0(maxa)
      real*8    xpos(maxa,maxr), ypos(maxa,maxr)
      real*8    zpos(maxa,maxr)
      real*8    cell0(3), cell(3,maxr)
      real*8    msd(maxa,maxr), msd_x(maxa,maxr)
      real*8    msd_y(maxa,maxr), msd_z(maxa,maxr)
      
      common /pos/ xpos0, ypos0, zpos0, xpos, ypos, zpos, cell0, cell
      common /msd/ msd, msd_x, msd_y, msd_z

      do i = 1, num
         do j = 1, num_dat
            if(j == 1) then
               dx = xpos(i,j)-xpos0(i)
               dy = ypos(i,j)-ypos0(i)
               dz = zpos(i,j)-zpos0(i)
               xpos0(i) = xpos0(i)*cell0(1)
               ypos0(i) = ypos0(i)*cell0(2)
               zpos0(i) = zpos0(i)*cell0(3)
            else
               dx = xpos(i,j)-xpos(i,j-1)
               dy = ypos(i,j)-ypos(i,j-1)
               dz = zpos(i,j)-zpos(i,j-1)
               xpos(i,j-1) = xpos(i,j-1)*cell(1,j-1)
               ypos(i,j-1) = ypos(i,j-1)*cell(2,j-1)
               zpos(i,j-1) = zpos(i,j-1)*cell(3,j-1)
            endif
         
            if(dx < -0.5d0) then
               xpos(i,j) = xpos(i,j)+1.0d0
            else if(dx > 0.5d0) then
               xpos(i,j) = xpos(i,j)-1.0d0
            else
               xpos(i,j) = xpos(i,j)
            end if
            
            if(dy < -0.5d0) then
               ypos(i,j) = ypos(i,j)+1.0d0
            else if(dy > 0.5d0) then
               ypos(i,j) = ypos(i,j)-1.0d0
            else
               ypos(i,j) = ypos(i,j)
            end if
            
            if(dz < -0.5d0) then
               zpos(i,j) = zpos(i,j)+1.0d0
            else if(dz > 0.5d0) then
               zpos(i,j) = zpos(i,j)-1.0d0
            else
               zpos(i,j) = zpos(i,j)
            end if
         end do
         xpos(i,num_dat) = xpos(i,num_dat)*cell(1,num_dat)
         ypos(i,num_dat) = ypos(i,num_dat)*cell(2,num_dat)
         zpos(i,num_dat) = zpos(i,num_dat)*cell(3,num_dat)
      enddo

      do i=1, num
         write(*,*)'MSD calculation of atom',i
         do j=start, final
            do k=1, period
               if(j==1) then
                  diff_x = xpos(i,k)-xpos0(i)
                  diff_y = ypos(i,k)-ypos0(i)
                  diff_z = zpos(i,k)-zpos0(i)
               else
                  diff_x = xpos(i,j+k-1)-xpos(i,j-1)
                  diff_y = ypos(i,j+k-1)-ypos(i,j-1)
                  diff_z = zpos(i,j+k-1)-zpos(i,j-1)
               endif
               if(diff_x < 0.0d0) diff_x = -1.0d0*(diff_x)
               if(diff_y < 0.0d0) diff_y = -1.0d0*(diff_y)
               if(diff_z < 0.0d0) diff_z = -1.0d0*(diff_z)
               msd(i,k) = msd(i,k)+(diff_x*diff_x)
     $              +(diff_y*diff_y)+(diff_z*diff_z)
               msd_x(i,k) = msd_x(i,k)+(diff_x*diff_x)
               msd_y(i,k) = msd_y(i,k)+(diff_y*diff_y)
               msd_z(i,k) = msd_z(i,k)+(diff_z*diff_z)
            end do
         end do
      end do

      do j=1, period
         do i=1, num
            msd(i,j) = msd(i,j)/(final-start+1)
            msd_x(i,j) = msd_x(i,j)/(final-start+1)
            msd_y(i,j) = msd_y(i,j)/(final-start+1)
            msd_z(i,j) = msd_z(i,j)/(final-start+1)
         end do
      end do

      return
      end

C-----Output the data-----C

      subroutine output(num,num_at,num_ele,id,period,type)

      parameter (maxa=150000, maxr=2501)
      integer*4 num, num_at(20), num_ele, id(maxa), period, t_ele
      integer*4 i, j, k, l, ia1, ia2, type, fileid

      real*8    msd(maxa,maxr)
      real*8    msd_x(maxa,maxr), msd_y(maxa,maxr), msd_z(maxa,maxr) 
      real*8    msd_sp(20,maxr), msd_sp_x(20,maxr)
      real*8    msd_sp_y(20,maxr),msd_sp_z(20,maxr)
      reaL*8    inter1, inter2
      
      character*1  bango
      character*11 base
      character*12 filename
      common /msd/ msd, msd_x, msd_y, msd_z 

      open(30,file='msd.txt',form='formatted',status='unknown',
     $     err=99,access='sequential')
C      if(t_ele /= 0) then
C         open(40,file='msdxyz.txt',form='formatted',status='unknown',
C     $        err=99,access='sequential')
C      endif

      do i=1, period
         do j=1, 20
            msd_sp(j,i)   = 0.0d0
            msd_sp_x(j,i) = 0.0d0
            msd_sp_y(j,i) = 0.0d0
            msd_sp_z(j,i) = 0.0d0
         end do
      end do
      
      do i=1, period
         do j=1, num
C               if(i.EQ.1 .AND. j.EQ.2) then
C                  write(*,*) '----- atom=',k, msd_sp(j,i)
C               endif
            ele = id(j)
            msd_sp(ele,i) = msd_sp(ele,i)+msd(j,i)/num_at(ele)
            msd_sp_x(ele,i) = msd_sp_x(ele,i)+msd_x(j,i)/num_at(ele)
            msd_sp_y(ele,i) = msd_sp_y(ele,i)+msd_y(j,i)/num_at(ele)
            msd_sp_z(ele,i) = msd_sp_z(ele,i)+msd_z(j,i)/num_at(ele)
C               if(i.EQ.1 .AND. j.EQ.2) then
C                  write(*,*) 'atom=',k, msd_sp(j,i)
C               endif
         end do
      end do

      do i=1, period
         write(30,'(I5,1X,10(F11.7))') 
     $   i, (msd_sp(j,i),j=1,num_ele)
C         write(*,'(I5,1X,10(F10.7))') 
C     $   i, (msd_sp(j,i),j=1,kind_ele)
      end do
      
      if(type == 1) then
         do i=1, num_ele
            write(bango,'(I1)')i
            write(*,*) 'bango=',bango
            base = 'msdxyz.txt_'
            filename = base//bango
            fileid = 40+i
            open(40,file=filename, form='formatted',
     $           status='unknown', err=99,access='sequential')
            do j=1, period
               write(40,'(I5,1X,3(F11.7))') j,
     $           msd_sp_x(i,j), msd_sp_y(i,j), msd_sp_z(i,j)
            end do
         end do
      endif
      
      close(30)
C      close(40)
      
      return
 99   stop
      
      end
