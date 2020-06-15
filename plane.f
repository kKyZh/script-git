      program plane
      
      integer*4 i, j, k ,l, style, num, types, no, catom
      integer*4 task, count, numk(20,3), id_patom(20,3)
      integer*4, allocatable, dimension(:)::id, new_id, kind
      real*8    vx, vy, vz, cx, cy, cz, norm
      real*8    cell(3), xlo, xhi, ylo, yhi, zlo, zhi
      real*8    center(3), mass(10), dx, dy, dz, dist, cdist, dotp
      real*8    ux, uy, uz
      real*8, allocatable, dimension(:,:)::pos

      character*64 filename, dummy

      call getarg(1,filename)
      open(20,FILE=filename, form='formatted', status='old',
     $     err=99,access='sequential')
      open(30,FILE='changedata.plane', form='formatted', 
     $     status='unknown',access='sequential')

      write(*,*)'Position data in the input file.'
      write(*,*)'(1) absolute (2) partial'
      read(5,*) style

      write(*,*)'Please select the task'
      write(*,*)'(1) Group separation by plane (2) Only plane'
      read(5,*) task

      write(*,*)'Please enter the normal vecter of plane.'
      write(*,*)'e.g. (111) -> 1 1 1'
      read(*,*)vx, vy, vz
      norm = dsqrt(vx*vx+vy*vy+vz*vz)
      vx = vx/norm
      vy = vy/norm
      vz = vz/norm

      write(*,*)'Please enter the center position of plane.'
      write(*,*)'Use the partial position.'
      read(*,*)cx, cy, cz

      read(20,'(A64)')dummy
      read(20,'(A64)')dummy
      read(20,'(I8)')num

      allocate(id(num))
      allocate(pos(num,3))
      allocate(kind(num))
      allocate(new_id(num))

      read(20,'(A64)')dummy
      read(20,'(I3)')types
      read(20,'(A64)')dummy
      read(20,*)xlo,xhi
      read(20,*)ylo,yhi
      read(20,*)zlo,zhi
      cell(1) = xhi-xlo
      cell(2) = yhi-ylo
      cell(3) = zhi-zlo
      center(1) = xlo+cell(1)*cx
      center(2) = ylo+cell(2)*cy
      center(3) = zlo+cell(3)*cz

      read(20,'(A64)')dummy
      read(20,'(A64)')dummy
      read(20,'(A64)')dummy
      do i=1, types
         read(20,'(3X,F11.7)')mass(i)
      end do
      read(20,'(A64)')dummy
      read(20,'(A64)')dummy
      read(20,'(A64)')dummy
      do i=1, num
         read(20,*)no,id(i),(pos(i,j),j=1,3)
         if(style == 2) then
            pos(i,1) = pos(i,1)*cell(1)
            pos(i,2) = pos(i,2)*cell(2)
            pos(i,3) = pos(i,3)*cell(3)
         endif
         dx = pos(i,1) - center(1)
         dy = pos(i,2) - center(2)
         dz = pos(i,3) - center(3)
         dist = dsqrt(dx*dx+dy*dy+dz*dz)
         
         if(i==1) then
            cdist = dist
            catom = 1
         else if(dist < cdist) then
            cdist = dist
            catom = i
         end if
         
      end do

      write(*,'(A13,1X,I7,1X,3(F12.7))')
     $     'center atom =',catom, (pos(catom,j),j=1,3)

      num_patom  = 0
      kind = 1
      numk = 0
      do i=1, num
         if(i == catom) then
            kind(i) = 2
            numk(id(i),2) = numk(id(i),2)+1
            write(*,'(A12,1X,I7,1X,3(F12.7))')
     $              'on the plane',i,pos(i,1),pos(i,2),pos(i,3)
         else
            dx = pos(i,1) - pos(catom,1)
            dy = pos(i,2) - pos(catom,2)
            dz = pos(i,3) - pos(catom,3)
            norm = dsqrt(dx*dx+dy*dy+dz*dz)
        
            ux = dx/norm
            uy = dy/norm
            uz = dz/norm
         
            dotp = ux*vx+uy*vy+uz*vz

            if(abs(dotp) <= 0.0001) then
               kind(i) = 2
               numk(id(i),2) = numk(id(i),2)+1
               write(*,'(A12,1X,I7,1X,4(F12.7))')
     $           'on the plane',i,pos(i,1),pos(i,2),pos(i,3),dotp
               
            else if(dotp < -0.0001) then
               if(task == 1) then
                  kind(i) = 3
                  numk(id(i),3) = numk(id(i),3)+1
                  write(*,'(A12,1X,I7,1X,4(F12.7))')
     $                 'on the plane',i,pos(i,1),pos(i,2),pos(i,3),dotp
               else
                  kind(i) = 1
                  numk(id(i),1) = numk(id(i),1)+1
               endif
            else
               kind(i) = 1
               numk(id(i),1) = numk(id(i),1)+1
            endif
         endif
      end do

      count = 0
      do i=1, 3
         do j=1, types
            if(numk(j,i) > 0) then
               count = count+1
               id_patom(j,i) = count
            endif
         end do
      end do
      
      write(*,*)'count=',count
      do i=1, num
         j = id(i)
         k = kind(i)
         new_id(i) = id_patom(j,k)
      end do

      i = 1
      types = count
      mass = 10.0d0
      write(30,*)' Position data'
      write(30,*)' '
      write(30,'(I8,1X,A5)')num, 'atoms'
      write(30,*)' '
      write(30,'(I3, 1X, A10)')types,'atom types'
      write(30,*)' '
      write(30,'(1X,F10.6,1X,F10.6,1X,A7)')xlo,xhi,'xlo xhi'
      write(30,'(1X,F10.6,1X,F10.6,1X,A7)')ylo,yhi,'ylo yhi'
      write(30,'(1X,F10.6,1X,F10.6,1X,A7)')zlo,zhi,'zlo zhi'
      write(30,*)' '
      write(30,*)' Masses'
      write(30,*)' '
      do i=1, types
         write(30,'(I3,1X,F11.7)')i,mass(i)
      end do
      write(30,*)' '
      write(30,*)' Atoms'
      write(30,*)' '
      do i=1,types
         do j=1,num
            if(new_id(j) == i) then
               write(30,'(1X,I9,1X,I2,3(F12.7))')
     $            j,new_id(j),(pos(j,k),k=1,3)
            endif
         end do
      end do
C         write(*,'(1X,I9,1X,I2,3(F12.7))')
C     $        patom(i),id_patom(patom(i)),(pos(patom(i),j),j=1,3)
      
 99   stop
      end
