      program changextal

      integer*4 i, j, k, no, types, id(5000000), num, mode, style
      integer*4 multix, multiy, multiz, ix, iy, iz newnum, num_surf
      integer*4 count, direc, tmp(5000000), ia, ja, peri, nx, ny, nz
      integer*4 surf(5000000), neighb1, neighb2, coord
      real*8    new_cell(3), cell(3), pos(5000000,3)
      real*8    new_pos(5000000,3), mass(16), xlo, ylo, zlo
      real*8    xhi, yhi, zhi, move, ppx, ppy, ppz, dx, dy, dz
      real*8    xborder, yborder, zborder, xmin, range
      real*8    minx, maxx, miny, maxy, minz, maxz
      character*64 dummy, filename

      call getarg(1,filename)
      open(20,FILE=filename, form='formatted', status='old',
     $     err=99,access='sequential')
      open(30,FILE='changedata.lammps', form='formatted', 
     $     status='unknown',access='sequential')

      write(*,*)'Position data in the input file'
      write(*,*)'(1) absolute (2) partial'
      read(5,*) style

      write(*,*)'Please select a mode'
      write(*,*)
     $ '(1) mulitple (2) change of cell para (3) displacement (4) sort'
      read(5,*)mode
      
      select case(mode)
      case(1)
         xborder = 0.0d0
         yborder = 0.0d0
         zborder = 0.0d0
         direc = 0
         write(*,*)'How many cells are there in your new model?'
         write(*,*)
     $   'Please enter the number in x, y and z direction, respectively'
         read(*,*)multix, multiy, multiz
         write(*,*)'(1)Periodic or (2)No periodic ?'
         read(*,*) peri
         if(peri == 2) then
            write(*,*)'Non periodic direction'
            write(*,*)'(1)x, (2)y, (3)z, (4)x and y,'
            write(*,*)'(5)x and z, (6)y and z, (7)all'
            read(*,*) direc
            if(direc == 1 ) then
               write(*,*)'What is the margin of x border?'
               read(*,*) xborder
            else if(direc == 2 ) then
               write(*,*)'What is the margin of y border?'
               read(*,*) yborder
            else if(direc == 3 ) then
               write(*,*)'What is the margin of z border?'
               read(*,*) zborder
            else if(direc == 4) then
               write(*,*)'What is the margin of x border?'
               read(*,*) xborder
               write(*,*)'What is the margin of y border?'
               read(*,*) yborder
            else if(direc == 5) then
               write(*,*)'What is the margin of x border?'
               read(*,*) xborder
               write(*,*)'What is the margin of z border?'
               read(*,*) zborder
            else if(direc == 6) then
               write(*,*)'What is the margin of y border?'
               read(*,*) yborder
               write(*,*)'What is the margin of z border?'
               read(*,*) zborder
            else if(direc == 7) then
               write(*,*)'What is the margin of x border?'
               read(*,*) xborder
               write(*,*)'What is the margin of y border?'
               read(*,*) yborder
               write(*,*)'What is the margin of z border?'
               read(*,*) zborder
            end if
         end if

         read(20,'(A64)')dummy
         read(20,'(A64)')dummy
         read(20,'(I8)')num
         read(20,'(A64)')dummy
         read(20,'(I3)')types
         read(20,'(A64)')dummy
         read(20,*)xlo,xhi
         read(20,*)ylo,yhi
         read(20,*)zlo,zhi
         cell(1) = xhi-xlo
         cell(2) = yhi-ylo
         cell(3) = zhi-zlo
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
         end do

         if(style == 2) then
            do i=1, num
               do j=1, 3
                  pos(i,j) = cell(j)*pos(i,j)
               end do
            end do
         end if

         minx = xlo - xborder
         maxx = xlo + cell(1)*multix + xborder
         miny = ylo - yborder
         maxy = ylo + cell(2)*multiy + yborder
         minz = zlo - zborder
         maxz = zlo + cell(3)*multiz + zborder


         count = 0
         if(peri == 1) then
            do ix=1, multix
               do iy=1, multiy
                  do iz=1, multiz
                     do j=1, num
                        count = count+1
                        id(count) = id(j)
                        new_pos(count,1) = pos(j,1)+(ix-1)*cell(1)
                        new_pos(count,2) = pos(j,2)+(iy-1)*cell(2)
                        new_pos(count,3) = pos(j,3)+(iz-1)*cell(3)
                     end do
                  end do
               end do
            end do
         else if(peri ==2) then
            nx = 1
            ny = 1
            nz = 1
            if(direc==1 .or. direc==4 .or. direc==5 .or. direc==7) then
               nx = 0
               multix = multix+1
            endif
            if(direc==2 .or. direc==4 .or. direc==6 .or. direc==7) then
               ny = 0
               multiy = multiy+1
            endif
            if(direc==3 .or. direc==5 .or. direc==6 .or. direc==7) then
               nz = 0
               multiz = multiz+1
            endif

            do ix=nx, multix
               do iy=ny, multiy
                  do iz=nz, multiz
                     do i=1, num
                        ppx = pos(i,1)+(ix-1)*cell(1)
                        ppy = pos(i,2)+(iy-1)*cell(2)
                        ppz = pos(i,3)+(iz-1)*cell(3)
                        
                        if(ppx >= minx .and. ppx <= maxx) then
                           if(ppy >= miny .and. ppy <= maxy) then
                              if(ppz >= minz .and. ppz <= maxz) then
                                 count =  count +1
                                 id(count) = id(i)
                                 new_pos(count,1) = ppx
                                 new_pos(count,2) = ppy
                                 new_pos(count,3) = ppz
                              endif
                           end if
                        end if
                     end do
                  end do
               end do
            end do
         endif
         
         newnum = count

         write(30,*)' Position data'
         write(30,*)' '
         write(30,'(I8,1X,A5)')newnum, 'atoms'
         write(30,*)' '
         write(30,'(I3,1X,A10)')types, 'atom types'
         write(30,*)' '
         write(30,'(1X,F10.6,1X,F10.6,1X,A7)')minx,maxx,'xlo xhi'
         write(30,'(1X,F10.6,1X,F10.6,1X,A7)')miny,maxy,'ylo yhi'
         write(30,'(1X,F10.6,1X,F10.6,1X,A7)')minz,maxz,'zlo zhi'
         write(30,*)' '
         write(30,*)' Masses'
         write(30,*)' '
         do i=1, types
            write(30,'(I3,1X,F11.7)')i,mass(i)
         end do
         write(30,*)' '
         write(30,*)' Atoms'
         write(30,*)' '
         do i=1, newnum 
            write(30,'(1X,I9,1X,I2,3(F12.7))')
     $           i,id(i),(new_pos(i,j),j=1,3)
         end do

      case(2)
         write(*,*)'New unit cell parameter in x-axis'
         read(5,*)new_cell(1)
         write(*,*)'New unit cell parameter in y-axis'
         read(5,*)new_cell(2)
         write(*,*)'New unit cell parameter in z-axis'
         read(5,*)new_cell(3)
         
         read(20,'(A64)')dummy
         read(20,'(A64)')dummy
         read(20,'(I8)')num
         read(20,'(A64)')dummy
         read(20,'(I3)')types
         read(20,'(A64)')dummy
         read(20,*)xlo,xhi
         read(20,*)ylo,yhi
         read(20,*)zlo,zhi
         cell(1) = xhi-xlo
         cell(2) = yhi-ylo
         cell(3) = zhi-zlo
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
         end do

         write(30,*)' Position data'
         write(30,*)' '
         write(30,'(I8,1X,A5)')num, 'atoms'
         write(30,*)' '
         write(30,'(I3,1X,A10)')types, 'atom types'
         write(30,*)' '
         write(30,'(1X,A3,1X,F11.7,1X,A7)')'0.0',new_cell(1),'xlo xhi'
         write(30,'(1X,A3,1X,F11.7,1X,A7)')'0.0',new_cell(2),'ylo yhi'
         write(30,'(1X,A3,1X,F11.7,1X,A7)')'0.0',new_cell(3),'zlo zhi'
         write(30,*)' '
         write(30,*)' Masses'
         write(30,*)' '
         do i=1, types
            write(30,'(I3,1X,F11.7)')i,mass(i)
         end do
         write(30,*)' '
         write(30,*)' Atoms'
         write(30,*)' '
         do i=1, num
            if(style == 1) then
               ppx = (pos(i,1)-xlo)/cell(1)
               ppy = (pos(i,2)-ylo)/cell(2)
               ppz = (pos(i,3)-zlo)/cell(3)
               if(ppx < 0) then
                  ppx = ppx+1
               else if(ppx > 1) then
                  ppx = ppx-1
               endif
               if(ppy < 0) then
                  ppy = ppy+1
               else if(ppy > 1) then
                  ppy = ppy-1
               endif
               if(ppz < 0) then
                  ppz = ppz+1
               else if(ppz > 1) then
                  ppz = ppz-1
               endif
            endif
            new_pos(i,1) = ppx*new_cell(1)
            new_pos(i,2) = ppy*new_cell(2)
            new_pos(i,3) = ppz*new_cell(3)
            write(30,'(1X,I6,1X,I2,3(F12.7))')
     $           i,id(i),(new_pos(i,j),j=1,3)
         end do

      case(3)
         write(*,*)'Direction of displacement'
         write(*,*)'(1)x (2)y (3)z'
         read(5,*) direc
         write(*,*)'What is the displacement distance?'
         read(5,*) move

         if(direc == 1) then
            dx = move
            dy = 0.0d0
            dz = 0.0d0
         else if(direc == 2) then
            dx = 0.0d0
            dy = move
            dz = 0.0d0
         else if(direc == 3) then
            dx = 0.0d0
            dy = 0.0d0
            dz = move
         end if

         read(20,'(A64)')dummy
         read(20,'(A64)')dummy
         read(20,'(I8)')num
         read(20,'(A64)')dummy
         read(20,'(I3)')types
         read(20,'(A64)')dummy
         read(20,*)xlo,xhi
         read(20,*)ylo,yhi
         read(20,*)zlo,zhi
         cell(1) = xhi-xlo
         cell(2) = yhi-ylo
         cell(3) = zhi-zlo
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
         end do

         if(style == 2) then
            do i=1, num
               do j=1, 3
                  pos(i,j) = cell(j)*pos(i,j)
               end do
            end do
         end if

         write(30,*)' Position data'
         write(30,*)' '
         write(30,'(I8,1X,A5)')num, 'atoms'
         write(30,*)' '
         write(30,'(I3,1X,A10)')types, 'atom types'
         write(30,*)' '
         write(30,'(1X,F11.6,1X,F11.6,1X,A7)')xlo,xhi,'xlo xhi'
         write(30,'(1X,F11.6,1X,F11.6,1X,A7)')ylo,yhi,'ylo yhi'
         write(30,'(1X,F11.6,1X,F11.6,1X,A7)')zlo,zhi,'zlo zhi'
         write(30,*)' '
         write(30,*)' Masses'
         write(30,*)' '
         do i=1, types
            write(30,'(I3,1X,F11.6)')i,mass(i)
         end do
         write(30,*)' '
         write(30,*)' Atoms'
         write(30,*)' '

         do i=1, num
            new_pos(i,1) = pos(i,1)+dx
            new_pos(i,2) = pos(i,2)+dy
            new_pos(i,3) = pos(i,3)+dz
            write(30,'(1X,I6,1X,I2,3(F12.7))')
     $           i,id(i),(new_pos(i,j),j=1,3)
         end do
       
      case(4)
         write(*,*)
     $   'Order of data in data.lammps is sorted by atomic position'
         write(*,*)'What is the direction for sorting? '
         write(*,*)'(1)x (2)y (3)z (4)surface'
         read(5,*) direc
         if(direc == 4) then
            write(*,*)'Enter the distance between neighbors'
            read(5,*) range
            write(*,*)'Enter the coordination number'
            read(5,*) coord
            write(*,*)'Enter the coordination number of 2nd neighbor'
            read(5,*) coord2
         end if

         read(20,'(A64)')dummy
         read(20,'(A64)')dummy
         read(20,'(I8)')num
         read(20,'(A64)')dummy
         read(20,'(I3)')types
         read(20,'(A64)')dummy
         read(20,*)xlo,xhi
         read(20,*)ylo,yhi
         read(20,*)zlo,zhi
         cell(1) = xhi-xlo
         cell(2) = yhi-ylo
         cell(3) = zhi-zlo
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
         end do
         
         if(style == 2) then
            do i=1, num
               do j=1, 3
                  pos(i,j) = cell(j)*pos(i,j)
               end do
            end do
         end if

         if(direc == 4) then
            surf = 0
            num_surf = 0

            do i=1, num
               neighb1 = 0
               neighb2 = 0
               do j=1, num
                  if(i == j) cycle
                  dx = pos(j,1)-pos(i,1)
                  dy = pos(j,2)-pos(i,2)
                  dz = pos(j,3)-pos(i,3)
                  dist = dsqrt(dx*dx+dy*dy+dz*dz)
                  if(dist <= range) then
                     neighb1 = neighb1+1
                  end if
                  if(dist <=2*range) then
                     neighb2 = neighb2 +1
                  endif
               end do
               if(neighb1 < coord) then
                  if(neighb2 < coord2) then
                     num_surf = num_surf+1
                     if(mod(num_surf,1000) == 0) then
                        write(*,*)'number of surface atom',num_surf
                     endif
                     surf(i) = 1
                  end if
               endif
            end do

            write(30,*)' Position data'
            write(30,*)' '
            write(30,'(I8,1X,A5)')num, 'atoms'
            write(30,*)' '
            write(30,'(I3,1X,A10)')types, 'atom types'
            write(30,*)' '
            write(30,'(1X,F11.6,1X,F11.6,1X,A7)')xlo,xhi,'xlo xhi'
            write(30,'(1X,F11.6,1X,F11.6,1X,A7)')ylo,yhi,'ylo yhi'
            write(30,'(1X,F11.6,1X,F11.6,1X,A7)')zlo,zhi,'zlo zhi'
            write(30,*)' '
            write(30,*)' Masses'
            write(30,*)' '
            do i=1, types
               write(30,'(I3,1X,F11.6)')i,mass(i)
            end do
            write(30,*)' '
            write(30,*)' Atoms'
            write(30,*)' '

            count = 0
            do i=1, num
               if(surf(i) == 0) then
                  count = count+1
                  write(30,'(1X,I6,1X,I2,3(F12.7))')
     $                 count,id(i),(pos(i,j),j=1,3)
               endif
            end do
            
            do i=1, num
               if(surf(i) == 1) then
                  count = count+1
                  write(30,'(1X,I6,1X,I2,3(F12.7))')
     $                 count,id(i),(pos(i,j),j=1,3)
               endif
            end do
            
         else
            tmp = 0
            do i=1, num
               if(i == 1) then
                  tmp(i) = i
               else 
                  do j=1, i-1
                     ja = tmp(j)
                     pp = pos(ja,direc)
                     if(pos(i,direc) < pp) then
                        do k=i, j+1, -1
                           tmp(k) = tmp(k-1)
                        end do
                        tmp(j) = i
                        go to 50
                     endif
                  end do
                  tmp(i) = i
               endif
 50         end do

            write(30,*)' Position data'
            write(30,*)' '
            write(30,'(I8,1X,A5)')num, 'atoms'
            write(30,*)' '
            write(30,'(I3,1X,A10)')types, 'atom types'
            write(30,*)' '
            write(30,'(1X,F11.6,1X,F11.6,1X,A7)')xlo,xhi,'xlo xhi'
            write(30,'(1X,F11.6,1X,F11.6,1X,A7)')ylo,yhi,'ylo yhi'
            write(30,'(1X,F11.6,1X,F11.6,1X,A7)')zlo,zhi,'zlo zhi'
            write(30,*)' '
            write(30,*)' Masses'
            write(30,*)' '
            do i=1, types
               write(30,'(I3,1X,F11.6)')i,mass(i)
            end do
            write(30,*)' '
            write(30,*)' Atoms'
            write(30,*)' '
            
            do i=1, num
               ia = tmp(i)
               write(30,'(1X,I6,1X,I2,3(F12.7))')
     $              i,id(ia),(pos(ia,j),j=1,3)
            end do
         endif

      end select
      
      close(20)
      close(30)
 99   stop
      end

