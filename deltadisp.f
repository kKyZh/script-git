      program deltadisp

      parameter(maxa=1000000,maxr=100)
      integer*4 i, j, k
      integer*4 num, num_dat, num_at(20), num_ele, id(maxa)
      integer*4 step, at, get_num, nx, ny, nz, type
      integer*4 order, numv, npx, npy, npz, numn, numa
      integer*4 pair1(maxa), pair2(maxa), mid(maxa)
      real*8    xpos(maxa), ypos(maxa), zpos(maxa), cutoff
      real*8    cell_lo(3,maxr), cell_hi(3,maxr)
      real*8    val(11,maxa,maxr), dx, dy, dz, disp(maxa)
      real*8    nposx(maxa), nposy(maxa), nposz(maxa)
      real*8    dist, del, delx, dely, delz, move
      real*8    dd(maxa), ddx(maxa), ddy(maxa), ddz(maxa)
      character*128 filename, dummy, dummy2(maxr), dummy3(maxr)
      character*20 vname(15), vdum1, vdum2

      call getarg(1,filename)
      open(10,FILE=filename, form='formatted', status='old',
     $     err=99, access='sequential')

      val = 0.0d0
      num_dat = 0

      write(*,*)'How many variables in one row ?'
      read(5,*) order
      numv = order


 50   read(10,'(A64)',end=100)dummy
      num_dat = num_dat+1
      num_at = 0

      read(10,'(I6)')step
      read(10,'(A64)')dummy
      read(10,'(I6)')num
      read(10,'(A64)')dummy2(num_dat)
      do i=1, 3
         read(10,*)cell_lo(i,num_dat), cell_hi(i,num_dat)
      end do
      read(10,*) vdum1, vdum2, (vname(j),j=1,numv)
      
      do i=3, numv+2
         if(vname(i) == 'x' .or. vname(i) == 'xs') then
            npx = i-2
            write(*,*)'npx = ', npx
         else if(vname(i) == 'y' .or. vname(i) == 'ys') then
            npy = i-2
            write(*,*)'npy = ', npy
         else if(vname(i) == 'z' .or. vname(i) == 'zs') then
            npz = i-2
            write(*,*)'npz = ', npz
         endif
      end do

      do i=1, num
         read(10,*)at,id(at),(val(j,at,num_dat),j=1,numv-2)
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
      write(*,*)'Which data do you want to analyze?'
      write(*,*)'If you want to analyze all, please select 0'
      read(5,*) get_num

      write(*,*)'Where id the date of x displacement in the data row'
      read(5,*) order
      nx = order - 2
      write(*,*)'Where id the date of y displacement in the data row'
      read(5,*) order
      ny = order - 2
      write(*,*)'Where id the date of z displacement in the data row'
      read(5,*) order
      nz = order - 2
      write(*,*)'What is the cutoff distance'
      read(5,*) cutoff
      write(*,*)'What is the criterion of the displacement'
      read(5,*) move

      select case(get_num)
      case(0)
         do i=1, num_dat
            write(filename, '("ddisplace",i3.3,".dat")') i
            open (30,file=filename, status='replace')
         
            numn = 0
            do j=1, num
               if(mod(j,50000) == 0) then
                  write(*,*)'Now calculating ',j
               endif
               do k= 1, j-1
                  if(j == k) cycle
                  dx = abs(val(npx,j,i)-val(npx,k,i))
                  if(dx > cutoff) cycle
                  dy = abs(val(npy,j,i)-val(npy,k,i))
                  if(dy > cutoff) cycle
                  dz = abs(val(npz,j,i)-val(npz,k,i))
                  if(dz > cutoff) cycle
                  
                  dist = dsqrt(dx*dx+dy*dy+dz*dz)
                  if(dist <= cutoff) then
                     delx = val(nx,j,i)-val(nx,k,i)
                     dely = val(ny,j,i)-val(ny,k,i)
                     delz = val(nz,j,i)-val(nz,k,i)
                     del = dsqrt(delx*delx+dely*dely+delz*delz)
                     if(abs(del) > move) then
                        numn = numn+1
                        write(*,'(I5,1X,I6,1X,I6,1X,3(F8.4))')
     $                       numn, j, k, delx, dely, delz
                        nposx(numn) = 0.5d0*(val(npx,j,i)+val(npx,k,i))
                        nposy(numn) = 0.5d0*(val(npy,j,i)+val(npy,k,i))
                        nposz(numn) = 0.5d0*(val(npz,j,i)+val(npz,k,i))
                        ddx(numn) = abs(delx)
                        ddy(numn) = abs(dely)
                        ddz(numn) = abs(delz)
                        dd(numn) = abs(del)
                        pair1(numn) = j
                        pair2(numn) = k
                     endif
                  end if
               end do
            end do

            write(30,'(A)')'ITEM: TIMESTEP'
            write(30,'(I0)')i
            write(30,'(A)')'ITEM: NUMBER OF ATOMS'
            write(30,'(I0)')numn
            write(30,'(A)')dummy2(i)
            write(30,'(F0.6,1X,F0.6)')cell_lo(1,i), cell_hi(1,i)
            write(30,'(F0.6,1X,F0.6)')cell_lo(2,i), cell_hi(2,i)
            write(30,'(F0.6,1X,F0.6)')cell_lo(3,i), cell_hi(3,i)
            write(30,'(A)')
     $           'ITEM: ATOMS id type type x y z dd ddx ddy ddz'
            do j=1, numn
               write(30,'(I0,1X,I0,1x,I0,1X,7(F0.6,1X))')
     $              j,pair1(j),pair2(j),nposx(j),nposy(j),nposz(j),
     $              dd(j),ddx(j), ddy(j), ddz(j)
            end do
            write(*,*)'finish data',i

            close(30)
         end do
      case(1:)
         i = get_num
         write(filename, '("ddisplace",i3.3,".dat")') i
         open (30,file=filename, status='replace')
         open (40,file='displace.dat', status='new')

         numn = 0
         numa = 0
         do j=1, num
            if(mod(j,50000) == 0) then
               write(*,*)'Now calculating ',j
            endif

            dx = val(nx,j,i)
            dy = val(ny,j,i)
            dz = val(nz,j,i)
            del = abs(dx*dx+dy*dy+dz*dz)
            if(del > cutoff) then
               numa = numa+1
               xpos(numa) = val(npx,j,i) 
               ypos(numa) = val(npy,j,i) 
               zpos(numa) = val(npz,j,i) 
               disp(numa) = del
               mid(numa) = id(j)
            endif

            do k= 1, j-1
               if(j == k) cycle
               dx = abs(val(npx,j,i)-val(npx,k,i))
               if(dx > cutoff) cycle
               dy = abs(val(npy,j,i)-val(npy,k,i))
               if(dy > cutoff) cycle
               dz = abs(val(npz,j,i)-val(npz,k,i))
               if(dz > cutoff) cycle
                  
               dist = dsqrt(dx*dx+dy*dy+dz*dz)
               if(dist <= cutoff) then
                  delx = val(nx,j,i)-val(nx,k,i)
                  dely = val(ny,j,i)-val(ny,k,i)
                  delz = val(nz,j,i)-val(nz,k,i)
                  del = dsqrt(delx*delx+dely*dely+delz*delz)
                  if(abs(del) > move) then
                     numn = numn+1
                     write(*,'(I5,1X,I6,1X,I6,1X,3(F8.4))')
     $                    numn, j, k, delx, dely, delz
                     nposx(numn) = 0.5d0*(val(npx,j,i)+val(npx,k,i))
                     nposy(numn) = 0.5d0*(val(npy,j,i)+val(npy,k,i))
                     nposz(numn) = 0.5d0*(val(npz,j,i)+val(npz,k,i))
                     ddx(numn) = abs(delx)
                     ddy(numn) = abs(dely)
                     ddz(numn) = abs(delz)
                     dd(numn) = abs(del)
                     pair1(numn) = j
                     pair2(numn) = k
                  endif
               end if
            end do
         end do

         write(30,'(A)')'ITEM: TIMESTEP'
         write(30,'(I0)')i
         write(30,'(A)')'ITEM: NUMBER OF ATOMS'
         write(30,'(I0)')numn
         write(30,'(A)')dummy2(i)
         write(30,'(F0.6,1X,F0.6)')cell_lo(1,i), cell_hi(1,i)
         write(30,'(F0.6,1X,F0.6)')cell_lo(2,i), cell_hi(2,i)
         write(30,'(F0.6,1X,F0.6)')cell_lo(3,i), cell_hi(3,i)
         write(30,'(A)')'ITEM: ATOMS id type type x y z dd ddx ddy ddz'
         do j=1, numn
            write(30,'(I0,1X,I0,1x,I0,1X,7(F0.6,1X))')
     $           j,pair1(j),pair2(j),nposx(j),nposy(j),nposz(j),
     $           dd(j),ddx(j), ddy(j), ddz(j)
         end do

         write(40,'(A)')'ITEM: TIMESTEP'
         write(40,'(I0)')i
         write(40,'(A)')'ITEM: NUMBER OF ATOMS'
         write(40,'(I0)')numa
         write(40,'(A)')dummy2(i)
         write(40,'(F0.6,1X,F0.6)')cell_lo(1,i), cell_hi(1,i)
         write(40,'(F0.6,1X,F0.6)')cell_lo(2,i), cell_hi(2,i)
         write(40,'(F0.6,1X,F0.6)')cell_lo(3,i), cell_hi(3,i)
         write(40,'(A)')'ITEM: ATOMS id type x y z disp'
         do j=1, numa
            write(40,'(I0,1X,I0,1X,4(F0.6,1X))')
     $           j,mid(j),xpos(j),ypos(j),zpos(j),disp(j)
         end do
         write(*,*)'finish data',i
      end select

C      write(30,*)'ITEM: TIMESTEP'
C      write(30,*)i
C      write(30,*)'ITEM: NUMBER OF ATOMS'
C      write(30,*)num
C      write(30,*)dummy2(get_num)
C      write(30,*)cell_lo(1,get_num), cell_hi(1,get_num)
C      write(30,*)cell_lo(2,get_num), cell_hi(2,get_num)
C      write(30,*)cell_lo(3,get_num), cell_hi(3,get_num)
C      write(30,*)dummy3(get_num)
CC     do i=1, num
C         do j=1, num
C            if(i == j) cycle
C            dx = pos(


         
 99   stop
      end
