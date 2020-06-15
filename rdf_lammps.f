      program rdf_lammps

      integer*4 maxa, maxr
      parameter (maxa=100000, maxr=500)

      integer*4 num_dat, numbc, numbs, data1, data2, n_data
      integer*4 i, step, num, at, id(maxa), num_at(20), num_ele
      integer*4 isa1(20), isa2(20), isa1c, isa2c, isa1s(20), isa2s(20)
      integer*4 version, center, ele, ele_kind, surround, vm, intval
      integer*4 ptp, type, flag, ans, eval_step, num_eval, ex, ey, ez
      integer*4 e_step(11)
      real*8    min0(3), max0(3), cell0(3), cell(3,maxr)
      real*8    min(3,maxr), max(3,maxr)
      real*8    xpos0(maxa), ypos0(maxa), zpos0(maxa)
      real*8    xpos(maxa,maxr), ypos(maxa,maxr), zpos(maxa,maxr)
      real*8    cutoff, dlreng, ncut, eval_dis(10), ref_vol
      character*64 dummy, filename, input_file
      common /pos/ xpos0, ypos0, zpos0, xpos, ypos, zpos, cell0, cell
      common /set/ isa1, isa2, isa1c, isa2c, isa1s, isa2s, ele_kind, ptp

      call getarg(1,filename)
      open(10,FILE=filename, form='formatted', status='old',
     $     err=99, access='sequential')

      do i=1, 20
         num_at(i) = 0
      end do
      do i=1, 11
         e_step(i) = 0
      end do

C-----Read structural data-----C

 50   read(10,'(A64)',end=100)dummy
      read(10,*)step
      read(10,'(A64)')dummy
      read(10,*)num
      write(*,*)'number of atoms =',num
      read(10,'(A64)')dummy
      if(step == 0) then
         num_dat = 0
         do i=1, 3
            read(10,*)min0(i), max0(i)
            cell0(i) = max0(i)-min0(i)
            write(*,*)'cell0(',i,')=',cell0(i)
         end do
         read(10,'(A64)')dummy
         do i=1, num
            read(10,*)at,id(at),xpos0(at),ypos0(at),zpos0(at)
C            write(*,*)i,at, xpos0(at),ypos0(at),zpos0(at)
            num_at(id(at)) = num_at(id(at))+1
            if(i==1) then
               ele = id(at)
            else if( ele <= id(at) ) then
               ele = id(at)
            endif
         end do
      else
         num_dat = num_dat+1
         do i=1, 3
            read(10,*)min(i,num_dat), max(i,num_dat)
            cell(i,num_dat) = max(i,num_dat)-min(i,num_dat)
         end do
         write(*,'(I5,1X,3(F10.5))')num_dat,(cell(j,num_dat),j=1,3)
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

      write(*,*)'Number of data =',num_dat
      write(*,*)' ID Number of atoms'
      do i=1, 20
         if(num_at(i) /= 0)then
            if(i==1) then
               isa1(1) = 1
               isa2(1) = num_at(1)
            else
               isa1(i) = isa2(i-1)+1
               isa2(i) = isa1(i)+num_at(i)-1
            endif
            write(*,'(1X,I2,1X,I10,5X,I5,A2,I5)')
     $           i, num_at(i), isa1(i), '->', isa2(i)
         else
            num_ele = i-1
            exit
         endif
      end do

C-----Setting the conditions for rdf analysis-----C
      
      write(*,*)'Please select Atom version or Number verision.'
      write(*,*)'ID version (1) or Atom Number version (2) ?'
      read(5,*) version

      numbs = 0
      if(version == 1) then
         write(*,*)'Please input ID numer of atom species at center.'
         read(5,*) center
         write(*,*)'How many elements do you consider ?'
         read(5,*) ele_kind
         do i=1, ele_kind
            write(*,*)'Please input ID number of surrounding atoms.'
            read(5,*) surround
            isa1s(i) = isa1(surround)
            isa2s(i) = isa2(surround)
            numbs = numbs+isa2(surround)-isa1(surround)+1
         end do
         do i=1, ele_kind
            write(*,*) 'isa1s(',i,')= ',isa1s(i)
            write(*,*) 'isa2s(',i,')= ',isa2s(i)
         end do
         write(*,*) 'numbs= ',numbs
         
         numbc = isa2(center)-isa1(center)+1
         isa1c = isa1(center)
         isa2c = isa2(center)
            
      else if(version==2) then
         write(*,*)'Please input start number of atom at center.'
         read(5,*) isa1c
         write(6,*)'Please input end number of atom at center.'
         read(5,*) isa2c
         write(6,*)'Please input start number of surrounding atoms.'
         read(5,*) isa1s(1)
         write(6,*)'Please input end number of surrounding atoms.'
         read(5,*) isa2s(1)
         numbc = isa2c - isa1c + 1
         numbs = isa2s(1) - isa1s(1) + 1
         ele_kind = 1
      endif

C      write(*,*)'Do you need to expand the cell parameters?'
C      write(*,*)'x-direction; (1) yes or (2) no'
C      read(5,*)ex
C      write(*,*)'y-direction; (1) yes or (2) no'
C      read(5,*)ey
C      write(*,*)'z-direction; (1) yes or (2) no'
C      read(5,*)ez

      write(*,*)'Data of atomic position in the file'
      write(*,*)'(1) absolute position or (2) partial position'
      read(5,*) ptp

      write(*,*)'Please input the cut off distance(A).'
      read(5,*) cutoff
      
      write(*,*)'Please input d-length(A).'
      read(5,*) dlreng

      write(*,*)'Number of data :', num_dat
      write(*,*)'Please input initial data number for the analysis.'
      write(*,*)'If you want to calculate rdf at the 0-step structre,'
      write(*,*)'please enter the value of 0. '
      read(5,*) data1
      if(data1 < 0 .or. data1 > num_dat) then
         write(*,*)'No!! invalid value.'
         go to 99
      endif
      write(*,*)'Please input end data number.'
      read(5,*) data2
      if(data2 < 0 .or. data2 > num_dat) then
         write(*,*)'No!! invalid value.'
         go to 99
      endif
      write(*,*)'Please input the interval value for the analysis.'
      read(5,*) intval

      j = 0
      do i=data1, data2
         j = j+1
         if(i==data1) then
            e_step(1)= data1
         else
            e_step(j)= data1+intval*(j-1)
         endif
         if(e_step(j) == data2) then
            n_data = j
            exit
         else if(e_step(j) > data2) then
            n_data = j-1
            exit
         endif
      end do

C      n_data = (data2-data1)/intval
C      do i=1, n_data
C         e_step(i) = data1+i-1
C      end do

      write(*,*)'Volume Modification: '
      write(*,*)'Do you need to modify the volume (1) yes or (2) no'
      read(5,*) vm
      if(vm == 2) then
         ncut = 1.0d0
         ref_vol = 0.0d0
      else
         write(*,*)'Please input the value of reference volume'
         read(5,*) ref_vol
      end if

C      write(*,*)'Rdf values of each atoms at defined distances,'
C      write(*,*)'Do you need the rdf values? (1) yes or (2) no'
C      read(5,*) flag
C      if(flag == 1) then
C         write(*,*)'Which step do you evaluate?'
C         read(5,*) eval_step
C         write(*,*)'How many evaluation points?'
C         read(5,*) num_eval
C         do i=1, num_eval
C            write(*,*)'#',i,'Setting distance value for the evaluation.'
C            read(5,*) eval_dis(i)
C         end do

         open(70,file='atomrdf.dat',status='unknown',
     $        err=99, access='sequential',form='formatted')
         write(70,'(A14)')'ITEM: TIMESTEP'
         write(70,'(I4)')data1
         write(70,'(A21)')'ITEM: NUMBER OF ATOMS'
         write(70,'(I6)')num
         write(70,'(A16)')'ITEM: BOX BOUNDS'
         if(data1 == 0) then
            write(70,'(F11.5,1X,F11.5)')min0(1), max0(1)
            write(70,'(F11.5,1X,F11.5)')min0(2), max0(2)
            write(70,'(F11.5,1X,F11.5)')min0(3), max0(3)
         else
            write(70,'(F11.5,1X,F11.5)')
     $           min(1,data1), max(1,data1)
            write(70,'(F11.5,1X,F11.5)')
     $           min(2,data1), max(2,data1)
            write(70,'(F11.5,1X,F11.5)')
     $           min(3,data1), max(3,data1)
         endif
         write(70,'(A26,1X,10(I3,1X))')
     $        'ITEM: ATOMS id type x y z',(e_step(i),i=1,n_data)
C      else
C         eval_step = 0
C         num_eval = 0
C         eval_dis = 0.0d0
C      endif

      call calc(cutoff,dlreng,numbc,numbs,data1,data2,ncut,ref_vol,
     $     e_step,n_data,num,id)
      
 99   stop
      end

C-----Calculation-----C

      subroutine calc(cutoff,dlreng,numbc,numbs,data1,data2,ncut,
     $     ref_vol,e_step,n_data,num,id)
      
      integer*4 maxa, maxr
      parameter (maxa=100000,  maxr=500)
      real*8    pi
      parameter (pi=3.1415926535)

      integer*4 numbc, numbs, data1, data2, num, id(maxa)
      integer*4 i, j, jj, k, kk, n, l, iatom, jatom, count, num_atom
      integer*4 nijdr(2000), ngijdr, ntijdr(2000), nij(maxa,2000)
      integer*4 isa1(20), isa2(20), isa1c, isa2c, isa1s(20), isa2s(20)
      integer*4 ele_kind, ptp, flag, eval_step, num_eval, ct
      integer*4 it, surr(maxa,10), n_data, e_step(11)
      real*8    cutoff, dlreng, ref_vol, ncut
      real*8    min, max, cell0(3), cell(3,maxr)
      real*8    xpos0(maxa), ypos0(maxa), zpos0(maxa)
      real*8    xpos(maxa,maxr), ypos(maxa,maxr), zpos(maxa,maxr)
      real*8    lmin, gijdr(2000), aijdr(2000), sum, dist, distan
      real*8    r, r1, rr, aij, eval_dis(10), rdf(maxa,10)
      real*8    xp, yp, zp, rdf_sum(maxa)
      character*64 dummy

      common /pos/ xpos0, ypos0, zpos0, xpos, ypos, zpos, cell0, cell
      common /set/ isa1, isa2, isa1c, isa2c, isa1s, isa2s, ele_kind, ptp


      open(50,file='rdf.dat',status='unknown',
     $     err=399,access='sequential',form='formatted')
      open(60,file='surround.dat',status='unknown',
     $     err=399,access='sequential',form='formatted')
      

      write(*,*)'Ni(total) = ', numbc
      write(*,*)'Nj(total) = ', numbs
      write(*,*)'number of eval dat =', n_data
      write(*,*)'eval_step =', (e_step(i),i=1,n_data)
C      do i=1, num_eval
C         write(*,*)'eval_dis =', eval_dis(i)
C      end do

      do i = 1, 2000
         gijdr(i) = 0.0
         ntijdr(i) = 0.0
         do j=1, num
            nij(j,i) = 0
         end do
      end do

      do i=1, num
         rdf_sum(i) = 0.0d0
         do j=1, 10
            rdf(i,j) = 0.0d0
            surr(i,j) = 0
         end do
      end do

      do l=1, n_data
         i = e_step(l)
         if(i==0) then
            lmin = dmin1(cell0(1),cell0(2),cell0(3))/2.0
            if(ref_vol /= 0.0d0) then
               vol = ref_vol
            else
               vol = cell0(1)*cell0(2)*cell0(3)
            endif
         else
            lmin = dmin1(cell(1,i),cell(2,i),cell(3,i))/2.0
            if(ref_vol /= 0.0d0) then
               vol = ref_vol
            else
               vol = cell(1,i)*cell(2,i)*cell(3,i)
            endif
         endif

         if(lmin < 5.0d0) lmin = 5.0d0
         ngijdr = lmin/dlreng
         if(ngijdr > 2000) ngijdr = 2000

C         write(*,*)'Table Size = ', ngijdr
C         write(*,*)'Max Size = 2000'
         write(*,*) 'data = ', i, ' vol=', vol
         
         do n=1, ngijdr
            nijdr(n) = 0
         end do

         do iatom = isa1c, isa2c
            do j=1, ele_kind
               do jatom = isa1s(j), isa2s(j)
                  if(iatom /= jatom) then 
                     dist = distan(iatom,jatom,i,ptp)
                     if(dist <= cutoff)then
                        surr(iatom,l) = surr(iatom,l)+1
                     endif
C                     write(*,*)iatom,jatom,dist
                     if(dist <= lmin) then
                        nijdr(dist/dlreng+1) = nijdr(dist/dlreng+1) + 1
                        nij(iatom,dist/dlreng+1) = 
     $                       nij(iatom,dist/dlreng+1)+1
C                        if(iatom == 1) then
C                           if(dist < cutoff) then
C                              write(*,*)jatom, 'dist=',dist
C                              write(*,*)'dist/dlreng+1=',dist/dlreng+1
C                              write(*,*)'i=',iatom,'j=',jatom
C                              write(*,*)'nij=',nij(iatom,dist/dlreng+1)
C                           end if
C                        endif
                     endif
                  end if
               end do
            enddo
            if((i == eval_step) .and. (flag == 1)) then
               do jj= 1, num_eval
C                  write(*,*)iatom,' eval_dis=',eval_dis(jj)
                  kk = nint((eval_dis(jj)+dlreng*0.5)/dlreng)
C                  write(*,*)'kk=', kk
                  r = kk*dlreng - dlreng*0.5d0
                  rdf(iatom,jj) = 
     $                 dble(nij(iatom,kk))/(4*pi*r*r*dlreng*(numbs/vol))
                  rdf_sum(iatom) = rdf_sum(iatom)+rdf(iatom,jj)
C                  write(*,*)iatom, rdf(iatom,jj)
               end do
            endif
         end do
         count = 0
         aij = 0.0
         ct = 1
         do k = 1, ngijdr
            r = k*dlreng - dlreng*0.5d0
            gijdr(k) = gijdr(k) + dble(nijdr(k))/ 
     $           (4*pi*r*r*dlreng*(numbs/vol))/numbc/n_data
            ntijdr(k) = ntijdr(k) + nijdr(k)
            aijdr(k) = aijdr(k) + dble(nijdr(k))/numbc/n_data
C            write(*,*)'r=',r,' aij=',aij
            aij = aij+dble(nijdr(k-1))/numbc
         end do
      end do

      do m = 1, ngijdr
         r = m*dlreng - dlreng*0.5d0
         sum = sum + aijdr(m)
         write(50,11) r, gijdr(m), ntijdr(m), aijdr(m), sum
 11      format(2F15.7,I10,2F15.7)
      end do
      
C      if(flag == 1) then
         do i=1, num
            if(data1 == 0) then
               if(ptp == 2) then
                  xp = xpos0(i)*cell0(1)
                  yp = ypos0(i)*cell0(2)
                  zp = zpos0(i)*cell0(3)
               else
                  xp = xpos0(i)
                  yp = ypos0(i)
                  zp = zpos0(i)
               endif
            else
               if(ptp == 2) then
                  xp = xpos(i,data1)*cell(1,data1)
                  yp = ypos(i,data1)*cell(2,data1)
                  zp = zpos(i,data1)*cell(3,data1)
               else
                  xp = xpos(i,data1)
                  yp = ypos(i,data1)
                  zp = zpos(i,data1)
               endif
            endif
            write(70,12) i, id(i),xp, yp, zp, (surr(i,j),j=1,n_data)
 12         format(I6,1X,I2,1X,3F10.5,10I3)
         end do
C      endif
      close(50)
      close(60)

      return

 99   write(*,*)'d-length is too small.'
 399  stop

      end

C-----calculation of distance between i and j-----C

      function distan(nai,naj,step,pos_type,ex,ey,ez)

      integer*4 maxa, maxr
      parameter (maxa=100000, maxr=500)

      integer*4 nai, naj, step, pos_type
      real*8    distan
      real*8    xpos0(maxa), ypos0(maxa), zpos0(maxa)
      real*8    xpos(maxa,maxr), ypos(maxa,maxr), zpos(maxa,maxr)
      real*8    cell0(3), cell(3,maxr)
      real*8    xi, yi, zi, xj, yj, zj, a, b, c
      real*8    dx, dy, dz, rx, ry, rz

      common /pos/ xpos0, ypos0, zpos0, xpos, ypos, zpos, cell0, cell

      if(step == 0) then
         if(pos_type == 1) then
            xi = xpos0(nai)/cell0(1)
            yi = ypos0(nai)/cell0(2)
            zi = zpos0(nai)/cell0(3)
            xj = xpos0(naj)/cell0(1)
            yj = ypos0(naj)/cell0(2)
            zj = zpos0(naj)/cell0(3)
         else
            xi = xpos0(nai)
            yi = ypos0(nai)
            zi = zpos0(nai)
            xj = xpos0(naj)
            yj = ypos0(naj)
            zj = zpos0(naj)
         endif
         a  = cell0(1)
         b  = cell0(2)
         c  = cell0(3)
      else
         if(pos_type == 1) then
            xi = xpos(nai,step)/cell(1,step)
            yi = ypos(nai,step)/cell(2,step)
            zi = zpos(nai,step)/cell(3,step)
            xj = xpos(naj,step)/cell(1,step)
            yj = ypos(naj,step)/cell(2,step)
            zj = zpos(naj,step)/cell(3,step)
         else
            xi = xpos(nai,step)
            yi = ypos(nai,step)
            zi = zpos(nai,step)
            xj = xpos(naj,step)
            yj = ypos(naj,step)
            zj = zpos(naj,step)
         endif
         a  = cell(1,step)
         b  = cell(2,step)
         c  = cell(3,step)
      endif
      
      dx = xj-xi
      dy = yj-yi
      dz = zj-zi

      if(dx < -0.5d0) then
         xj = xj + 1.0d0
      else if(dx > 0.5d0) then
         xj = xj - 1.0d0
      else
         xj = xj
      end if

      if(dy < -0.5d0) then
         yj = yj + 1.0d0
      else if(dy > 0.5d0) then
         yj = yj - 1.0d0
      else
         yj = yj
      end if

      if(dz < -0.5d0) then
         zj = zj + 1.0d0
      else if(dz > 0.5d0) then
         zj = zj - 1.0d0
      else
         zj = zj
      end if

      rx = (xj-xi)*a
      ry = (yj-yi)*b
      rz = (zj-zi)*c

      distan = dsqrt(rx*rx+ry*ry+rz*rz)

      return
      end






