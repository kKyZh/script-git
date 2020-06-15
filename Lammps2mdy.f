      program Lammps2mdy

      parameter (maxa=40000, maxr=3001)
      integer*4 i, j, k, types, id(40000), num, at
      integer*4 pos2000(maxa,3), count, num_at(20), cloop
      integer*4 count1, count2, num_dat, step
      real*8    new_cell(3), cell0(3), cell(3,maxr), pos(maxa,3)
      real*8    min(3), max(3)
      real*8    xpos0(maxa), ypos0(maxa), zpos0(maxa)
      real*8    xpos(maxa,maxr), ypos(maxa,maxr)
      real*8    zpos(maxa,maxr), charge(maxa)
      real*8    avbox(3), avmsd(20), avsptemp(20), avpalag(8)
      real*8    avpress, avtens(6), avtemp, steps
      real*8    new_pos(maxa,3), mass(16), px, py, pz
      character*64 dummy
      character*2 name(20)
      character*64 filename

      call getarg(1,filename)
      open(10,FILE=filename, form='formatted', status='old',
     $     err=99, access='sequential')
      open(15,FILE='xtal.mdy', form='formatted', status='unknown',
     $     access='sequential')
      open(20,FILE='pos.mdy', form='formatted', status='unknown',
     $     access='sequential')
      open(30,FILE='system.mdy', form='formatted', status='unknown',
     $     access='sequential')
      open(40,FILE='charge.mdy', form='formatted', status='unknown',
     $     access='sequential')
      open(50,FILE='input.mdy',form='formatted', status='unknown',
     $     access='sequential')

C-----Initialize for system.mdy-----C
      avbox(1) = 0.0d0
      avbox(2) = 0.0d0
      avbox(3) = 0.0d0

      do i = 1, aspec
         avmsd(i) = 0.0d0
         avsptemp(i) = 0.0d0
      end do

      do i = 1, 8
         avpalag(i) = 0.0d0
      end do

      avpress = 0.0d0

      do i = 1, 6
         avtens(i) = 0.0d0
      end do

      avtemp = 0.0d0
      
      do i=1, maxa
         charge(i) = 0.0d0
      end do

C-----read data from dump-----C

      num_dat = 0
 50   read(10,'(A64)',end=100)dummy
      read(10,*)step
      write(*,*) step
      read(10,'(A64)')dummy
      read(10,*)num
      write(*,*) num
      read(10,'(A64)')dummy
      if(step == 0) then
         do i=1, 3
            read(10,*)min(i), max(i)
            cell0(i) = abs(max(i)-min(i))
         end do
         read(10,'(A64)')dummy
         do i=1, num
            read(10,*)at,id(at),xpos0(at),ypos0(at),zpos0(at)
C            if(min(1) < 0.0d0)xpos0(at) = xpos0(at)+abs(min(1)/cell0(1))
C            if(min(2) < 0.0d0)ypos0(at) = ypos0(at)+abs(min(2)/cell0(2))
C            if(min(3) < 0.0d0)zpos0(at) = zpos0(at)+abs(min(3)/cell0(3))
            if(xpos0(at) < 0.0d0) xpos0(at) = xpos0(at)+1.0d0
            if(ypos0(at) < 0.0d0) ypos0(at) = ypos0(at)+1.0d0
            if(zpos0(at) < 0.0d0) zpos0(at) = zpos0(at)+1.0d0
            num_at(id(at)) = num_at(id(at))+1
            pos2000(at,1) = int(xpos0(at)*2000.0d0)
            pos2000(at,2) = int(ypos0(at)*2000.0d0)
            pos2000(at,3) = int(zpos0(at)*2000.0d0)
         end do
         
         ele_num = id(num)
         do i=1, ele_num
            write(*,'(A26,I3,1X,A7)')
     $           'Please enter the name of #',i,'element'
            read(5,*) name(i)
         end do

         write(15,'(A5)')'TITLE'
         write(15,'(A63)') 
     $ 'Charge  0.0  Spin 0.0  VSIP 2  Ratio 100  Smear 0.10000
     $ MoEne 0'
         write(15,'(3(F10.5))')cell0(1), cell0(2), cell0(3)
         do i=1, num
            write(15,'(1X,A2,1X,I2,4X,3(F10.5))')
     $        name(id(i)),id(i),xpos0(i),ypos0(i),zpos0(i)
         end do

         write(20,'(18(I4))') ((pos2000(i,j),j=1,3),i=1,num)
         
         aspec = id(num)
         count = 0
         write(30,3000) count, cell0(1), cell0(2), cell0(3) 
         write(30,3050) count, avbox(1), avbox(2), avbox(3) 
         write(30,3100) count, avmsd(1), avmsd(2), avmsd(3), 
     &        avmsd(4), avmsd(5)
         if(aspec >= 6) then
            write(30,3200) count, avmsd(6), avmsd(7), avmsd(8), 
     &           avmsd(9), avmsd(10)
            if(aspec >= 11) then
               write(30,3210) count, avmsd(11), avmsd(12), avmsd(13),
     &              avmsd(14), avmsd(15)
               if(aspec >= 16) then
                  write(30,3220) count, avmsd(16), avmsd(17), avmsd(18),
     &                 avmsd(19), avmsd(20)
               endif
            endif
         endif
         write(30,3300) count, avpalag(1), avpalag(2), avpalag(3),
     &        avpalag(4), avpalag(5), avpalag(6), avpalag(7), 
     &        avpalag(8)

         write(30,3400) count, avpress, avtens(1), avtens(2), avtens(3),
     &        avtens(4), avtens(5), avtens(6)
         
         write(30,3500) count, avtemp, avsptemp(1), avsptemp(2), 
     &        avsptemp(3), avsptemp(4), avsptemp(5)

         if(aspec >= 6) then
            write(30,3600) count, avsptemp(6), avsptemp(7), 
     &           avsptemp(8), avsptemp(9), avsptemp(10)
            if(aspec >= 11) then
               write(30,3610) count, avsptemp(11), avsptemp(12), 
     &              avsptemp(13), avsptemp(14), avsptemp(15)
               if(aspec >= 16) then
                  write(30,3620) count, avsptemp(16), avsptemp(17), 
     &                 avsptemp(18), avsptemp(19), avsptemp(20)
               endif
            endif
         endif
         
      else
         num_dat = num_dat+1
         do i=1, 3
            read(10,*)min(i), max(i)
            cell(i,num_dat) = abs(max(i)-min(i))
            write(*,*) cell(i,num_dat)
         end do
         read(10,'(A64)')dummy
         do i=1, num
            read(10,*)at,id(at),xpos(at,num_dat), ypos(at,num_dat),
     $           zpos(at,num_dat)
C            if(min(1) < 0.0d0) xpos(at,num_dat) = xpos(at,num_dat) + 
C     $           abs(min(1)/cell(1,num_dat))
C            if(min(2) < 0.0d0) ypos(at,num_dat) = ypos(at,num_dat) + 
C     $           abs(min(2)/cell(2,num_dat))
C            if(min(3) < 0.0d0) zpos(at,num_dat) = zpos(at,num_dat) + 
C     $           abs(min(3)/cell(3,num_dat))
            if(xpos(at,num_dat) < 0.0d0) 
     $           xpos(at,num_dat)=xpos(at,num_dat)+1.0d0
            if(ypos(at,num_dat) < 0.0d0) 
     $           ypos(at,num_dat)=ypos(at,num_dat)+1.0d0
            if(zpos(at,num_dat) < 0.0d0) 
     $           zpos(at,num_dat)=zpos(at,num_dat)+1.0d0
            pos2000(at,1) = int(xpos(at,num_dat)*2000.0d0)
            pos2000(at,2) = int(ypos(at,num_dat)*2000.0d0)
            pos2000(at,3) = int(zpos(at,num_dat)*2000.0d0)
         end do
         write(20,'(18(I4))') ((pos2000(i,j),j=1,3),i=1,num)

         count = num_dat
         write(*,*) count, cell(1,count),cell(2,count),cell(3,count)
         write(30,3000)count,cell(1,count),cell(2,count),cell(3,count) 
         write(30,3050) count, avbox(1), avbox(2), avbox(3) 
         write(30,3100) count, avmsd(1), avmsd(2), avmsd(3), 
     &        avmsd(4), avmsd(5)
         if(aspec >= 6) then
            write(30,3200) count, avmsd(6), avmsd(7), avmsd(8), 
     &           avmsd(9), avmsd(10)
            if(aspec >= 11) then
               write(30,3210) count, avmsd(11), avmsd(12), avmsd(13),
     &              avmsd(14), avmsd(15)
               if(aspec >= 16) then
                  write(30,3220) count, avmsd(16), avmsd(17), avmsd(18),
     &                 avmsd(19), avmsd(20)
               endif
            endif
         endif
         write(30,3300) count, avpalag(1), avpalag(2), avpalag(3),
     &        avpalag(4), avpalag(5), avpalag(6), avpalag(7), 
     &        avpalag(8)

         write(30,3400) count, avpress, avtens(1), avtens(2), avtens(3),
     &        avtens(4), avtens(5), avtens(6)
         
         write(30,3500) count, avtemp, avsptemp(1), avsptemp(2), 
     &        avsptemp(3), avsptemp(4), avsptemp(5)

         if(aspec >= 6) then
            write(30,3600) count, avsptemp(6), avsptemp(7), 
     &           avsptemp(8), avsptemp(9), avsptemp(10)
            if(aspec >= 11) then
               write(30,3610) count, avsptemp(11), avsptemp(12), 
     &              avsptemp(13), avsptemp(14), avsptemp(15)
               if(aspec >= 16) then
                  write(30,3620) count, avsptemp(16), avsptemp(17), 
     &                 avsptemp(18), avsptemp(19), avsptemp(20)
               endif
            endif
         endif
         
      endif

      if(step == 0) then
         if(mod(num,10) == 0) then
            cloop = num/10
         else
            cloop = num/10 + 1
         endif
         
         do i = 1, cloop
            cout1 = 1+(i-1)*10
            cout2 =10+(i-1)*10
            if((i == cloop).and.(mod(num,10) /= 0)) then
               cout2 = mod(num,10)+(i-1)*10
            endif
            write(40,5000) count, (charge(j),j=cout1,cout2)
         end do
      endif

      go to 50

 100  continue
      
C-----making input.mdy-----C

      steps = real(num_dat)
      write(50,1001)
      write(50,1002)
      write(50,1003)
      write(50,'(A35,F7.1)')'Number of Steps for MD (2000 STEP)|',steps
      write(50,1005)
      write(50,1006)
      write(50,1007)
      write(50,1008)
      write(50,1009)
      write(50,1010)
      write(50,1011)
      write(50,1012)
      write(50,1013)
      write(50,1014)
      write(50,1015)
      write(50,1016)
      write(50,1017)
      write(50,1018)
      write(50,1019)
      write(50,1020)
      write(50,1021)
      write(50,1022)
      write(50,1023)
      write(50,1024)
      write(50,1025)
      write(50,1026)
      write(50,1027)
      write(50,1028)
      write(50,1029)
      write(50,1030)
      write(50,1031)
      write(50,1032)
      write(50,1033)
      write(50,1034)
      write(50,1035)
      write(50,1036)
      write(50,1037)
      write(50,1038)
      write(50,1039)
      write(50,1040)
      write(50,1041)
      write(50,1042)

 1001 format("Format Type of This File (18.0)   |   19.0")
 1002 format("Normal(1), OrderN(2), MBE(3),     |    1.0")
 1003 format("MBE-OrderN(4)                     |    ---")
 1004 format("Number of Steps for MD (2000 STEP)| 2000.0")
 1005 format("Output Interval for MD (50 STEP)  |    1.0")
 1006 format("Output Interval for FV (1 STEP)   |    1.0")
 1007 format("Temperature for MD     (300.0 K)  |  300.0")
 1008 format("Delta-t for MD         (2.0 fs)   |    0.2")
 1009 format("Temp. Change Every Interval(0.0 K)|    0.0")
 1010 format("Final Temp. After Change   (0.0 K)|    0.0")
 1011 format("Interval for Temp. Change (0 STEP)|    0.0")
 1012 format("Temp. Ctrl (0) 10 step, (1) every |    1.0")
 1013 format("Unit for Energy Conversion  (1.0) |    1.0")
 1014 format("Number for Randomize   (0.0)      |    0.0")
 1015 format("Output disp.(input) S(0),M(1),D(2)|    0.0")
 1016 format("Output disp.(result)S(0),M(1),D(2)|    0.0")
 1017 format("Output charge.mdy         (0 - 4) |    2.0")
 1018 format("Output band.mdy  NOT(0),E(1),EC(2)|    1.0")
 1019 format("Output force.mdy NOT(0),OUTPUT(1) |    0.0")
 1020 format("Output velo.mdy  NOT(0),OUTPUT(1) |    0.0")
 1021 format("NVE(0),NVT(1),NPH(2),NPT(3),SUR(4)|    3.0")
 1022 format("Pressure X (0.0001 GPa = 1.0 atm) |    0.0001")
 1023 format("Pressure Y (0.0001 GPa = 1.0 atm) |    0.0001")
 1024 format("Pressure Z (0.0001 GPa = 1.0 atm) |    0.0001")
 1025 format("Considered PBC Cell for X  (1.0)  |    1.0")
 1026 format("Considered PBC Cell for Y  (1.0)  |    1.0")
 1027 format("Considered PBC Cell for Z  (1.0)  |    1.0")
 1028 format("Repulsion Term Calz(0), Exp(1)    |    1.0")
 1029 format("Cutoff for S & H (7.5 A)(0.0-Auto)|    0.0")
 1030 format("Coulomb NOT(0),Fix(1),Mulliken(2) |    2.0")
 1031 format("Step Interval for Order-N (10)    |   10.0")
 1032 format("Considering Orbits for Order-N(6) |    6.0")
 1033 format("Force Calc. Single(0) or Multi(1) |    0.0")
 1034 format("Interval of SCF calculation       |    1.0")
 1035 format("Electric Field X         (0.0 V/A)|    0.0")
 1036 format("Electric Field Y         (0.0 V/A)|    0.0")
 1037 format("Electric Field Z         (0.0 V/A)|    0.0")
 1038 format("Charge threshold for SCF calc.(I) |    0.01")
 1039 format("Charge threshold for SCF calc.(II)|    0.3")
 1040 format("gamode                            |    3.0")
 1041 format("surface boundary                  |    1.0")
 1042 format("bind.ene. auto(0), atom(1), ion(2)|    0.0")
      close(18)



 5000 format('Charge:',3x,4x,4x,i5,':',10f6.3)

 3000 format('Cell :',i5,':',3f8.3)
 3050 format('AvCel:',i5,':',3f8.3)
 3100 format('MSD1 :',i5,':',5f8.3)
 3200 format('MSD2 :',i5,':',5f8.3)
 3210 format('MSD3 :',i5,':',5f8.3)
 3220 format('MSD4 :',i5,':',5f8.3)
 3300 format('Enrgy:',i5,':',8f8.3)
 3400 format('Press:',i5,':',7f8.3)
 3500 format('Temp1:',i5,':',6f8.2)
 3600 format('Temp2:',i5,':',5f8.2)
 3610 format('Temp3:',i5,':',5f8.2)
 3620 format('Temp4:',i5,':',5f8.2)


 99   stop
      end
