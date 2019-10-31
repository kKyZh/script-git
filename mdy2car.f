      program mdy2car

      integer*4 i, j, k, l, m, n, num
      integer*4 num_ele(20), no(10000), ele
      integer*4 count
      real*8    cell(6), rp(3,10000), posx, posy, posz
      character*64 dummy
      character*64 filename
      character*2  name(10000)
      character*4 namenum(10000), aa
      character*1 tag

      call getarg(1,filename)
      open(10,FILE=filename, form='formatted', status='old',
     $     err=99, access='sequential')
      open(20,FILE='mdy2car.car', form='formatted', status='unknown',
     $     access='sequential')

      cell(1) = 0.0d0
      cell(2) = 0.0d0
      cell(3) = 0.0d0
      cell(4) = 90.0d0
      cell(5) = 90.0d0
      cell(6) = 90.0d0
      do i=1, 20
         num_ele(i) = 0
      end do
      do i=1, 10000
         no(i) = 0
         rp(1,i) =0.0d0
         rp(2,i) =0.0d0
         rp(3,i) =0.0d0
      end do
      count = 0
      do i=1, 2
         read(10,'(A64)') dummy
      end do
      read(10,'(3(F10.6))') cell(1),cell(2),cell(3)
      do i=1,10000
         read(10,5,end=200) 
     $        name(i),no(i),rp(1,i),rp(2,i),rp(3,i)
         count =count+1
      end do

 5    format(1X,A2,1X,I2,4X,3(F10.5))
      
 200  continue
      num = count
      write(*,*)'total number =', num

      do i=1, num
         j = no(i)
         num_ele(j) = num_ele(j)+1
         k=500
         write(aa,'(I4)') num_ele(j)
         namenum(i) = trim(adjustl(name(i)))//trim(adjustl(aa))
         write(*,*) namenum(i)
      end do

      write(20,'(A17)')'!BIOSYM archive 3'
      write(20,'(A6)')'PBC=ON'
      write(20,'(A35)')'Materials Studio Generated CAR File'
      write(20,'(A29)')'!DATE Tue Dec 1 18:58:07 2015'
      write(20,'(A3,6(F10.4),A5)')'PBC', cell(1), cell(2), cell(3), 
     $     cell(4), cell(5), cell(6), ' (P1)'
      do i=1, num
         posx = cell(1)*rp(1,i)
         posy = cell(2)*rp(2,i)
         posz = cell(3)*rp(3,i)
         write(20,'(A4,1X,3(F15.9),A21,A2,A7)') namenum(i), posx, posy, 
     $        posz, ' XXXX 1      xx      ', name(i), '  0.000'
      end do
      write(20,'(A3)')'end'
      write(20,'(A3)')'end'


 99   stop
      end
