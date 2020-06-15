      program average_cell

      integer*4 start, final, step(2000), nums, i
      integer*4 num_run, num_cal, count
      real*8    tmp(2000), press(2000), Px(2000), Py(2000), Pz(2000)
      real*8    Ene(2000), lx(2000), ly(2000), lz(2000)
      real*8    Ttmp, Tpress, TPx, TPy, TPz, TEne, Tlx, Tly, Tlz
      real*8    Atmp, Apress, APx, APy, APz, AEne, Alx, Aly, Alz

      character*64 filename
      character*4 tag

      call getarg(1,filename)
      open(10,FILE=filename, form='formatted',status='old',
     $     err=99,access='sequential')

      num_run = 0
      do
         read(10,'(A4,A64)',end=999)tag,dummy
         if(tag == 'Step') then
            num_run = num_run+1
         endif
      end do

 999  rewind(10)
      write(*,*)'Serial calculation =', num_run
      write(*,*)'Which number do you calculate ?'
      read(5,*) num_cal
      write(*,*)'Please input start number of data'
      read(5,*) start
      write(*,*)'Please input final number of data'
      read(5,*) final

      count = 0
      do
         read(10,'(A4,A64)')tag,dummy
         if(tag == 'Step') then
            count = count+1
            if(count == num_cal)then
               exit
            endif
         endif
      end do

      do i=1, final
         read(10,*)step(i), tmp(i), press(i), Px(i), Py(i), Pz(i), 
     $        Ene(i), lx(i), ly(i), lz(i)
C         write(*,*)step(i), tmp(i), press(i), Px(i), Py(i), Pz(i), 
C     $        Ene(i), lx(i), ly(i), lz(i)
      end do

      do i=start, final
         Ttmp = Ttmp+tmp(i)
         Tpress = Tpress+press(i)
         TPx = TPx+Px(i)
         TPy = TPy+Py(i)
         TPz = TPz+Pz(i)
         TEne = TEne+Ene(i)
         Tlx = Tlx+lx(i)
         Tly = Tly+ly(i)
         Tlz = Tlz+lz(i)
      end do

      nums = final-start+1
      Atmp = Ttmp/nums
      Apress = Tpress/nums
      APx = TPx/nums
      APy = TPy/nums
      APz = TPz/nums
      AEne = TEne/nums
      Alx = Tlx/nums
      Aly = Tly/nums
      Alz = Tlz/nums
      
      write(*,'(F6.1,1X,4(F10.3,1X),F10.3,3(F10.5))')
     $     Atmp,Apress,APx,APy,APz,Aene,Alx,Aly,Alz

      close(10)

 99   stop
      end
