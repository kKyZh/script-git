      program xyz2fdff90

          !###########################################################
          !this is the post-run after a full
          !relaxation and the position.fdf is totally different from the
          !systemlabel.xyz (the function of writecoorxmol is .true.).
          !#########
          !for whole process
          !function with a xyz2fdf and a merge
          !#########

          implicit none
      integer i,j,k
      integer s_num, tse_num
      integer r2e
      integer rename, status
      real s_x, s_y, s_z
      real tse_x, tse_y, tse_z
      real temp_z
      real temp_x
      character(64) s_dummy, tse_dummy, dummy, dummy1, dummy2 ! read from file and write
      character(128) filename1, filename2, filename3, filename4
      character(128) filename5
      character(128) target_line
      character(256) tmp_tss_dummy
      logical found_line

      write(*,*)''
      write(*,*)'//Apr/10/2019// xyz2fdf after relaxtion for tss'
      write(*,*)''
      call get_command_argument(1,filename1)
      ! filename1 is the inputfile should be systemlable.xyz
      ! $siesta_*.xyz
      call get_command_argument(2,filename2)
      ! $TSelec_*.xyz
      call get_command_argument(3,filename5)
      ! $TSscat_*.fdf for replace the cell vector

      if(len_trim(filename1)==0.or.len_trim(filename2)==0)then
        write(*,*)''
        write(*,*)'Lack of .xyz file, please check again!'
        write(*,*)'.xyz files from SIESTA and TSelect are needed!'
        write(*,*)''
        goto 9999
      else
      endif

      call changename(filename1,filename3)
      call changename(filename2,filename4)
      ! original_position.fdf file will be changed from position.fdf to
      ! original_position.fdf for comparison (first thing is rename)
      ! the last thing is also rename

      open(10,file=filename1, status='old',&
        err=999, form='formatted', access='sequential')
      ! siesta
      open(20,file=filename2, status='old',&
        err=999, form='formatted', access='sequential')
      ! TSelec
      ! read systemlabel.xyz from other script
      open(11,file=filename3, status='old',&
        err=999, form='formatted', access='sequential')
      ! siesta cell vector
      open(21,file=filename4, status='old',&
        err=999, form='formatted', access='sequential')
      ! TSelec cell vector
      ! read cell vectors

      read(11,'(7x,f11.6)')s_x
      read(11,'(25x,f11.6)')s_y
      read(11,'(43x,f11.6)')s_z
      read(21,'(7x,f11.6)')tse_x
      read(21,'(25x,f11.6)')tse_y
      read(21,'(43x,f11.6)')tse_z

      !s_x should be the same value of tse_x if not make both equal the
      !same value at tse_x
      !s_y also should be the same as tse_y when creat the model of two
      !structures, if not check it should be the same or not

      if(s_x/=tse_x)then
        s_x=tse_x
      endif

      !####################### cell vector finished
      
      open(30,file='s_position.fdf', status='old',&
        err=998, form='formatted', access='sequential')
      open(40,file='tse_position.fdf', status='old',&
        err=998, form='formatted', access='sequential')
      open(50,file='tss_position.fdf', status='old',&
        err=997, form='formatted', access='sequential')
      ! s = siesta and tse = transiestaelect and tss = transiestascat

      !----------------------------------------------------------
      !open(999,file='Error_file', status='unknown',&
      !  form='formatted', access='sequential')
      ! add a error_file for checking after finish all content
      !------ for replace the cell vector after full relaxation

      open(60,file=filename5, status='old',&
        err=996, form='formatted', access='sequential')
      open(70,file='tmp_tss_position.fdf', status='unknown',&
        err=997, form='formatted', access='sequential')
      !------

      !###### .xyz total number and a null line
      read(10,'(i)')s_num
      read(10,'(a)')s_dummy
      read(20,'(i)')tse_num
      read(20,'(a)')tse_dummy

      !###### read one .fdf and write new .fdf first line
      read(30,'(a)')dummy
      read(40,'(a)')dummy
      read(50,'(a)')dummy

      write(70,'(a)')trim(dummy)

      ! ts electrode should be at top and bottom for tsscatering
      do i=1,tse_num
      read(20,'(4x,1x,f11.6,a24)')temp_x,dummy1
      read(50,'(36x,(a))')dummy2

      write(70,'(1x,f11.6,(a),(a))')temp_x,trim(dummy1),trim(dummy2)
      enddo

      ! siesta should be at middle for device part of tsscattering
      do i=1,s_num
      read(10,'(4x,a12,a12,1x,f11.6)')dummy, dummy1, temp_z
      read(50,'(36x,(a))')dummy2

      write(70,'(a12,a12,1x,f11.6,(a))')&
        trim(dummy), trim(dummy1),temp_z+tse_z,trim(dummy2)
      enddo

      ! ts electrode should be at top and bottom for tsscatering
      ! backspace to position and read again tse_position
      do i=1,tse_num
      backspace(20)
      enddo

      do i=1,tse_num
      read(20,'(4x,a12,a12,1x,f11.6)')dummy, dummy1, temp_z
      read(50,'(36x,(a))')dummy2

      write(70,'(a12,a12,1x,f11.6,(a))')&
        trim(dummy),trim(dummy1),temp_z+tse_z+s_z,trim(dummy2)
      enddo

      ! the last line
      read(30,'(a)')dummy
      read(40,'(a)')dummy
      read(50,'(a)')dummy
      
      write(70,'(a)')trim(dummy)
      status=rename('tmp_tss_position.fdf','tss_position.fdf')

      !-----------------------------------------------------------
      !###### need to find a new method for replace the specific line in
      !a target file, much easier one.

      j=0 
      do 
      read(60,'(a)',iostat=r2e)target_line
      if(r2e/=0)then
        goto 6000
      else
      j=j+1
      found_line=(index(target_line,'%block LatticeParameters')/=0)
      if(found_line)then
        k=j
      endif
      endif
      enddo

6000  continue
      rewind(60)
      open(80,file='tmp_tss.fdf', status='unknown',&
        err=996, form='formatted', access='sequential')

      do i=1,k
      read(60,'(a)')tmp_tss_dummy
      write(80,'(a)')trim(tmp_tss_dummy)
      enddo

      read(60,'(a)')tmp_tss_dummy
      write(80,'(3(1x,f11.6),a)')s_x,s_y,(tse_z+s_z+tse_z),&
        ' 90.0000 90.0000 90.0000'

      do i=k+2, j
      read(60,'(a)')tmp_tss_dummy
      write(80,'(a)')trim(tmp_tss_dummy)
      enddo

      status=rename('tmp_tss.fdf',''//filename5//'')

      !------------------------------------------------------------
      !###### rename ######
      !status=rename('s_position.fdf','original_s_position.fdf')
      !status=rename('tse_position.fdf','original_tse_position.fdf')
      !turn on after program finished

      close(10)
      close(11)
      close(20)
      close(21)
      close(30)
      close(40)
      close(50)
      close(60)
      close(70)
      close(80)
      goto 9999

999   continue
      write(*,*)''
      write(*,*)'Error: Cannot find file "',trim(filename1),&
        '" or "',trim(filename2),'".'
      write(*,*)'==> Please run SIESTA/TranSIESTA again and let '&
        'WriteCoorXmal = .true.'
      write(*,*)''
      goto 9999

998   continue
      write(*,*)''
      write(*,*)'Error: Cannot find file "s_position.fdf"'&
        ' or tse_position.fdf.'
      write(*,*)'==> Please check it and run it again.'
      write(*,*)''
      goto 9999

997   continue
      write(*,*)''
      write(*,*)'Error: Cannot open/create file "tss_position.fdf".'
      write(*,*)''
      goto 9999

996   continue
      write(*,*)''
      write(*,*)'Error: Cannot open the .fdf file for scattering!'
      write(*,*)''
      goto 9999

995   continue
      write(*,*)''
      write(*,*)'Error: Cannot open the temp.fdf file for scattering!'
      write(*,*)''
      goto 9999

9999  stop

      !###### subroutine for change name (.xyz to .STRUCT_OUT)
      contains

        subroutine changename(in_name,out_name)

          implicit none

          character(32) a
          character(128), intent(in) :: in_name
          character(128) temp_name
          character(128), intent(out) :: out_name

          a='.xyz'
          temp_name=in_name(1:len_trim(in_name)-len_trim(a))
          out_name=''//trim(temp_name)//'.STRUCT_OUT'

        endsubroutine changename

      endprogram xyz2fdff90
