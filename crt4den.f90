      program crt4den
        !###### creat file for denchar.fdf

      implicit none
      integer i
      integer nu_sp
      integer status, rename
      real cell_x, cell_y, cell_z
      character(32) filename, filename1 ! .fdf & .STRUCT_OUT
      character(32) c_cell_x,c_cell_y,c_cell_z
      character(128)dummy,tab
      logical chk_fil

        write(*,*)''
        write(*,*)'Running...   Creating denchar.fdf file',&
        'Version--1.3 beta//Apr/8/2019'


        call get_command_argument(1,filename)
        ! get a filename for other files
        ! input file name is needed
        ! .fdf info and .STRUCT_OUT info

        if(len_trim(filename)==0)then
        write(*,*)''
        write(*,*)'Lack of .fdf input file, please check again!'
        write(*,*)'Need: atoms .etc info and cell vectors info'
        write(*,*)''
        goto 9999
        elseif&
        (filename((len_trim(filename)-3):len_trim(filename))/='.fdf')then
        write(*,*)''
        write(*,*)'Please input a .fdf file, bye...'
        write(*,*)''
        goto 9999
        else
        endif

      inquire&
      (file=''//filename(1:(len_trim(filename)-4))//'.selected.WFSX',&
      exist=chk_fil)
      if(.not.chk_fil)then
        write(*,*)''
        write(*,*)'Error : File *.selected.WFSX is not exist.'
        write(*,*)''
        goto 9999
      else
      endif

      filename1=''//filename(1:len_trim(filename)-4)//'.STRUCT_OUT'

      open(10,file=filename1, status='old',&
        err=999, form='formatted', access='sequential')
      ! .STRUCT_OUT for cell vectors
      open(20,file=filename, status='old',&
        err=998, form='formatted', access='sequential')
      ! .fdf for info
      open(30,file='denchar.fdf', status='unknown',&
        err=997, form='formatted', access='sequential')
      ! denchar.fdf

      do i=1,3
      read(20,'(a)')dummy
      write(30,'(a)')trim(dummy)
      enddo
      read(20,'(a17,i)')dummy,nu_sp ! a17 has a15 words and 2 tabs
      write(30,'(a,i0)')trim(dummy),nu_sp
      do i=1,2
      read(20,'(a)')dummy
      write(30,'(a)')trim(dummy)
      enddo

      do i=1,nu_sp
      read(20,'(a)')dummy
      write(30,'(a)')trim(dummy)
      enddo

      do i=1,2
      read(20,'(a)')dummy
      write(30,'(a)')trim(dummy)
      enddo

      do i=1,4
      read(20,'(a)')dummy
      enddo

      do i=1,7
      read(20,'(a)')dummy
      write(30,'(a)')trim(dummy)
      enddo

      read(20,'(a)')dummy

      read(10,'(7x,f11.6)')cell_x
      read(10,'(25x,f11.6)')cell_y
      read(10,'(43x,f11.6)')cell_z
      write(c_cell_x,'(f11.6)')cell_x
      write(c_cell_y,'(f11.6)')cell_y
      write(c_cell_z,'(f11.6)')cell_z

      write(30,'(3(f11.6),a)')cell_x,cell_y,cell_z,&
        ' 90.0000 90.0000 90.0000'

      do i=1,10
      read(20,'(a)')dummy
      write(30,'(a)')trim(dummy)
      enddo
      
      !############################################################
      write(30,'(a)')'#############################################'
      write(30,'(a)')'Denchar.TypeOfRun 3D'
      write(30,'(a)')'Denchar.PlotCharge  T'
      write(30,'(a)')'Denchar.PlotWaveFunctions T'
      write(30,'(a)')'Denchar.CoorUnits Ang'
      write(30,'(a)')'Denchar.DensityUnits  Ele/Ang**3'
      write(30,'(a)')''
      write(30,'(a)')'Denchar.NumberPointsX 100'
      write(30,'(a)')'Denchar.NumberPointsY 100'
      write(30,'(a)')'Denchar.NumberPointsZ 100'
      write(30,'(a)')''
      write(30,'(a)')'#############################################'
      write(30,'(a)')'Denchar.MinX  0.0 Ang'
      write(30,'(3a)')'Denchar.MaxX  +',adjustl(trim(c_cell_x)),' Ang'
      write(30,'(a)')'Denchar.MinY  0.0 Ang'
      write(30,'(3a)')'Denchar.MaxY  +',adjustl(trim(c_cell_y)),' Ang'
      write(30,'(a)')'Denchar.MinZ  0.0 Ang'
      write(30,'(3a)')'Denchar.MaxZ  +',adjustl(trim(c_cell_z)),' Ang'
      write(30,'(a)')''
      write(30,'(a)')'#############################################'
      write(30,'(a)')'Denchar.PlaneGeneration NormalVector'
      write(30,'(a)')''
      write(30,'(a)')'%block Denchar.CompNormalVector'
      write(30,'(a)')' 0.000 0.000 1.000'
      write(30,'(a)')'%endblock Denchar.CompNormalVector'
      write(30,'(a)')''
      write(30,'(a)')'%block Denchar.PlaneOrigin'
      write(30,'(a)')' 0.000 0.000 0.000'
      write(30,'(a)')'%endblock Denchar.PlaneOrigin'
      write(30,'(a)')''
      write(30,'(a)')'%block Denchar.X-Axis'
      write(30,'(a)')' 1.000 0.000 0.000'
      write(30,'(a)')'%endblock Denchar.X-Axis'
      write(30,'(a)')''

        write(*,*)''
        write(*,*)'File completed...'          
        write(*,*)''

        status=rename&
        (''//filename(1:(len_trim(filename)-4))//'.selected.WFSX',&
        ''//filename(1:(len_trim(filename)-4))//'.WFSX')
        write(*,*)'File *.WFSX is copied from *.selected.WFSX'
        write(*,*)''

      close(10)
      close(20)
      close(30)
      goto 9999

999   continue
      write(*,*)''
      write(*,*)'Error: Cannot find *.STRUCT_OUT file ',trim(filename1)
      write(*,*)''
      goto 9999

998   continue
      write(*,*)''
      write(*,*)'Error: Cannot find *.fdf file ',trim(filename)
      write(*,*)''
      goto 9999

997   continue
      write(*,*)''
      write(*,*)'Error: Cannot open/creat denchar.fdf file'
      write(*,*)''
      goto 9999

9999    stop
        endprogram crt4den
