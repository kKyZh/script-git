      program mer4fdf
        implicit none
        integer i,j
        integer nutot_at, nutot_sp
        integer s_nutot_at, tse_nutot_at
        integer s_nutot_sp, tse_nutot_sp
        integer, allocatable :: s_nutot_el(:), tse_nutot_el(:)
        integer, allocatable :: nutot_el(:)
        real, allocatable :: s_cell(:), tse_cell(:)
        real, allocatable :: cell(:)
        character(128) dummy, s_dummy, tse_dummy

      write(*,*)''
      write(*,'(3a)')'Merge s_position.fdf and tse_position.fdf',&
        ' for position.fdf of TranSIESTA (Scattering)',&
        ' /Version-1.2//May/17/2019/'
      write(*,*)''

       open(10, file='s_position.fdf', status='old',&
         err=999, form='formatted', access='sequential')
       open(20, file='tse_position.fdf', status='old',&
         err=998, form='formatted', access='sequential')
       open(30, file='s_getdata.lammps', status='old',&
         err=997, form='formatted', access='sequential')
       open(40, file='tse_getdata.lammps', status='old',&
         err=996, form='formatted', access='sequential')
       open(50, file='position.fdf', status='unknown',&
         err=995, form='formatted', access='sequential')
       open(60, file='getdata.lammps', status='unknown',&
         err=994, form='formatted', access='sequential')

        write(*,*)'------------------------------------------'
        write(*,*)'Running...'
      
        !#########################################################################
        !------------------------- for getdata.lammps
        !pre-read getdata.lammps from 30 and 40 for allocate   
        read(30,'(a)')dummy
        read(30,*)s_nutot_at!total atom
        read(30,*)s_nutot_sp!total element species
        read(40,'(a)')dummy
        read(40,*)tse_nutot_at!total atom
        read(40,*)tse_nutot_sp!total element species
        rewind(30)
        rewind(40)

        nutot_at=s_nutot_at+tse_nutot_at*2 ! total for tss atoms
        nutot_sp=s_nutot_sp+tse_nutot_sp*2 ! total for tss species

        allocate(s_nutot_el(nutot_sp),tse_nutot_el(nutot_sp))
        allocate(nutot_el(nutot_sp))
        allocate(s_cell(3),tse_cell(3))
        allocate(cell(3))

        !pre-read for nutot_el
        !write 60 file
        read(30,'(a)')dummy
        read(30,*)dummy!total atom
        read(30,*)dummy,dummy,dummy,(s_nutot_el(i),i=1,s_nutot_sp)!total element species
        read(40,'(a)')dummy
        read(40,*)dummy!total atom
        read(40,*)dummy,dummy,dummy,(tse_nutot_el(i),i=1,tse_nutot_sp)!total element species

        write(60,'(a)')' # Position data'
        write(60,'(1x,i0,1x,a)')nutot_at,'atoms'
        write(60,'(1x,i0,1x,a,<nutot_sp>(1x,i0))')&
          nutot_sp,'atom types',&
          (tse_nutot_el(i),i=1,tse_nutot_sp),&
          (s_nutot_el(i),i=1,s_nutot_sp),&
          (tse_nutot_el(i),i=1,tse_nutot_sp)

        do i=1,3
        read(30,*)dummy,s_cell(i)
        read(40,*)dummy,tse_cell(i)
        if(i==3)then
        cell(i)=s_cell(i)+tse_cell(i)*2
        else
        cell(i)=tse_cell(i)
        endif
        enddo

        do i=1,3
        write(60,'(a,f11.6,a)')' 0.000000',cell(i),' xlo xhi'
        enddo

        write(*,*)''
        write(*,*)&
          'getdata.lammps created (No coordinates information included)'

        !#########################################################################
        !--------------------- for position.fdf
        read(10,'(a)')dummy
        read(20,'(a)')dummy
        write(50,'(a)')trim(dummy) ! first line
        do i=1,tse_nutot_at
        read(20,'(a)')dummy
        write(50,'(a)')trim(dummy)
        enddo
        do i=1,s_nutot_at
        read(10,'(a)')dummy
        write(50,'(a)')trim(dummy)
        enddo
        !------------ rewind tse_position.fdf
        rewind(20)
        read(20,'(a)')dummy
        do i=1,tse_nutot_at
        read(20,'(a)')dummy
        write(50,'(a)')trim(dummy)
        enddo
        read(20,'(a)')dummy 
        write(50,'(a)')trim(dummy) ! last line

        write(*,*)''
        write(*,*)&
          'position.fdf created (Coordinates is not corrected)'
        write(*,'(1x,2a)')&
          'Correct position.fdf file will be obtained by xyz2fdff90',&
          ' for tss_*.fdf after two fully relaxed calculations'
        write(*,*)'------------------------------------------'
        write(*,*)''
        write(*,*)&
        'Please run "crtfil4trasief90" for making tss_*.fdf input file'
      write(*,'(1x,2a)')&
        'If you want to create tss_*.fdf file, please add',&
        ' tse_*.fdf file as the first argument.'
        write(*,*)

        deallocate(s_nutot_el,tse_nutot_el)
        deallocate(nutot_el)
        deallocate(s_cell,tse_cell)
        deallocate(cell)

      goto 9999

999   continue
      write(*,*)''
      write(*,*)'Error: Cannot find s_position.fdf file '
      write(*,*)''
      goto 9999

998   continue
      write(*,*)''
      write(*,*)'Error: Cannot find tse_position.fdf file '
      write(*,*)''
      goto 9999

997   continue
      write(*,*)''
      write(*,*)'Error: Cannot find s_getdata.lammps file '
      write(*,*)''
      goto 9999

996   continue
      write(*,*)''
      write(*,*)'Error: Cannot find tse_getdata.lammps file '
      write(*,*)''
      goto 9999

995   continue
      write(*,*)''
      write(*,*)'Error: Cannot creat/open position.fdf file '
      write(*,*)''
      goto 9999

994   continue
      write(*,*)''
      write(*,*)'Error: Cannot creat/open getdata.lammps file '
      write(*,*)''
      goto 9999

9999  stop

      endprogram mer4fdf
