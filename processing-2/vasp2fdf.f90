      program vasp2fdf

      use m_strain_get
      use jsu_readline

      implicit none

      integer i, j, k
      integer strain_chs, strain_lo, strain_hi
      integer num_fil, len_fra_strain_nam
      integer len_fra_percent, len_strain_nam 
      real strain_in
      real strain_nam, strain_i
      character(32) strain_chr_nam
      ! ==============================
      integer status, system
      logical chk_filex
      integer fil_no_ex
      ! ==============================
      character(128) string, dummy
      integer num, maxid
      integer, allocatable :: num_ele(:)
      real, allocatable :: pos(:,:)
      ! ==============================
      integer chk
      integer chs_go
      ! ==============================

      write(*,*)
      write(*,*) 'vasp2fdf -> CONTCAR to position.fdf'
      write(*,*) 'please copy getdata.lammps here'


      open(10, file='getdata.lammps', status='old', &
        err=95, form='formatted', access='sequential') 

      call strain_get( strain_chs, num_fil, &
        strain_lo, strain_hi, strain_in, &
        len_strain_nam, len_fra_strain_nam, len_fra_percent)

        ! find length of fraction part, if len < 0 -> = 0
        ! file include itself
        
        ! strain_lo to strain_hi, interval strain_in, 
        ! i for number files

        write(*,*) num_fil
        i = 0
        if( strain_chs .eq. 1) then
        do strain_i = strain_lo, strain_hi, strain_in
        i = i + 1
        strain_nam = (100 + strain_i) / 100.0
        write(strain_chr_nam, &
          '(f<len_strain_nam>.<len_fra_strain_nam>)') strain_nam
        !##### check output
        write(*,'(2a)') trim(adjustl(strain_chr_nam)), '#'
        if( len_fra_percent .eq. 0) then
        write(*,'(1x, i0, a)') &
          int(strain_i), '%'
        else
        write(*,'(1x, f<len_strain_nam>.<len_fra_percent>,a)') &
          strain_i, '%'
        endif

        inquire( file=&
          './strain_'&
          //trim(adjustl(strain_chr_nam))//'/CONTCAR', &
          exist=chk_filex)

        fil_no_ex = 0
        if( chk_filex .eq. .false.) then
          write(*,*)
          write(*,*) &
          './strain_'&
          //trim(adjustl(strain_chr_nam))//'/CONTCAR -> not exist'
        fil_no_ex = fil_no_ex + 1
        endif
        enddo

        elseif( strain_chs .eq. 2) then
        strain_i = 0
        len_strain_nam = 4
        len_fra_strain_nam = 2
        write(*,'(1x, i0, a)') &
          int(strain_i), '% no strain'
        strain_nam = (100 + strain_i) / 100.0
        write(strain_chr_nam, &
          '(f<len_strain_nam>.<len_fra_strain_nam>)') strain_nam
        write(*,'(2a)') trim(adjustl(strain_chr_nam)), '#'

        inquire( file=&
          './strain_'&
          //trim(adjustl(strain_chr_nam))//'/CONTCAR', &
          exist=chk_filex)

        if( chk_filex .eq. .false.) then
          write(*,*)
          write(*,*) &
          './strain_'&
          //trim(adjustl(strain_chr_nam))//'/CONTCAR -> not exist'
        fil_no_ex = 1
        endif
        endif
        !##### check output

        if( fil_no_ex .gt. 0) then
          write(*,*)
          write(*,*) '#############################'
          write(*,*) 'WARNING : CONTCAR file missed'
          write(*,*) '#############################'
100   continue
          write(*,*)
          write(*,*) 'Do you want to continue?'
          call userreadline( string, '([1] yes, [2] no) : ')
          read( string, *, iostat=chk) chs_go
          if( chk .ne. 0) goto 100
          if( chs_go .eq. 1) then
          write(*,*)
          write(*,*) 'chose [1] yes'
          elseif( chs_go .eq. 2) then
          write(*,*)
          write(*,*) 'chose [2] no, -> quit'
          write(*,*)
            goto 99
          else
            goto 100
          endif
        endif
        
        ! ############################## pre-read
        read(10, *) dummy
        read(10, *) num, dummy
        read(10, *) maxid
        backspace(10)
        allocate(num_ele(maxid))
        allocate(pos(3,num))
        read(10, *) maxid, dummy, dummy, &
          (num_ele(j), j=1, maxid)
        rewind(10)
        close(10)
        ! ############################## pre-read

        strain_i = strain_lo
        do i = 1, num_fil
        strain_nam = (100 + strain_i) / 100.0
        write(strain_chr_nam, &
          '(f<len_strain_nam>.<len_fra_strain_nam>)') strain_nam
        open(80000+i, &
          file=&
          './strain_'//trim(adjustl(strain_chr_nam))//'/CONTCAR',&
          status='old', &
          err=94, form='formatted', access='sequential') 
        ! lattice parameter is not needed for this time
        ! Z axis fixed, other two are vaccum space
        ! if change too large, maybe result is wrong
        do j = 1, 9
        read(80000+i, *) dummy
        enddo
        ! first 9 lines are useless this time
        ! only coordinates are required
        do k = 1, num
        read(80000+i, *) (pos(j,k), j=1, 3)
        enddo
        open(50000+i, &
          file=&
          './strain_'//trim(adjustl(strain_chr_nam))//&
          '/position_'//trim(adjustl(strain_chr_nam))//'.fdf',&
          status='unknown', &
          err=96, form='formatted', access='sequential') 
        write(50000+i, '(a)') &
          '%block AtomicCoordinatesAndAtomicSpecies'
        do k = 1, num
        write(50000+i, '(3(1x, f11.6))') (pos(j,k), j=1, 3)
        enddo
        write(50000+i, '(a)') &
          '%endblock AtomicCoordinatesAndAtomicSpecies'

        rewind(80000+i)
        rewind(50000+i)
        close(80000+i)
        close(50000+i)
        enddo

        deallocate(num_ele)
        deallocate(pos)
        goto 99

94    continue
        write(*,*)
        write(*,*) 'Error : Can not open CONTCAR file'
        write(*,*)
        goto 99

95    continue
        write(*,*)
        write(*,*) 'Error : Can not open getdata.lammps file'
        write(*,*)
        goto 99

96    continue
        write(*,*)
        write(*,*) 'Error : Can not open positionfdf file'
        write(*,*)
        goto 99

99    stop

      endprogram vasp2fdf
