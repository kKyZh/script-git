      module m_changeatnum

      private
      public changeatnum

      contains

      subroutine changeatnum(exec)

      use jsu_readline

      implicit none
      integer, intent(in) :: exec
      integer i, j, k, l, lo, hi
      integer num, maxid, len_dump, len_dump2
      integer num_ele_lo, num_ele_hi
      integer check, renum, resort
      integer status, rename
      integer, allocatable :: id(:), num_ele(:), nw_id(:)
      real, allocatable :: pos(:,:)
      real tmp_real
      character(1) chs
      character(128) string, dummy
      character(32), allocatable :: name_ele(:)
      character(16), allocatable :: chr_id(:)
      character(:), allocatable :: tmp_chr
      character(16), allocatable :: chr_dump(:)
      ! 12 + 4 total chr_dump

      !######################### select : change the number of atoms
      write(*,*)
      write(*,*) '--------------- Information ---------------'
      write(*,*) 'This part is for corresponding the atom number ', &
        'between the s_position.fdf and tse_position.fdf'
      write(*,*) '-------------------------------------------'
100   continue
      write(*,*)
      write(*,*) 'Do you want to change the number of atoms?'
      !call userreadline( string, '(y, n) : ')
      if( exec .eq. 1) then
        string = 'y'
      elseif( exec .eq. 0) then
        string = 'n'
      endif
      read( string, *, iostat=check) chs
      if( check .ne. 0) then
        goto 100
      else
        select case(chs)
        case('y', 'Y')
          renum = 1
          goto 101
        case('n', 'N')
          renum = 0
          write(*,*)
          write(*,*) "Don't change... Next step"
          goto 101
        case default
          goto 100
      endselect
      endif
      !############### select : change the number of atoms finished

101   continue

      !######################### select : sort Z axis
      write(*,*)
      write(*,*) '--------------- Information ---------------'
      write(*,*) 'This part is for corresponding the atom number ', &
        'between the s_position.fdf and tse_position.fdf'
      write(*,*) '-------------------------------------------'
300   continue
      write(*,*)
      write(*,*) 'Do you want to sort Z axis from low to high?'
      !call userreadline( string, '(y, n) : ')
      if( exec .eq. 1) then
        string = 'n'
      elseif( exec .eq. 0) then
        string = 'y'
      endif
      read( string, *, iostat=check) chs
      if( check .ne. 0) then
        goto 300
      else
        select case(chs)
        case('y', 'Y')
          resort = 1
          if( renum .eq. 1) then
            write(*,*)
            write(*,*) 'Not support sort and renumber TOGETHER now.'
            goto 300
          endif
          goto 301
        case('n', 'N')
          resort = 0
          write(*,*)
          write(*,*) "Don't change... Next step"
          goto 301
        case default
          goto 300
      endselect
      endif
      !######################### select : sort Z axis finished

301   continue

      open(40, file='getdata.lammps', status='old', &
        err=98, form='formatted', access='sequential')
      open(50, file='position.fdf', status='unknown', &
        err=97, form='formatted', access='sequential')

      !############################## basic information
      read(40, *) dummy
      read(40, *) num
      read(40, *) maxid
      backspace(40)

      allocate(num_ele(maxid))
      allocate(id(maxid))
      allocate(nw_id(maxid))
      allocate(chr_id(maxid))
      allocate(pos(3, num))
      allocate(chr_dump(num))
      allocate(name_ele(maxid))

      read(40, *) maxid, dummy, dummy, &
        (num_ele(j), j=1, maxid)
      !############################## basic information finished
      len_dump = 1
      !############################## position read
      read(50, '(a)') dummy ! head block
      num_ele_lo = 1
      num_ele_hi = 0
      do k = 1, maxid
      num_ele_hi = num_ele_hi + num_ele(k)
      do i = num_ele_lo, num_ele_hi
      read(50, *) (pos(j, i), j=1, 3), id(k), name_ele(k)
      if( len_trim(adjustl(name_ele(k))) .gt. len_dump) then
      len_dump = len_trim(adjustl(name_ele(k)))
      len_dump2 = len_dump + 4
      endif
      backspace(50)
      read(50, '(3(1x, f11.6), a<len_dump2>)') &
        (pos(j, i), j=1, 3), chr_dump(i)
      enddo
      write(chr_id(k), '(i0)') id(k)
      num_ele_lo = num_ele_lo + num_ele(k)
      enddo
      rewind(40)
      rewind(50)
      allocate(character(len=len_dump2)::tmp_chr)
      !############################## position read finished

      if( renum .eq. 1) then
      !############################## renumber
      do i =1, maxid
103   continue
      write(*,*)
      write(*,'(2a)') &
        'Please enter the number of ', trim(adjustl(name_ele(i)))
      write(*,'(2a)') &
        'Less than two characters (first two will be counted)'
      call userreadline( string, &
        '(Intiger '//trim(adjustl(chr_id(i)))//' -> ) : ')
      read( string, '(i2)', iostat=check) nw_id(i)
      if( check .ne. 0) then
        goto 103
      else
        id(i) = nw_id(i)
      endif
      enddo
      !############################## renumber finished
      endif

      if( resort .eq. 1) then
      !############################## resort
      lo = 1
      hi = num - 1
      do j = lo, hi
      k = 0
      do i = lo, hi
      if( pos(3, i) .gt. pos(3, i+1)) then
        do l = 1, 3
        tmp_real = pos(l, i)
        pos(l, i) = pos(l, i+1)
        pos(l, i+1) = tmp_real
        enddo
        tmp_chr = chr_dump(i)
        chr_dump(i) = chr_dump(i+1)
        chr_dump(i+1) = tmp_chr
        k = k + 1
      endif
      enddo
      if( k .eq. 0) exit
      hi = hi -1
      enddo
      !############################## resort finished
      endif

      if( renum .eq. 1 .or. resort .eq. 1) then
      !############################## rewrite position.fdf
      write(50, '(a)') '%block AtomicCoordinatesAndAtomicSpecies'
      num_ele_lo = 1
      num_ele_hi = 0
      do k = 1, maxid
      num_ele_hi = num_ele_hi + num_ele(k)
      do i = num_ele_lo, num_ele_hi
      if( renum .eq. 1) then
        write(50, '(3(1x,f11.6), 1x, i2, 1x, a)') &
          (pos(j, i), j=1, 3), id(k), trim(adjustl(name_ele(k)))
      elseif( resort .eq. 1) then
        write(50, '(3(1x,f11.6), a<len_dump2>)') &
          (pos(j, i), j=1, 3), chr_dump(i)
      endif
      enddo
      num_ele_lo = num_ele_lo + num_ele(k)
      enddo
      write(50, '(a)') '%endblock AtomicCoordinatesAndAtomicSpecies'
      !############################## rewrite position.fdf finished
      endif

      write(*,*)
      if( renum .eq. 1) then
        write(*,*) 'position.fdf renewed number of species...'
      endif
      if( resort .eq. 1) then
        write(*,*) 'position.fdf resorted by Z axis...'
      endif
      if( renum .eq. 0 .and. resort .eq. 0) then
        write(*,*) 'no new file created...'
      endif
      write(*,*)

      rewind(50)

      close(40)
      close(50)

      deallocate(num_ele)
      deallocate(id)
      deallocate(nw_id)
      deallocate(chr_id)
      deallocate(chr_dump)
      deallocate(pos)
      deallocate(name_ele)

      !############################## rename position.fdf
      write(*,*) '########## rename the position.fdf file ##########'
200   continue
      write(*,*)
      write(*,*) 'Do you want to rename the position.fdf file?'
      call userreadline( string, '(y, n) : ')
      read( string, *, iostat=check) chs
      if( check .ne. 0) then
        goto 200
      else
        selectcase (chs)
        case('y', 'Y')
201   continue
          write(*,*)
          write(*,*) 'Please choose the name of new position.fdf'
          write(*,*) '(1) s_position.fdf -> for SIESTA (DEVICE)'
          write(*,*) &
            '(2) tse_position.fdf -> for TranSIESTA (Electrode)'
          write(*,*) &
            '(3) tss_position.fdf -> for TranSIESTA (Scattering)'
          call userreadline( string, '([1], [2], [3]) : ')
          read( string, *, iostat= check) chs
          if( check .ne. 0) then
            goto 201
          else
            selectcase (chs)
            case ('1')
              write(*,*)
              write(*,*) '[1] to s_position.fdf, s_getdata.lammps'
              status= rename('position.fdf', 's_position.fdf')
              status= rename('getdata.lammps', 's_getdata.lammps')
              goto 202
            case ('2')
              write(*,*)
              write(*,*) '[2] to tse_position.fdf, tse_getdata.lammps'
              status= rename('position.fdf', 'tse_position.fdf')
              status= rename('getdata.lammps', 'tse_getdata.lammps')
              goto 202
            case ('3')
              write(*,*)
              write(*,*) '[3] to tss_position.fdf, tss_getdata.lammps'
              status= rename('position.fdf', 'tss_position.fdf')
              status= rename('getdata.lammps', 'tss_getdata.lammps')
              goto 202
            case default
              goto 201
            endselect
          endif
        case ('n', 'N')
          write(*,*)
          write(*,*) "Don't rename... finised"
          write(*,*)
          goto 202
        case default
          goto 200
        endselect
      endif
      !############################## rename position.fdf finished

202   continue

      goto 99

97    continue
      write(*,*)
      write(*,*) 'ERROR: Can no open position.fdf file.'
      write(*,*)
      goto 99

98    continue
      write(*,*)
      write(*,*) 'ERROR: Can no open getdata.lammps file.'
      write(*,*)
      goto 99

99    stop

      endsubroutine changeatnum
      endmodule m_changeatnum
