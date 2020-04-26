      module m_Lammps2fdf

      private
      public Lammps2fdf

      contains
        
      subroutine Lammps2fdf

      use jsu_readline

      implicit none
      integer i, j, l
      integer num, maxid, at, len_dump
      integer num_ele_lo, num_ele_hi
      integer, allocatable :: id(:), num_ele(:)
      real, allocatable :: pos(:,:)
      character(128) dummy, string
      character(32), allocatable :: name_ele(:)

      write(*,*)
      write(*,*) 'subroutine for lmpdum2posf90 //Apr/18/2020//'
      write(*,*) 'data from getdump.lammps to position.fdf'
      write(*,*)

      open(40, file='getdata.lammps', status='old', &
        err=98, form='formatted', access='sequential')
      open(50, file='position.fdf', status='unknown', &
        err=97, form='formatted', access='sequential')

      read(40, *) dummy
      read(40, *) num
      ! #################### pre-read maxid and go back one line
      read(40, *) maxid
      backspace(40)

      allocate(num_ele(maxid))
      allocate(id(num))
      allocate(pos(3, num))
      allocate(name_ele(maxid))

      read(40, *) maxid, dummy, dummy, &
        (num_ele(j), j=1, maxid)
      
      do i = 1, 6
      read(40, '(a)') dummy
      enddo

      ! #################### name of species enter
      write(*,*)
      write(*,*) '*************** For position.fdf ***************'
      do i =1, maxid
      write(*,*)
      write(*, '(a, i0, a, i0)') &
        'Please enter the new name of species ', i, &
        ', Amount = ', num_ele(i)
      call userreadline( string, '(name of species) : ')
      read( string, *) name_ele(i)
      if( i .eq. 1) then
        len_dump = sizeof(trim(adjustl(name_ele(i))))
      elseif( len_trim(adjustl(name_ele(i))) .gt. len_dump) then
        len_dump = len_trim(adjustl(name_ele(i)))
      endif
      enddo
      ! #################### name of species enter finished

      ! #################### read position and write
      write(50, '(a)') '%block AtomicCoordinatesAndAtomicSpecies'
      num_ele_lo = 1
      num_ele_hi = 0
      do l = 1, maxid
      num_ele_hi = num_ele_hi + num_ele(l)
      do i = num_ele_lo, num_ele_hi
      read(40, *) at, id(i), (pos(j, i), j=1, 3)
      write(50, '(3(1x, f11.6), 1x, i2, 1x, a<len_dump>)') &
        (pos(j, i), j=1, 3), id(i), name_ele(l)
      enddo
      num_ele_lo = num_ele_lo + num_ele(l)
      enddo
      write(50, '(a)') '%endblock AtomicCoordinatesAndAtomicSpecies'
      ! #################### read position and write finished

      write(*,*)
      write(*,*) 'position.fdf created...'
      write(*,*)

      rewind(40)
      close(40)
      close(50)
      deallocate(num_ele)
      deallocate(id)
      deallocate(pos)
      deallocate(name_ele)

      goto 99

97    continue
      write(*,*)
      write(*,*) 'ERROR: Can not open position.fdf file.'
      write(*,*)
      goto 99

98    continue
      write(*,*)
      write(*,*) 'ERROR: Can not open getdata.lammps file.'
      write(*,*)
      goto 99

99    stop
      endsubroutine Lammps2fdf
      endmodule m_Lammps2fdf

