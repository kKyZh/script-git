      program test

      use jsu_readline
      implicit none

        integer num, num_data, renum
        integer i, j, k, l, m, step, at, get_num, nx, ny, nz, format_co
        integer numv, maxid, getdata_num,order, order_jump
        integer tempid
        integer check
        integer num_ele_low,num_ele_hi
        integer status, rename, system
        integer posre_nam_chk, posre_chs_chk ! renew a position.fdf name
        integer vasp_out ! output vasp POSCAR or not
        integer fix_type ! F F F type choose
        integer sele_fix ! fix one kind of atoms
        integer strain_chs ! strain or not
        integer num_fil ! position.fdf POSCAR files
        integer strain_lo, strain_hi ! strain lo and hi range
        real strain_in ! strain interval
        real strain_i ! strain change
        real real_check ! mod, amod, dmod are not working well
        integer len_strain_nam ! length of strain_nam real
        integer len_fra_strain_nam ! length of fraction of strain_name
        integer len_fra_percent ! length of fraction of percent
        integer len_int_strain_nam ! real part of strain_name
        integer, allocatable :: id(:)
        integer, pointer :: num_ele(:)
        integer, allocatable :: nw_id(:)! new add change number
        real cell_lo_re, cell_hi_re
        real real_zero
        real hi_z, lo_z ! for POSCAR fix Z axis, I-V and strain
        real,allocatable :: cellx(:), celly(:), cellz(:)
        real,allocatable :: posx2(:,:), posy2(:,:), posz2(:,:)
        real,allocatable :: posx1(:), posy1(:), posz1(:)
        real, allocatable :: cell_lo(:,:), cell_hi(:,:)
        real, allocatable :: val(:,:,:)
        character(32) posre_nam, posre_chs ! renew a position.fdf name
        character(128) :: filename, dummy
        character(64) :: tempchar
        !------------------------------ for position.fdf
        character(len=:), allocatable :: name_ele(:)
        !------------------------------ for POSCAR
        character(2), allocatable :: vasp_ele(:)
        !---------------------- for sort transiesta part
        real, allocatable :: pos_x(:), pos_y(:), pos_z(:)
        integer len_dump, len_dump2
        character(32) len_temp
        character(32) read_dump
        character(1) chs
        character(len=:), allocatable :: dump(:)
        !---------- swap parameter
        integer lo, hi
        real temp_real
        character(len=:), allocatable :: temp_dump
        !-----------------------------------------------
        !----------------------------------
        integer lchk ! length of check
        integer nna, narg, lna, larg ! no correct name & number of
        ! argument & length of name & length of argument
        character(32) na ! options variable: name
        character(64) :: chk_filename ! check name is *.car
        !----------------------------------
        character(128) string ! userreadline
        !----------------------------------
        character(1) fix_x, fix_y, fix_z ! fixed axis x y z -> F T
        !----------------------------------
        real strain_nam
        character(32) strain_chr_nam ! directory name strain percent
        character(32) strain_chr_in ! char for interval
        !----------------------------------
        integer chk_fix
        integer fix_species
        integer dime_ly ! carbon dime layer
        integer length_wide ! carbon 6-member-ring length wide part
        integer num_wide ! wide carbon atoms number
        integer num_narrow ! narrow carbon atoms number
        real dist_dime_ly ! distance between two carbon atoms
        real, allocatable :: x_pos(:,:,:)
        integer, allocatable :: x_id(:)
        integer, allocatable :: y_id(:)
        character(len=:), allocatable :: x_name_ele(:)
        real tmp_real
        integer tmp_int
        character(len=:), allocatable :: tmp_name_ele
        integer re_num_wide
        integer re_num_narrow
        real z_narrow_lo
        real z_narrow_hi
        integer z_l
        integer z_po_l



        renum = 0
49      continue

        write(*,*)
        write (*,'(1x,2a)')'Please input how many variables (integer)',&
          ' in one row?'
        write (*,'(1x,a,/,3a)') &
          '(8 variables in one row at dump.GNR file)',&
        ' (or "0" to run Lammps2fdf directly, "-1" to quit the job,',&
        ' "-2" to sort Z axis for Transiesta,',&
        ' "-3" to change species number)' 

        !call userreadline( string, '(Integer) : ' )
        string = '5'
        read (string,*,iostat=check)order
!----- need to be changed use ascii for recognize characters
        if(check.ne.0)then
          goto 49
        elseif(order.eq.0)then
          write(*,*)
          write(*,*) 'Please be sure you have getdata.lammps file.'
          renum = 0
          !call Lammps2fdf
          goto 99
                
        elseif(order.eq.-1)then
          write(*,*)
          write(*,*) 'You quit the job'
          write(*,*)
          goto 99
                
        elseif(order.eq.-2)then
          write(*,*)
          write(*,*) 'Please be sure you have getdata.lammps file.'
          renum = 0
          !call changeatnum(renum)
          goto 99

        elseif(order.eq.-3)then
          write(*,*)
          write(*,*) 'renumber has to refresh position.fdf file.'
          write(*,*) 'reselect x, y, z, enabled renumber'
          renum = 1
          goto 49
        
        elseif (order.lt.3.and.order.gt.0) then
          write(*,*)
          write(*,*) 'no data selected, try again'
          goto 49

        elseif (order.gt.8)then
          write(*,*)
          write(*,*) 'over the data selected range, try again'
          goto 49
        else
          numv = order-2
        endif

        open(10,file='dump.GNR', status='old',&
                err=98, form='formatted', access='sequential')
        open(40,file='test.fdf', status='unknown', &
                err=96, form='formatted', access='sequential') 

        num_data = 0
102     continue
        read(10,'(a)',end=103) dummy
        num_data = num_data + 1
        read(10,'(i)') step
        read(10,'(a)') dummy
        read(10,'(i)') num
        read(10,'(a)') dummy

        do i = 1, 4
        read(10,'(a)') dummy
        enddo

        do i = 1, num
        read(10, *) at, tempid
          if (i .eq. 1) then 
            maxid=tempid
          else if (tempid .gt. maxid) then
            maxid=tempid
          endif
        enddo

        goto 102
        ! pre-read finished
103     continue
        rewind(10)

        
81      continue
        write(*,*)
        write(*,*)'Which number of data do you want to get'
        write(*,'(1x,a,1x,i0)')'Number of data =',num_data
        write(*,'(a2,i0,a)') ' (',num_data,' is the final data)'
        !call userreadline( string, '(Integer) : ' )
        string = '2'
        read(string,*,iostat=check) getdata_num
        if(check.gt.0) then
          goto 81
        elseif( num_data.lt.getdata_num.or.getdata_num.lt.1) then
          goto 81
        endif

        allocate(id(num))
        allocate(num_ele(maxid))
        allocate(cell_lo(3,getdata_num),cell_hi(3,getdata_num))
        allocate(val(numv,num,getdata_num))
        allocate(cellx(getdata_num),celly(getdata_num),&
          cellz(getdata_num))
        allocate(posx2(num,getdata_num),posy2(num,getdata_num),&
          posz2(num,getdata_num))
        allocate(character(32)::name_ele(maxid)) 
        allocate(vasp_ele(maxid)) 






        ! choose getdata_num and read
        do k = 1, getdata_num
        do i=1,maxid
          num_ele(i)=0
        enddo
        
        do i = 1, 5
        read(10,'(a)') dummy
        enddo

        do i=1, 3
          read(10,*)cell_lo(i,getdata_num),cell_hi(i,getdata_num)
        enddo

        read(10, '(a)') dummy

        do i=1, num
        read(10,*)at,id(i),(val(j,i,getdata_num),j=1,numv)
        num_ele(id(i))=num_ele(id(i))+1
        ! num_ele : number of each atom species, 
        ! initial before each step
        enddo
        enddo
        ! read data finished
        
        ! sort by species
        do i=1,num
        do j=1,num

          if(j>=2.and.j<=num&
          .and.id(j-1)>id(j))then
          tempid=id(j)
          id(j)=id(j-1)
          id(j-1)=tempid

          do l = 1, numv
          temp_real = val(l, j, getdata_num)
          val(l, j, getdata_num) = val(l, j-1, getdata_num)
          val(l, j-1, getdata_num) = temp_real
          enddo
          endif

        enddo
        enddo
        ! sort by species finished

        rewind(10)


70      continue
        write(*,*)
        write(*,*)'Format of coordination of atom position'
        write(*,'(1x, 2a)') '(1) direct coordinates (fractional); ', &
                        '(2) cartesian coordinates (cartesian);'

        !call userreadline( string, '(Integer) : ' )
        string = '2'
        read( string,*,iostat=check) format_co
        if(check>0)then
        goto 70
        elseif (format_co.ne.1 .and. format_co.ne.2) then
        goto 70
        !practice for do while
        !do while (format_co/=1.and.format_co/=2)
        !goto 70
        !enddo
        endif

        ! decrease lines here, make x, y, z in one loop
71      continue
        write(*,*)
        write(*,*) &
          'Select which column in dump.GNR for X axis in getdata.lammps'
        !call userreadline( string, '(Integer) : ' )
        string = '5'
        read( string,*,iostat=check) order
        if(check>0)then
          goto 71
        elseif (order.lt.3 .or. order.gt.8) then
          goto 71
        endif
        nx = order - 2

72      continue
        write(*,*)
        write(*,*) &
          'Select which column in dump.GNR for Y axis in getdata.lammps'
        !call userreadline( string, '(Integer) : ' )
        string = '3'
        read( string,*,iostat=check) order
        if(check>0)then
          goto 72
        elseif (order.lt.3 .or. order.gt.8 .or. order.eq.nx+2) then
          goto 72
        endif
        ny = order - 2

73      continue
        write(*,*)
        write(*,*) &
          'Select which column in dump.GNR for Z axis in getdata.lammps'
        !call userreadline( string, '(Integer) : ' )
        string = '4'
        read( string,*,iostat=check) order
        if(check>0)then
          goto 73
        elseif (order.lt.3 .or. order.gt.8 .or. &
            order.eq.nx+2 .or. order.eq.ny+2) then
          goto 73
        endif
        nz = order - 2

        do i = 1, 3
        if( i .eq. nx)then
        cellx(getdata_num) = abs(cell_hi(nx, getdata_num) - &
          cell_lo(nx, getdata_num))
        elseif( i .eq. ny)then
        celly(getdata_num) = abs(cell_hi(ny, getdata_num) - &
          cell_lo(ny, getdata_num))
        elseif( i .eq. nz)then
        cellz(getdata_num) = abs(cell_hi(nz, getdata_num) - &
          cell_lo(nz, getdata_num))
        endif
        enddo

        do i = 1, num
        if (format_co.eq.2) then
          posx2(i,getdata_num)=&
            val(nx,i,getdata_num)*cellx(getdata_num)+&
            cell_lo(nx,getdata_num)
          posy2(i,getdata_num)=&
            val(ny,i,getdata_num)*celly(getdata_num)+&
            cell_lo(ny,getdata_num)
          posz2(i,getdata_num)=&
            val(nz,i,getdata_num)*cellz(getdata_num)+&
            cell_lo(nz,getdata_num)
          write(*,'(1x,i7,1x,i2,3(1x,f11.6))')&
          i, id(i), posx2(i,getdata_num),&
          posy2(i,getdata_num),posz2(i,getdata_num)
        else if (format_co.eq.1) then
          posx2(i,getdata_num)= val(nx,i,getdata_num)
          posy2(i,getdata_num)= val(ny,i,getdata_num)
          posz2(i,getdata_num)= val(nz,i,getdata_num)
          write(*,'(1x,i7,1x,i2,3(1x,f11.6))')&
          i, id(i), posx2(i,getdata_num),&
          posy2(i,getdata_num),posz2(i,getdata_num)
        endif
        enddo
        write(*,*)
        write(*,*)'********** For position.fdf **********'
        do i=1,maxid
        write(*,*)
        write(*,'(a,i0,a,i0)')'Please enter the new name of species '&
                ,i,', Amount = ',num_ele(i)
        call userreadline( string, '(name of species) : ' )
        read( string,*)name_ele(i)
        !-------------- for transiesta get unknown string length
        if(i.eq.1)then
        len_dump=sizeof(trim(name_ele(i)))
        elseif(len_trim(name_ele(i)).gt.len_dump)then
          len_dump=len_trim(name_ele(i))
        endif
        enddo
                

        allocate(x_pos(3, num, maxid))
        allocate(x_id(num))
        allocate(y_id(num))
        allocate(character(len=len_dump) :: x_name_ele(num))
        allocate(character(len=len_dump) :: tmp_name_ele)

          do i = 1, num
          y_id(i) = id(i)
          x_id(i) = id(i)
          x_pos(1, i, x_id(i)) = posx2(i, getdata_num)
          x_pos(2, i, x_id(i)) = posy2(i, getdata_num)
          x_pos(3, i, x_id(i)) = posz2(i, getdata_num)
          enddo
        num_ele_low = 1
        num_ele_hi = 0
        do j=1, maxid
        num_ele_hi = num_ele_hi + num_ele(j)
        do i = num_ele_low, num_ele_hi

        x_name_ele(i) = name_ele(j)

        enddo
        num_ele_low = num_ele_low + num_ele(j)
        enddo

        ! bubble sort
        lo = 1
        hi = num - 1
        do k = lo, hi
        l = 0
        do i = lo, hi
        if( x_pos(3, i, x_id(i)) .gt. &
          x_pos(3, i+1, x_id(i+1))) then
          tmp_int = y_id(i)
          y_id(i) = y_id(i+1)
          y_id(i+1) = tmp_int
          do j = 1, 3
          tmp_real = x_pos(j, i, x_id(i))
          x_pos(j, i, x_id(i)) = x_pos(j, (i+1), x_id(i+1))
          x_pos(j, (i+1), x_id(i+1)) = tmp_real
          enddo
          tmp_name_ele = x_name_ele(i)
          x_name_ele(i) = x_name_ele(i+1)
          x_name_ele(i+1) = tmp_name_ele
          l = l + 1
        endif
        enddo
        if( l .eq. 0) goto 999
        hi = hi - 1
        enddo

999     continue

        !do i = 1, num
        !  write(*,'(3(1x,f11.6), 1x, i2, 1x, a<len_dump>)')&
        !  (x_pos(j, i, x_id(i)), j = 1, 3), &
        !  y_id(i), x_name_ele(i)
        !enddo


        strain_nam = 1.00
        num_ele_low = 1
        num_ele_hi = 0
        do j=1, maxid
        num_ele_hi = num_ele_hi + num_ele(j)
        do i = num_ele_low, num_ele_hi
          ! position.fdf
          write(40,'(3(1x,f11.6), 1x, i2, 1x, a<len_dump>)')&
          posx2(i,getdata_num)*strain_nam, &
          posy2(i,getdata_num)*strain_nam, &
          posz2(i,getdata_num)*strain_nam, id(i), name_ele(j)
        enddo
        num_ele_low = num_ele_low + num_ele(j)
        enddo

        do i = 1, 10
        write(40,'(a)')
        enddo


        do i = 1, num
          write(40,'(3(1x,f11.6), 1x, i2, 1x, a<len_dump>)')&
          (x_pos(j, i, x_id(i)), j = 1, 3), &
          y_id(i), x_name_ele(i)
        enddo

        j = 1
        dime_ly = 1
        num_wide = 1
        re_num_narrow = 1
        re_num_wide = 1
        l = 0
        do i = 2, num
        if(trim(adjustl(x_name_ele(i))) .eq. 'H') then
          !write(*,*) 'it is H, go next'
          j = j
          l = l + 1
          if( l .gt. 2) then
          if( x_pos(3, i, x_id(i)) .gt. &
            x_pos(3, i-1, x_id(i-1))) then
            !write(*,*) 'not the zigzag H line'
            l = 1
          else
            z_po_l = x_pos(3, i, x_id(i))
            z_l = l
          endif
          endif
        elseif(trim(adjustl(x_name_ele(i))) .ne. 'H') then
        dist_dime_ly = x_pos(3, i, x_id(i)) - x_pos(3, j, x_id(j))
        !write(*,*) dist_dime_ly
          if( dime_ly .eq. 2) then
            re_num_narrow = num_wide
            re_num_wide = num_wide
          endif
          num_wide = num_wide + 1
          if(dist_dime_ly .ge. 1.40) then
            if( re_num_narrow + 1 .gt. num_wide) then
              write(*,*) l, num_wide
              re_num_narrow = num_wide - 1
              re_num_wide = num_wide - 1
              z_narrow_lo = x_pos(3, (j-num_wide-l+2), &
                x_id(j-num_wide-l+2))
            endif
            if( dime_ly .gt. 2) then
            if( re_num_wide + 1 .lt. num_wide) then
              write(*,*) l, num_wide, z_l
              re_num_wide = num_wide - 1
              z_narrow_hi = x_pos(3, (j-num_wide-z_l-l+1), &
                x_id(j-num_wide-z_l-l+1))
            endif
            endif

            num_wide = 0
            dime_ly = dime_ly + 1
            num_wide = num_wide + 1
            l = 0
            !write(*,*) dime_ly
          endif
          !write(*,*) num_wide
          j = i
          !write(*,*) x_pos(3, i, x_id(i)), x_pos(3, j, x_id(j))
        endif
        enddo
        !z_narrow_hi = z_narrow_hi - 1.420272
        !z_narrow_lo = z_narrow_lo - 1.420272

        write(*,*) re_num_narrow, z_narrow_lo, re_num_wide, z_narrow_hi
        






        deallocate(id)
        deallocate(num_ele)
        deallocate(val) 
        deallocate(cell_lo,cell_hi) 
        deallocate(cellx,celly,cellz)
        deallocate(posx2,posy2,posz2)
        deallocate(name_ele)
        deallocate(vasp_ele)

        deallocate(tmp_name_ele)
        deallocate(x_pos)
        deallocate(x_id)
        deallocate(x_name_ele)





        !rewind(40)
        !close(40)
        close(10)

        goto 99

98      continue
        write(*,*)
        write(*,*)'Error: Can not open a dump.GNR file.'
        write(*,*)
        goto 99

96      continue
        write(*,*)
        write(*,*)'Error: Can not open a getdata.lammps file.'
        write(*,*)
        goto 99

99    stop
      endprogram test
