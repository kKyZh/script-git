        program lmpdum2pos
        
        use jsu_readline
        use m_manual
        use m_Lammps2fdf
        use m_changeatnum

        implicit none
        
        integer num, num_data, renum
        integer i, j, k, m, step, at, get_num, nx, ny, nz, format_co
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
        integer l, lo, hi
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
        !----------------------------------
        real, allocatable :: z_lattice(:)
        real zhi_check
        !----------------------------------

        write(*,*)
        write(*,'(1x,2a)') &
          'Running... getdump file from ## dump.GNR ##... ',&
                '(Version --3.1 beta//Apr/17/2020//)'
        write(*,*)
        write(*,*)'Include manual option: -h '
        write(*,*)'By using ==> car2Lammps -h '
        write(*,*)'To see how to use it...'

        ! ###### help options ######

        narg=command_argument_count()
        nna=0
        if(narg==0)then
          write(*,*)
          write(*,*)'call manual: -h'
          write(*,*)
          goto 98
        else
        do i=1,narg
        call get_command_argument(i,na)
          larg=len_trim(na)
          if(na(1:1)=='-')then
            do lna=2,larg
            if(na(lna:lna)=='h')then
              write(*,*)
              call manual
              write(*,*)
              goto 98
            endif
            enddo
            nna=nna+1
          endif
        enddo
        endif

        if(narg==nna)then
          write(*,*)
          write(*,*)'------'
          write(*,*)'No available options'
          write(*,*)'call manual: -h'
          write(*,*)
          goto 98
        endif

        ! ###### help options finished ######
        ! ###### check variable has .car ######

        do i=1,narg
        call get_command_argument(i,chk_filename)
        !lchk=len_trim(chk_filename)
        ! keep for future upgrade
        if(chk_filename=='dump.GNR')then
          filename=chk_filename
          goto 49
        else
          goto 98
        endif
        enddo

        ! ###### check variable has .car finished ###### ???

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
        write(*,'(a)') 'Usually "5" -> Atoms ID, type, X,Y,Z ;'
        call userreadline( string, '(Integer) : ' )
        read (string,*,iostat=check)order
!----- need to be changed use ascii for recognize characters
        if(check.ne.0)then
          goto 49
        elseif(order.eq.0)then
          write(*,*)
          write(*,*) 'Please be sure you have getdata.lammps file.'
          renum = 0
          call Lammps2fdf
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
          call changeatnum(renum)
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

97      continue

        ! ############################## output POSCAR or not
        write(*,*)
        write(*,*) 'Do you need VASP POSCAR file?'
        call userreadline( string, '([1] yes, [2] no) : ')
        read( string, *, iostat=check) vasp_out
        if(check .ne. 0) then
          goto 97
        elseif(vasp_out .ne. 1 .and. vasp_out .ne. 2) then
          goto 97
        endif
        ! ##################### output POSCAR or not select finished

        open(10,file=filename, status='old',&
                err=98, form='formatted', access='sequential')
        open(40,file='getdata.lammps', status='unknown', &
                err=96, form='formatted', access='sequential') 
        ! #################### dump.GNR & getdata.lammps here
        ! #################### position.fdf and POSCAR are later

        ! ############################## strain or not
        ! strain and fix one specie -> specific fix wide part
        ! only narrow part strained
        ! strain with each line
91      continue
        write(*,*)
        write(*,'(5a)') 'Do you need strain?', &
          ' (position.fdf -> ./strain_*/position_*.fdf)',&
          ' (* strain without %)', &
          ' (POSCAR -> mkdir strain_* -> ./strain_*/POSCAR)', &
          ' (Uniaxial strain along Z axis for I-V calculation)'
        call userreadline( string, '(1. yes, 2. no) : ')
        read( string, *, iostat=check) strain_chs
        if( check .ne. 0) then
          goto 91
        elseif( strain_chs .eq. 1) then
          write(*,*)
          write(*,*) '1. yes, with strain'
199     continue
          write(*,*)
          write(*,*) 'Lowest strain (with %)'
          call userreadline( string, '(Integer) : ')
          read( string, *, iostat=check) strain_lo
          if( check .ne. 0) goto 199
          if( strain_lo .lt. 0) goto 199
          !only positive value availiable this time
198     continue
          write(*,*)
          write(*,*) 'Highest strain (with %)'
          call userreadline( string, '(Integer) : ')
          read( string, *, iostat=check) strain_hi
          if( check .ne. 0) goto 198
          if( strain_hi .lt. strain_lo) goto 198
          if( (strain_hi / 100) .ge. 10) then
            ! 100 + strain_hi
            len_int_strain_nam = len_trim(adjustl(string)) - 2 + 1
          elseif((strain_hi/100).ge.9 .and. (strain_hi/100).lt.10) then
            len_int_strain_nam = 3
            ! specific 900 + 100 = 1000 include .
          else
            len_int_strain_nam = 2
          endif
          if( strain_hi .eq. strain_lo) then
            strain_in = 1
            string = '1'
            goto  196
          endif
197     continue
          write(*,*)
          write(*,*) 'Interval (with %)'
          call userreadline( string, '(Decimal or Integer) : ')
          read( string, *, iostat=check) strain_in
          if( check .ne. 0) goto 197
          if( strain_in .gt. (strain_hi - strain_lo)) goto 197
          ! find remainder manually, amod, dmod, mod not work well
           real_check =( int(strain_hi - strain_lo) - &
           (( int((strain_hi - strain_lo) / strain_in)) * strain_in))
          ! find remainder manually, amod, dmod, mod not work well
          !write(*,*) real_check
          if( real_check .ne. 0) goto 197
          ! find length of fraction part, if len < 0 -> = 0
196     continue
          ! strain_lo = strain_hi
          len_strain_nam = len_trim(adjustl(string))
          write( strain_chr_in, '(f0.0)') strain_in
          len_fra_strain_nam = len_strain_nam - &
          len_trim(adjustl(strain_chr_in))
          if( len_fra_strain_nam .lt. 0) then
            len_fra_strain_nam = 0
          endif
          len_fra_percent = len_fra_strain_nam
          len_fra_strain_nam = len_fra_strain_nam + 2
          len_strain_nam = len_int_strain_nam + len_fra_strain_nam
          if( strain_lo .eq. strain_hi) then
            num_fil = 1
          else
            num_fil = ((strain_hi - strain_lo) / strain_in) + 1
          endif
          write(*,*) num_fil
          ! find length of fraction part, if len < 0 -> = 0
          ! file include itself
          
          ! strain_lo to strain_hi, interval strain_in, 
          ! i for number files
          i = 0
          do strain_i = strain_lo, strain_hi, strain_in
          i = i + 1
          strain_nam = (100 + strain_i) / 100.0
          write(strain_chr_nam, &
            '(f<len_strain_nam>.<len_fra_strain_nam>)') strain_nam
          write(*,'(2a)') trim(adjustl(strain_chr_nam)), '#'
          if( len_fra_percent .eq. 0) then
          write(*,'(1x, i0, a)') &
            int(strain_i), '%'
          else
          write(*,'(1x, f<len_strain_nam>.<len_fra_percent>,a)') &
            strain_i, '%'
          endif
          status = system( &
            'mkdir strain_'&
            //trim(adjustl(strain_chr_nam))//&
              ' 2>/dev/null')

          ! just show new directory created or not
          if( status .ne. 0) then
            write(*,*) 'file '//trim(adjustl(strain_chr_nam))//&
              ' not created'
          endif

          !############################## open write files
          open(50000+i, &
            file=&
            './strain_'//trim(adjustl(strain_chr_nam))//&
            '/position_'//trim(adjustl(strain_chr_nam))//'.fdf',&
            status='unknown', &
            err=95, form='formatted', access='sequential') 

          if(vasp_out .eq. 1) then
            open(80000+i, &
              file=&
              './strain_'//trim(adjustl(strain_chr_nam))//'/POSCAR',&
              status='unknown', &
              err=94, form='formatted', access='sequential') 
          endif

          enddo
          !############################## open write files

          ! real can not add leading 0 but integer can
          ! do tricks from character for leading 0 real format
          ! real no blank leading spaces 

          ! ### no strain
        elseif( strain_chs .eq. 2) then
          write(*,*)
          write(*,*) '2. No, without strain'
          ! ### no strain -> only one file
          i = 1
        open(50000+i,file=&
          './strain_1.00/position_1.00.fdf', status='unknown', &
                err=95, form='formatted', access='sequential') 

        if(vasp_out .eq. 1) then
        open(80000+i,file=&
          './strain_1.00/POSCAR', status='unknown', &
          err=94, form='formatted', access='sequential')
        endif

        strain_i = 0.0
        strain_lo = 0.0
        strain_hi = 0.0
        strain_in = 0.0
        num_fil = 1
        strain_nam = (100 + strain_i) / 100.0
        len_strain_nam = 4
        len_fra_strain_nam = 2
        len_fra_percent = 0
        write(strain_chr_nam, &
          '(f<len_strain_nam>.<len_fra_strain_nam>)') strain_nam

        ! ### not strain or no strain
        else
          goto 91
        endif
        ! ############################## strain or not finished

        ! pre-read all num_data and step
        ! at present no need to obtain value of step
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
        ! uncomment here to choose data, usually 2 is the final data
        string = '2'
        read(string,*,iostat=check) getdata_num
        if(check.gt.0) then
          goto 81
        elseif( num_data.lt.getdata_num.or.getdata_num.lt.1) then
          goto 81
        endif
        write(*,'(i0)') getdata_num

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
        allocate(z_lattice(num_fil))
                
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

        call userreadline( string, '(Integer) : ' )
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
        ! z axis in dump.GNR for x axis in getdata.lammps
        ! fixed temporary, uncomment when choose
        ! 5, 3, 4 -> z, x, y, it depends on structure in material studio
        string = '5'
        read( string,*,iostat=check) order
        if(check>0)then
          goto 71
        elseif (order.lt.3 .or. order.gt.8) then
          goto 71
        endif
        write(*,'(i0)') order
        nx = order - 2

72      continue
        write(*,*)
        write(*,*) &
          'Select which column in dump.GNR for Y axis in getdata.lammps'
        !call userreadline( string, '(Integer) : ' )
        ! 5, 3, 4 -> z, x, y, it depends on structure in material studio
        string = '3'
        read( string,*,iostat=check) order
        if(check>0)then
          goto 72
        elseif (order.lt.3 .or. order.gt.8 .or. order.eq.nx+2) then
          goto 72
        endif
        write(*,'(i0)') order
        ny = order - 2

73      continue
        write(*,*)
        write(*,*) &
          'Select which column in dump.GNR for Z axis in getdata.lammps'
        !call userreadline( string, '(Integer) : ' )
        ! 5, 3, 4 -> z, x, y, it depends on structure in material studio
        string = '4'
        read( string,*,iostat=check) order
        if(check>0)then
          goto 73
        elseif (order.lt.3 .or. order.gt.8 .or. &
            order.eq.nx+2 .or. order.eq.ny+2) then
          goto 73
        endif
        write(*,'(i0)') order
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

        ! ######################### direct or cardinal position
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
        else if (format_co.eq.1) then
          posx2(i,getdata_num)= val(nx,i,getdata_num)
          posy2(i,getdata_num)= val(ny,i,getdata_num)
          posz2(i,getdata_num)= val(nz,i,getdata_num)
        endif
        enddo
        ! ######################### direct or cardinal position

        !###################################################
        ! new added to create POSCAR
        write(*,*)
        write(*,*) 'Running...  Outputing the data to position.fdf'
        if(vasp_out .eq. 1) then
        write(*,*) '            Outputing the data to POSCAR'
        endif
        !###################################################

        ! ############################## position.fdf input
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
        !len_dump is fixed after this

        ! ############################## POSCAR input
        if(vasp_out .eq. 1) then
        write(*,*)
        write(*,*)'********** For POSCAR **********'
        do i = 1, maxid
        write(*,*)
        write(*,'(a,i0,a,i0,a, a<len_dump>)') &
          'Please enter the symbol of element '&
          ,i,', Amount = ',num_ele(i), ' From ', name_ele(i)
        write(*,*) 'ex. C, O, H (autu-trancated over 2 characters)'
        call userreadline( string, '(Symbol of element) : ' )
        ! add only alphabet ascii check? later
        read( string,*) vasp_ele(i)
        enddo
        endif

        ! ############################## fix select
93      continue

        if( vasp_out .eq. 1) then
          write(*,*)
          write(*,*) 'Please choose one type of fixed atoms'
          write(*,*) '(1) No fix;'
          write(*,'(1x, 2a)') '(2) Fix one kind of atoms;', &
            ' (fix Z axis for strain and I-V)'
          write(*,*) '(3) Fix X axis; (Fix atoms in POSCAR)'
          write(*,*) '(4) Fix Y axis; (Fix atoms in POSCAR)'
          write(*,*) '(5) Fix Z axis; (Fix atoms in POSCAR)'
          write(*,'(1x, 2a)') &
            '(6) Fix lowest and highest dimer line;', &
            ' (this for strain and I-V later, fix along Z axis)'

        call userreadline( string, '([1] -> [6]) : ')
        read( string, *, iostat=check) fix_type
        if(check .ne. 0) then
          goto 93
        else
          selectcase (fix_type)
          case (1)
            write(*,*) 
            write(*,*) '(1) No fix;'
          case (2)
92      continue
            write(*,*) 
            write(*,'(1x, 2a)') '(2) Fix one kind of atoms;', &
              '(fix Z axis for strain and I-V)'
            do i = 1, maxid
        write(*,'(a,i0,a,i0,a, a<len_dump>)') &
          ' [',i,'], Amount = ',num_ele(i), ' From ', name_ele(i)
            enddo
            call userreadline( string, '(Integer) : ')
            read( string, *, iostat=check) sele_fix
            if( check .ne. 0) then
              goto 92
            elseif( sele_fix .lt. 1 .or. sele_fix .gt. maxid) then
              goto 92
            endif
            ! ****************************** vector perpendicular to
            ! surface layer always fixed
            ! user can modify later
            write(*,*)
            write(*,'(1x,a, a<len_dump>)') &
              'You selected : ', name_ele(sele_fix)
            fix_x = 'T'
            fix_y = 'F'
            fix_z = 'T'
          case (3)
            write(*,*) 
            write(*,*) '(3) Fix X axis; (Fix atoms in POSCAR)'
            fix_x = 'T'
            fix_y = 'F'
            fix_z = 'F'
          case (4)
            write(*,*) 
            write(*,*) '(4) Fix Y axis; (Fix atoms in POSCAR)'
            fix_x = 'T'
            fix_y = 'T'
            fix_z = 'F'
          case (5)
            write(*,*) 
            write(*,*) '(5) Fix Z axis; (Fix atoms in POSCAR)'
            fix_x = 'T'
            fix_y = 'F'
            fix_z = 'T'
          case (6)
            write(*,*) 
            write(*,'(1x, 2a)') &
              '(6) Fix lowest and highest dimer line;', &
              ' (this for strain and I-V later, fix along Z axis)'
            fix_x = 'T'
            fix_y = 'F'
            fix_z = 'T'
          case default
            goto 93
          endselect
        endif
        endif
        ! ############################## fix select finished

        ! ##################################################
        ! special case for strain and fix atoms
        ! ##################################################
        ! ############################## fix 2 speciees -> 
        ! fix but with strain applied, or fix and without strain
        ! ex. wide part fix -> apply strain will not affect position of
        ! wide part
        ! === new fixed wide part stran
        fix_species = 0
        ! === new fixed wide part stran

      if( fix_type .eq. 2 .and. strain_chs .eq. 1) then
        write(*,*)
        write(*,*) 'this is special case for fix species with strain'
300   continue
        write(*,*) 'Fixed species with / without strain'
        write(*,*) 'Special for dumbbell-shape -> wide / narrow'
        write(*,*) '( this time only can fix wide segment C atoms)'
        call userreadline( string, &
          '([1] with (normal); [2] without (new); ) : ')
        read( string, *, iostat=chk_fix) fix_species
        if( chk_fix .ne. 0) goto 300
        if( fix_species .eq. 1) then
          write(*,*)
          write(*,*) '[1] with ;'
          ! fixed part with strain -> normal situation
          ! escape from here directly
        elseif( fix_species .eq. 2) then
          write(*,*)
          write(*,*) '[2] without ;'
          ! fixed part without strain -> new part
          ! because narrwo and wide part have different yield ratio
          ! posx2,posy2, posz2
          ! no change with strain fixed part
          ! new lattice parameter between wide part fixed

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
            ! here maybe system bug
            ! without below line, 17112005 can not get z_l value
            ! therefore, no z_narrow_hi
            ! but 29112005 same condition -> works
            ! very weird need below line
            !write(*,*) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            ! up is only for 17112005, 29112005 works fine
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
              !write(*,*) l, num_wide
              re_num_narrow = num_wide - 1
              re_num_wide = num_wide - 1
              z_narrow_lo = x_pos(3, (j-num_wide-l+2), &
                x_id(j-num_wide-l+2))
            endif
            if( dime_ly .gt. 2) then
            if( re_num_wide + 1 .lt. num_wide) then
              !write(*,*) l, num_wide, z_l
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
        write(*,*) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        write(*,*) re_num_narrow, z_narrow_lo, re_num_wide, z_narrow_hi
        ! wired error, must have two blank lines, then 17112005 not okay
            !write(*,*) '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      endif
      endif
        ! ############################## fix select finished
        ! ##################################################

        ! ######################### write getdata.lammps head #####
        if( strain_chs .eq. 1) then
          ! new add for strain, real no format, due to decimal after dot
          write(40,'(a,1x,5(i0,1x),f,3(1x,i0),(a))') &
          '#', fix_species, strain_chs, num_fil, strain_lo, strain_hi,&
          strain_in, len_strain_nam, len_fra_strain_nam,&
          len_fra_percent, ' w strain (not sorted)'
        elseif( strain_chs .eq. 2) then
          write(40,'(a,1x,4(i0,1x),f,3(1x,i0),(a))') &
          '#', fix_species, strain_chs, num_fil, strain_lo, strain_hi,&
          strain_in, len_strain_nam, len_fra_strain_nam,&
          len_fra_percent, ' w/o strain (not sorted)'
        endif
          write(40,'(i0,1x,a)')num,'atoms'
          write(40,'(i0,1x,a,3(1x,i0))')maxid, 'atom types',&
            (num_ele(j),j=1,maxid)
          write(40,'(2(1x,f11.6),1x,a)')&
            cell_lo(nx,getdata_num), cell_hi(nx,getdata_num),'xlo xhi'
          write(40,'(2(1x,f11.6),1x,a)')&
            cell_lo(ny,getdata_num), cell_hi(ny,getdata_num),'ylo yhi'
          write(40,'(2(1x,f11.6),1x,a)')&
            cell_lo(nz,getdata_num), cell_hi(nz,getdata_num),'zlo zhi'
          write(40,*)
          write(40,*)'Atoms'
          write(40,*)

        ! ######################### write getdata.lammps body #####
        do i = 1, num
        if (format_co.eq.2) then
          write(40,'(1x,i7,1x,i2,3(1x,f11.6))')&
          i, id(i), posx2(i,getdata_num),&
          posy2(i,getdata_num),posz2(i,getdata_num)
        else if (format_co.eq.1) then
          write(40,'(1x,i7,1x,i2,3(1x,f11.6))')&
          i, id(i), posx2(i,getdata_num),&
          posy2(i,getdata_num),posz2(i,getdata_num)
        endif
        enddo
        ! ######################### finished write getdata.lammps ##

        !------------------------------ lowest and highest atoms
        if( vasp_out .eq. 1) then
          hi_z = 0
          lo_z = posz2(1, getdata_num)
          do i = 1, num
          if( posz2(i, getdata_num) .gt. hi_z) then
            hi_z = posz2(i, getdata_num)
          endif
          if( posz2(i, getdata_num) .lt. lo_z) then
            lo_z = posz2(i, getdata_num)
          endif
          enddo
        endif
        !------------------------------ lowest and highest atoms

        write(*,*)
        write(*,*) 'Size of supercell'
        write(*,'(a, 1x, f11.6)') 'x =', cellx(getdata_num)
        write(*,'(a, 1x, f11.6)') 'y =', celly(getdata_num)
        write(*,'(a, 1x, f11.6)') 'z =', cellz(getdata_num)
        write(*,*)
        write(*,'(1x,a,1x,i0)') 'Total steps in LAMMPS =', step
        write(*,'(1x,a,1x,i0)') 'Number of data =',num_data
        write(*,'(1x,a,1x,i0)') 'Number of obtained data =',getdata_num
        write(*,*)
        write(*,'(1x,a,1x,i0)') 'total number of atoms =', num
        write(*,'(1x,a,1x,i0)') 'total types of atoms =',maxid
        do i=1,maxid
                write(*,'(1x,a,1x,i0,1x,a,1x,i0)')'type (',i,')&
                        element', num_ele(i) 
        enddo        
              
        write(*,*)
        write(*,*)'file completed...'
        write(*,*)'getdata.lammps created...'

        zhi_check = 0
        ! ############################## write files
        ! include strain or not strain with strain_nam as strain ratio
        strain_i = strain_lo
        do m = 1, num_fil
        strain_nam = (100 + strain_i) / 100.0
        ! headpart of POSCAR
        if(vasp_out .eq. 1) then
        write(80000+m,'(a)') 'POSCAR(VASP)_with_position(SIESTA)'
        write(80000+m,'(a)') '1.0'
        real_zero = 0.000000
        write(80000+m, '(3(1x,f11.6))') &
          cellx(getdata_num), real_zero, real_zero 
        write(80000+m, '(3(1x,f11.6))') &
          real_zero, celly(getdata_num), real_zero 
        write(80000+m, '(3(1x,f11.6))') &
          real_zero, real_zero, cellz(getdata_num)*strain_nam
        write(80000+m, '(<maxid>(1x, a5))') &
          (adjustr(vasp_ele(i)), i = 1, maxid)
        write(80000+m, '(<maxid>(1x, i5))') (num_ele(i), i = 1, maxid)

        write(80000+m, '(a)') 'Selective dynamics' 
        ! always add selective dynamics even all F F F
        if( format_co .eq. 1) then
        write(80000+m, '(a)') 'Direct'
        elseif( format_co .eq. 2) then
        write(80000+m, '(a)') 'Cartesian'
        endif

        endif

        ! headline of position.fdf
        write(50000+m,'(a)')'%block AtomicCoordinatesAndAtomicSpecies' 

        num_ele_low = 1
        num_ele_hi = 0
        do j=1, maxid
        num_ele_hi = num_ele_hi + num_ele(j)
        do i = num_ele_low, num_ele_hi
          ! position.fdf
      if( fix_species .eq. 2) then
        ! new species fixed strain only narrow part

        if( posz2(i,getdata_num) .lt. z_narrow_lo) then
          write(50000+m,'(3(1x,f11.6), 1x, i2, 1x, a<len_dump>)')&
          posx2(i,getdata_num), &
          posy2(i,getdata_num), &
          posz2(i,getdata_num), id(i), name_ele(j)
        elseif( posz2(i,getdata_num) .le. z_narrow_hi) then
          write(50000+m,'(3(1x,f11.6), 1x, i2, 1x, a<len_dump>)')&
          posx2(i,getdata_num), &
          posy2(i,getdata_num), &
          posz2(i,getdata_num)+&
          (posz2(i,getdata_num)-z_narrow_lo+0.710136)*&
          (strain_nam-1.0), &
          id(i), name_ele(j)
        elseif( posz2(i,getdata_num) .gt. z_narrow_hi) then
          write(50000+m,'(3(1x,f11.6), 1x, i2, 1x, a<len_dump>)')&
          posx2(i,getdata_num), &
          posy2(i,getdata_num), &
          posz2(i,getdata_num)+&
          (z_narrow_hi-z_narrow_lo+1.420272)*(strain_nam-1.0), &
          id(i), name_ele(j)
        ! === new add z_lattice
        ! === check zhi_check is the largest
        if( posz2(i,getdata_num) .gt. zhi_check) then
          zhi_check = posz2(i,getdata_num)
        endif
        z_lattice(m) = &
          zhi_check + 1.420272/2 + &
          (z_narrow_hi-z_narrow_lo+1.420272)*(strain_nam-1.0)
        ! === largest + local strain
        ! === new add z_lattice
        endif

      else
          write(50000+m,'(3(1x,f11.6), 1x, i2, 1x, a<len_dump>)')&
          posx2(i,getdata_num), &
          posy2(i,getdata_num), &
          posz2(i,getdata_num)*strain_nam, id(i), name_ele(j)
      endif
          ! POSCAR with relaxation all F F F
          if(vasp_out .eq. 1) then
            if( fix_type .ge. 3 .and. fix_type .le. 5) then
          write(80000+m,'(3(1x,f11.6),3(1x,a), 2a)') &
          posx2(i,getdata_num), &
          posy2(i,getdata_num), &
          posz2(i,getdata_num)*strain_nam, fix_x, fix_y, fix_z, &
          ' # ', trim(adjustl(vasp_ele(j)))

            elseif( fix_type .eq. 2) then
              if( j .eq. sele_fix) then
      if( fix_species .eq. 2) then
        ! new species fixed strain only narrow part
        if( posz2(i,getdata_num) .lt. z_narrow_lo) then
          write(80000+m,'(3(1x,f11.6),3(1x,a), 2a)') &
          posx2(i,getdata_num), &
          posy2(i,getdata_num), &
          posz2(i,getdata_num), fix_x, fix_y, fix_z, &
          ' # ', trim(adjustl(vasp_ele(j)))

        elseif( posz2(i,getdata_num) .le. z_narrow_hi) then
          write(80000+m,'(3(1x,f11.6),3(1x,a), 2a)') &
          posx2(i,getdata_num), &
          posy2(i,getdata_num), &
          posz2(i,getdata_num)+&
          (posz2(i,getdata_num)-z_narrow_lo+0.710136)*&
          (strain_nam-1.0), &
          fix_x, fix_y, fix_z, &
          ' # ', trim(adjustl(vasp_ele(j)))
        elseif( posz2(i,getdata_num) .gt. z_narrow_hi) then
          write(80000+m,'(3(1x,f11.6),3(1x,a), 2a)') &
          posx2(i,getdata_num), &
          posy2(i,getdata_num), &
          posz2(i,getdata_num)+&
         (z_narrow_hi-z_narrow_lo+1.420272)*(strain_nam-1.0), &
          fix_x, fix_y, fix_z, &
          ' # ', trim(adjustl(vasp_ele(j)))
        endif

      else
          write(80000+m,'(3(1x,f11.6),3(1x,a), 2a)') &
          posx2(i,getdata_num), &
          posy2(i,getdata_num), &
          posz2(i,getdata_num)*strain_nam, fix_x, fix_y, fix_z, &
          ' # ', trim(adjustl(vasp_ele(j)))
      endif
              else
          write(80000+m,'(3(1x,f11.6),2a)') &
          posx2(i,getdata_num), &
          posy2(i,getdata_num), &
          posz2(i,getdata_num)*strain_nam, &
          ' T F F # ', trim(adjustl(vasp_ele(j)))
              endif
            elseif( fix_type .eq. 6) then
              if( posz2(i,getdata_num) .eq. hi_z &
                .or. posz2(i, getdata_num) .eq. lo_z) then
          write(80000+m,'(3(1x,f11.6),3(1x,a), 2a)') &
          posx2(i,getdata_num), &
          posy2(i,getdata_num), &
          posz2(i,getdata_num)*strain_nam, fix_x, fix_y, fix_z, &
          ' # ', trim(adjustl(vasp_ele(j)))
              else
          write(80000+m,'(3(1x,f11.6),2a)') &
          posx2(i,getdata_num), &
          posy2(i,getdata_num), &
          posz2(i,getdata_num)*strain_nam, &
          ' T F F # ', trim(adjustl(vasp_ele(j)))
              endif

            elseif( fix_type .eq. 1) then
          write(80000+m,'(3(1x,f11.6),2a)') &
          posx2(i,getdata_num), &
          posy2(i,getdata_num), &
          posz2(i,getdata_num)*strain_nam, &
          ' T F F # ', trim(adjustl(vasp_ele(j)))
            endif
          endif
          ! # is comment line in vasp and siesta
        enddo
        num_ele_low = num_ele_low + num_ele(j)
        enddo
        ! endline of position fdf
        write(50000+m,'(a)') &
          '%endblock AtomicCoordinatesAndAtomicSpecies' 

        strain_i = strain_i + strain_in

        if( vasp_out .eq. 1) then
        rewind(80000+m)
        close(80000+m)
        endif
        rewind(50000+m)
        close(50000+m)
        enddo

        ! write getdata.lammps bottom line for later use
        ! z_lattice for fix_species 2, only wide fixed with no strain
        if( fix_species .eq. 2) then
        write(40, '(a,<num_fil>(1x,f11.6))') '#',&
          (z_lattice(m), m = 1, num_fil)
        ! z_lattice for fix_species 2, only wide fixed with no strain
        ! write getdata.lammps bottom line for later use

        ! use template file to rewrite POSCAR with correct Z lattice
        if( strain_chs .eq. 1) then
        if(vasp_out .eq. 1) then
          i = 0
          do strain_i = strain_lo, strain_hi, strain_in
          i = i + 1
          strain_nam = (100 + strain_i) / 100.0
          write(strain_chr_nam, &
            '(f<len_strain_nam>.<len_fra_strain_nam>)') strain_nam
          open(80000+i, &
          file=&
          './strain_'//trim(adjustl(strain_chr_nam))//'/POSCAR',&
          status='old', &
          err=94, form='formatted', access='sequential') 
          open(20000+i, &
          file=&
          './strain_'//trim(adjustl(strain_chr_nam))//'/POSCAR.tmp',&
          status='unknown', &
          err=94, form='formatted', access='sequential') 
          enddo
        endif
        elseif( strain_chs .eq. 2) then
        if(vasp_out .eq. 1) then
          i = 1
          open(80000+i,file=&
          './strain_1.00/POSCAR', status='old', &
          err=94, form='formatted', access='sequential')
          open(20000+i,file=&
          './strain_1.00/POSCAR.tmp', status='unknown', &
          err=94, form='formatted', access='sequential')
        endif
        endif
        ! use template file to rewrite POSCAR with correct Z lattice

        ! === read & write ===
        do i = 1, num_fil
        do j = 1, 4
        read(80000+i, '(a)') dummy
        write(20000+i, '(a)') trim(dummy)
        enddo
        ! the changed line for z_lattice
        read(80000+i, '(a)') dummy
        write(20000+i, '(3(1x,f11.6))') &
          real_zero, real_zero, z_lattice(i)
        ! the changed line for z_lattice
        ! add spaces and line
        do j = 1, num + 4
        read(80000+i, '(a)') dummy
        write(20000+i, '(a)') trim(dummy)
        enddo
        close(80000+i)
        close(20000+i)
        enddo
        ! copy POSCAR.tmp to POSCAR
          i = 0
          do strain_i = strain_lo, strain_hi, strain_in
          i = i + 1
          strain_nam = (100 + strain_i) / 100.0
          write(strain_chr_nam, &
            '(f<len_strain_nam>.<len_fra_strain_nam>)') strain_nam
          status = rename(&
          './strain_'//trim(adjustl(strain_chr_nam))//'/POSCAR.tmp',&
          './strain_'//trim(adjustl(strain_chr_nam))//'/POSCAR')
          enddo
        endif

        write(*,*)
        write(*,*)'Outputing completed'
        write(*,*)

        if( strain_chs .eq. 1) then
        call execute_command_line(&
          'cp strain_1.00/position_1.00.fdf position.fdf')
        write(*,*)'strain_*/position_*.fdf created...'
        write(*,*)'position.fdf created...'
        if(vasp_out .eq. 1) then
        call execute_command_line(&
          'cp strain_1.00/POSCAR POSCAR')
          write(*,*) 'strain_*/POSCAR with strain created...'
          write(*,*) 'POSCAR created...'
        endif
        elseif( strain_chs .eq. 2) then
        call execute_command_line(&
          'cp strain_1.00/position_1.00.fdf position.fdf')
        write(*,*)'position.fdf created...'
        if(vasp_out .eq. 1) then
        call execute_command_line(&
          'cp strain_1.00/POSCAR POSCAR')
          write(*,*) 'POSCAR created...'
        endif
        endif

        write(*,*)
        write(*,*) 'Please check ./position.fdf created or not...'
        write(*,*) 'Currently, ./position.fdf is for crtfil4trasief90'
        write(*,*)
        
        deallocate(z_lattice)
        deallocate(id)
        deallocate(num_ele)
        deallocate(val) 
        deallocate(cell_lo,cell_hi) 
        deallocate(cellx,celly,cellz)
        deallocate(posx2,posy2,posz2)
        deallocate(name_ele)
        deallocate(vasp_ele)

      if( fix_species .eq. 2) then
        deallocate(tmp_name_ele)
        deallocate(x_pos)
        deallocate(x_id)
        deallocate(y_id)
        deallocate(x_name_ele)
      endif

        rewind(40)
        close(10)
        close(40)

        if( renum .eq. 1) then
          call changeatnum(renum)
        endif

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

95      continue
        write(*,*)
        write(*,*)'Error: Can not open a position.fdf file.'
        write(*,*)
        goto 99

94      continue
        write(*,*)
        write(*,*)'Error: Can not open a POSCAR file.'
        write(*,*)
        goto 99

99      stop

        endprogram lmpdum2pos
