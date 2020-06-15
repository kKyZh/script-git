      program sumldos
        use jsu_readline
        implicit none
        integer i,j,k,l
        integer chk_chotyp, chotyp ! choose type and check
        integer chk_atspe ! check of atom species
        integer nuatspe, chk_nuatspe ! how many atom species and check
        integer chk_contin ! check input of continue or not
        integer seltd ! selected species and atoms
        integer r2e ! read to the end
        integer ln_co ! number of comment lines
        integer ln_dat ! number of data lines
        integer minatnu ! interval selection parameters
        integer maxatnu ! interval selection parameters
        integer chk_minatnu ! interval selection parameters
        integer chk_maxatnu ! interval selection parameters
        integer intvl ! interval selection parameters
        integer chk_intvl ! interval selection parameters
        integer rang ! interval selection parameters
        integer totfil ! total files
        integer nufilnam ! number of file name
        integer num, maxid ! # of totatom and totatomtypes
        integer sort_num ! total atoms after sort, w/o H atoms
        integer tmp_int ! swap sort
        integer lo, hi ! bubble sort
        integer subs_ln ! # of atoms in different line
        integer dim_ln ! # of dimer C line
        integer ln_at_lo, ln_at_hi ! # of atom in one line low to high
        integer, allocatable :: id(:), typ_at(:) ! position type atom
        integer, allocatable :: num_ele(:) ! element number
        integer, allocatable :: width(:) ! # of C atom each line
        real(8) tmp_real ! swap sort
        real(8), allocatable :: pos_x(:), pos_y(:), pos_z(:) ! position
        ! atom species and atoms use same variables
        real(8), allocatable :: posx(:), posy(:) ! read x and y of ldos
        real(8), allocatable :: sum_ldos(:) ! sum value of y axis
        character(2) contin ! choose continue or not
        character(8) char_chotyp ! choose type character
        character(16), allocatable :: atspe(:) ! atom species
        character(32) filnam ! check file name
        character(32) char_outnam ! serial outfile names
        character(32) char_nufilnam ! check file name
        character(128) dummy ! read file
        logical chk_filex ! check file exist or not
        logical fnd_co ! found the # symbol
        character(128) string ! C interoperate

        write(*,*)
        write(*,'(1x,3a)')'Running... sum the value of LDOS, ',&
                'then output a summarized *.dat file ',&
                '(Version --1.2 beta//Feb/17/2020//)'
        write(*,*)'Please run getldos before this script'

101     continue
        write(*,*)
        write(*,*)'Please choose type'
        write(*,*)'(1) Atom species;'
        write(*,*)'(2) Atom number (Individual);'
        write(*,*)'(3) Atom number (Range);'
        write(*,*)'(4) Dimer lines;'
        write(*,*)'(5) Honeycomb lines (Confirm structure before use);'
        write(*,*)'(Q) Quit;'
        call userreadline( string, '(Enter type) : ')
        read( string,*,iostat=chk_chotyp)char_chotyp

        if(chk_chotyp.ne.0)then
          goto 101
        else

        select case (char_chotyp)
        case('1')
          read(char_chotyp,*)chotyp
          write(*,*)
          write(*,*)'You choose (1) Atom species;'
          write(*,*)
          goto 102

        case('2')
          read(char_chotyp,*)chotyp
          write(*,*)
          write(*,*)'You choose (2) Atom number (Individual);'
          write(*,*)
          goto 103

        case('3')
          read(char_chotyp,*)chotyp
          write(*,*)
          write(*,*)'You choose (3) Atom number (Range);'
          write(*,*)
          goto 104

        case('4')
          read(char_chotyp,*)chotyp
          write(*,*)
          write(*,*)'You choose (4) Dimer lines;'
          write(*,*)
          goto 205

        case('5')
          read(char_chotyp,*)chotyp
          write(*,*)
          write(*,*)'You choose (5) Honeycomb lines;'
          write(*,*)
          goto 205
          ! same pattern 4 & 5 use if do loop

        case('q','Q')
          write(*,*)
          write(*,*)'You choose quit... bye!!!'
          write(*,*)
          goto 999
        case default
          goto 101
        endselect
        endif

102   continue
        write(*,*)
        write(*,*)'How many atom species do you have?'
        call userreadline( string, '(Integer number) : ')
        read( string,*,iostat=chk_nuatspe)nuatspe
        if(chk_nuatspe.ne.0)then
          goto 102
        elseif(nuatspe.le.0)then
          write(*,*)
          write(*,*)'Error : Need a number > 0;'
          goto 102
        else
          allocate(atspe(nuatspe))
        endif
        do i=1,nuatspe

1021    continue
        write(*,*)
        write(*,*)'----------------------------------------------------'
        write(*,*)'List of all PDOS_*.dat files'
        write(*,*)
        call system('ls PDOS_*.dat')
        write(*,*)
        write(*,'(1x,<seltd+1>(a,1x),/)',advance='no')&
          'You selected :',(trim(atspe(j)),j=1,seltd)
        write(*,*)'----------------------------------------------------'
        write(*,*)
        write(*,'(1x,a,i0,a)')'Please enter NO.',i,&
          ' atom species (not *.dat file name)'
        call userreadline( string, '(Species) : ')
        read( string,*,iostat=chk_atspe)atspe(i)
        if(chk_atspe.ne.0)then
          goto 1021
        else
          do k=1,seltd
          if(atspe(i)==atspe(k))then
          write(*,*)
          write(*,*)'Error : You already entered it, try again;'
          goto 1021
          endif
          enddo
          inquire(file='PDOS_'//trim(atspe(i))//'.dat',exist=chk_filex)
          if(chk_filex==.false.)then
            write(*,*)
            write(*,*)'Error : File not exist, try again'
            goto 1021
          else
            seltd=i
          endif
        endif
        enddo

        write(*,*)
        write(*,*)'----------------------------------------------------'
        write(*,'(1x,a,i0,a)')'You selected ',nuatspe,' species : '
        write(*,*)
        do i=1,nuatspe
        write(*,*)trim(atspe(i))
        enddo
        write(*,*)'----------------------------------------------------'

1022    continue
        write(*,*)
        write(*,*)'Are you sure to continue?'
        write(*,'(1x,a)',advance='no')'==>   '
        call userreadline( string, '(Y/N) : ')
        read( string,*,iostat=chk_contin)contin
        if(chk_contin.ne.0)then
          goto 1022
        else
          selectcase(contin)
            case('y','Y')
              goto 1023
            case('n','N')
              write(*,*)
              write(*,*)'You choose quit... bye!!!'
              write(*,*)
              deallocate(atspe)
              goto 999
            case default
              goto 1022
            endselect
          endif

1023    continue
          open(5001,file='PDOS_'//trim(atspe(1))//'.dat',&
            form='formatted',status='old',err=998,access='sequential')
          ! do pre-read first
          j=0
          k=0
          do 
          read(5001,'(a)',iostat=r2e)dummy
          if(r2e.ne.0)then
            goto 1024
          else
            fnd_co=(index(dummy,'#')/=0)
            if(fnd_co)then
              j=j+1
            else
              k=k+1
            endif
          endif
          enddo

1024    continue
          close(5001)
          ln_co=j-1 ! one comment line at bottom
          ln_dat=k
          allocate(posx(ln_dat))
          allocate(posy(ln_dat))
          allocate(sum_ldos(ln_dat))
          do i=1,ln_dat
          sum_ldos(i)=0
          enddo

          do i=1,nuatspe
          open(5000+i,file='PDOS_'//trim(atspe(i))//'.dat',&
            form='formatted',status='old',err=998,access='sequential')
          do l=1,ln_co
          read(5000+i,'(a)')dummy
          enddo
          do l=1,ln_dat
          ! ###### ------ this line might be wrong if previous files
          ! ###### ------ changed, needs to find a better way
          read(5000+i,'(1x,f11.6,3x,f11.6)')posx(l),posy(l) ! fixed read
          sum_ldos(l)=sum_ldos(l)+posy(l)
          enddo
          enddo
          open(5000,file='PDOS_sumldos.dat',form='formatted',&
            status='unknown',err=997,access='sequential')
          write(5000,'(a,/,a,/,a,<nuatspe>(1x,a),/,a)')&
            '#','# Atom species : ','#',(trim(atspe(i)),i=1,nuatspe),'#'
          do i=1,ln_dat
          write(5000,'(1x,f11.6,3x,f11.6)')posx(i),sum_ldos(i)
          enddo
          write(*,*)
          write(*,*)'File PDOS_sumldos.dat created'
          write(*,*)
          ! #########################################################
          ! ###### ------ later use if loop to decrease script lines
          ! #########################################################
        goto 105

103   continue
        write(*,*)
        write(*,*)'How many atoms do you have?'
        call userreadline( string, '(Integer) : ')
        read( string,*,iostat=chk_nuatspe)nuatspe
        if(chk_nuatspe.ne.0)then
          goto 103
        elseif(nuatspe.le.0)then
          write(*,*)
          write(*,*)'Error : Need a number > 0;'
          goto 103
        else
          allocate(atspe(nuatspe))
        endif
        seltd=0
        do i=1,nuatspe

1031    continue
        write(*,*)
        write(*,*)'----------------------------------------------------'
        write(*,*)'List of all PDOS_*.dat files'
        write(*,*)
        call system('ls PDOS_*.dat')
        write(*,*)
        write(*,'(1x,<seltd+1>(a,1x),/)',advance='no')&
          'You selected :',(trim(atspe(j)),j=1,seltd)
        write(*,*)'----------------------------------------------------'
        write(*,*)
        write(*,'(1x,a,i0,a)')'Please enter NO.',i,&
          ' atom number (not *.dat file name)'
        call userreadline( string, '(number) : ')
        read( string,*,iostat=chk_atspe)atspe(i)
        if(chk_atspe.ne.0)then
          goto 1031
        else
          do k=1,seltd
          if(atspe(i)==atspe(k))then
          write(*,*)
          write(*,*)'Error : You already entered it, try again;'
          goto 1031
          endif
          enddo
          inquire(file='PDOS_'//trim(atspe(i))//'.dat',exist=chk_filex)
          if(chk_filex==.false.)then
            write(*,*)
            write(*,*)'Error : File not exist, try again'
            goto 1031
          else
            seltd=i
          endif
        endif
        enddo
        write(*,*)
        write(*,*)'----------------------------------------------------'
        write(*,'(1x,a,i0,a)')'You selected ',nuatspe,' atoms : '
        write(*,*)
        do i=1,nuatspe
        write(*,*)trim(atspe(i))
        enddo
        write(*,*)'----------------------------------------------------'

1032    continue
        write(*,*)
        write(*,*)'Are you sure to continue?'
        call userreadline( string, '(Y/N) : ')
        read( string,*,iostat=chk_contin)contin
        if(chk_contin.ne.0)then
          goto 1032
        else
          selectcase(contin)
            case('y','Y')
              goto 1033
            case('n','N')
              write(*,*)
              write(*,*)'You choose quit... bye!!!'
              write(*,*)
              deallocate(atspe)
              goto 999
            case default
              goto 1032
            endselect
          endif

1033    continue
          open(5001,file='PDOS_'//trim(atspe(1))//'.dat',&
            form='formatted',status='old',err=998,access='sequential')
          ! do pre-read first
          j=0
          k=0
          do 
          read(5001,'(a)',iostat=r2e)dummy
          if(r2e.ne.0)then
            goto 1034
          else
            fnd_co=(index(dummy,'#')/=0)
            if(fnd_co)then
              j=j+1
            else
              k=k+1
            endif
          endif
          enddo

1034    continue
          close(5001)
          ln_co=j-1 ! one comment line at bottom
          ln_dat=k
          allocate(posx(ln_dat))
          allocate(posy(ln_dat))
          allocate(sum_ldos(ln_dat))
          do i=1,ln_dat
          sum_ldos(i)=0
          enddo

          do i=1,nuatspe
          open(5000+i,file='PDOS_'//trim(atspe(i))//'.dat',&
            form='formatted',status='old',err=998,access='sequential')
          do l=1,ln_co
          read(5000+i,'(a)')dummy
          enddo
          do l=1,ln_dat
          ! ###### ------ this line might be wrong if previous files
          ! ###### ------ changed, needs to find a better way
          read(5000+i,'(1x,f11.6,3x,f11.6)')posx(l),posy(l) ! fixed read
          sum_ldos(l)=sum_ldos(l)+posy(l)
          enddo
          enddo
          open(5000,file='PDOS_sumldos.dat',form='formatted',&
            status='unknown',err=997,access='sequential')
          write(5000,'(a,/,a,/,a,<nuatspe>(1x,a),/,a)')&
            '#','# Atom number : ','#',(trim(atspe(i)),i=1,nuatspe),'#'
          do i=1,ln_dat
          write(5000,'(1x,f11.6,3x,f11.6)')posx(i),sum_ldos(i)
          enddo
          write(*,*)
          write(*,*)'File PDOS_sumldos.dat created'
          write(*,*)
          ! #########################################################
          ! ###### ------ later use if loop to decrease script lines
          ! #########################################################
        goto 105

104     continue
          ! #########################################################
          ! ###### ------ find a solution when enter real go error
          ! ###### ------ , . / etc. can be read as integer???
          ! #########################################################
        write(*,*)
        write(*,*)'Please enter the minimum atom number'
        write(*,*)'(Decimal number will be ignored)'
        call userreadline( string, '(Integer) : ')
        read( string,*,iostat=chk_minatnu)minatnu
        if(chk_minatnu.ne.0)then
          goto 104
        elseif(minatnu.le.0)then
          write(*,*)
          write(*,*)'Error : Need an integer > 0, try again;'
          goto 104
        else
        endif

1041    continue
        write(*,*)
        write(*,*)'Please enter the maximum atom number'
        write(*,*)'(Decimal number will be ignored)'
        call userreadline( string, '(Integer) : ')
        read( string,*,iostat=chk_maxatnu)maxatnu
        if(chk_maxatnu.ne.0)then
          goto 1041
        elseif(maxatnu.lt.minatnu)then
          write(*,*)
          write(*,'(1x,a,i0,a)')&
            'Error : Need an integer > ',minatnu,', try again;'
          goto 1041
        else
        endif
        rang=maxatnu-minatnu
        if(rang==0)then
          intvl=0
        else

1042    continue
        write(*,*)
        write(*,*)'Please enter the interval'
        write(*,*)'(Decimal number will be ignored)'
        call userreadline( string, '(Integer) : ')
        read( string,*,iostat=chk_intvl)intvl
        if(chk_intvl.ne.0)then
          goto 1042
        elseif(intvl.le.0.or.intvl.gt.rang)then
          write(*,*)
          write(*,'(1x,a,i0)')&
            'Error : Need 0 < interval <= ',rang
          goto 1042
        elseif(mod(rang,intvl).ne.0)then
          write(*,*)
          write(*,'(1x,a,i0)')&
            'Error : Need an integer can divide ',rang
          goto 1042
        else
          totfil=rang/intvl+1
        endif
        endif
        nufilnam=minatnu
        write(char_nufilnam,'(i0)')nufilnam

        j=0
        write(*,*)
        do i=1,totfil
          inquire&
            (file='PDOS_'//trim(char_nufilnam)//'.dat',exist=chk_filex)
          if(chk_filex==.false.)then
            write(*,*)&
              'File "PDOS_'//trim(char_nufilnam)//'.dat" not exist'
          elseif(chk_filex==.true.)then
            write(*,*)&
              'File "PDOS_'//trim(char_nufilnam)//'.dat" exist'
          ! do pre-read first
          open(5001,file='PDOS_'//trim(char_nufilnam)//'.dat',&
            form='formatted',status='old',err=996,access='sequential')
          l=0
          k=0
          do 
          read(5001,'(a)',iostat=r2e)dummy
          if(r2e.ne.0)then
            goto 1043
          else
            fnd_co=(index(dummy,'#')/=0)
            if(fnd_co)then
              l=l+1
            else
              k=k+1
            endif
          endif
          enddo

1043    continue
            close(5001)
            ln_co=l-1 ! one comment line at bottom
            ln_dat=k
            j=j+1
          endif
          nufilnam=nufilnam+intvl
          write(char_nufilnam,'(i0)')nufilnam
        enddo
          write(*,*)
          if(j.eq.0)then
            write(*,*)'Error : No selected files;'
            write(*,*)
            goto 999
          endif
        if(totfil.ne.j)then
          write(*,*)&
            'Warning : Assumed files not equal to Selected files'
          write(*,'(1x,a,i0)')'Assumed files : ',totfil
          write(*,'(1x,a,i0)')'Selected files : ',j

1044    continue
        write(*,*)
        write(*,*)'Are you sure to continue?'
        call userreadline( string, '(Y/N) : ')
        read( string,*,iostat=chk_contin)contin
        if(chk_contin.ne.0)then
          goto 1044
        else
          selectcase(contin)
            case('y','Y')
              goto 1045
            case('n','N')
              write(*,*)
              write(*,*)'You choose quit... bye!!!'
              write(*,*)
              goto 999
            case default
              goto 1044
          endselect
        endif
        else
          write(*,'(1x,a,i0)')'Assumed files : ',totfil
          write(*,'(1x,a,i0)')'Selected files : ',j
        endif

1045    continue
          allocate(posx(ln_dat))
          allocate(posy(ln_dat))
          allocate(sum_ldos(ln_dat))
          do i=1,ln_dat
          sum_ldos(i)=0
          enddo
        nufilnam=minatnu
        write(char_nufilnam,'(i0)')nufilnam
        j=0
        do i=1,totfil
          inquire&
            (file='PDOS_'//trim(char_nufilnam)//'.dat',exist=chk_filex)
          if(chk_filex==.true.)then
          open(5000+i,file='PDOS_'//trim(char_nufilnam)//'.dat',&
            form='formatted',status='old',err=996,access='sequential')
          do l=1,ln_co
          read(5000+i,'(a)')dummy
          enddo
          do l=1,ln_dat
          ! ###### ------ this line might be wrong if previous files
          ! ###### ------ changed, needs to find a better way
          read(5000+i,'(1x,f11.6,3x,f11.6)')posx(l),posy(l) ! fixed read
          sum_ldos(l)=sum_ldos(l)+posy(l)
          enddo
          endif
          nufilnam=nufilnam+intvl
          write(char_nufilnam,'(i0)')nufilnam
        enddo
          open(5000,file='PDOS_sumldos.dat',form='formatted',&
            status='unknown',err=997,access='sequential')
          write(5000,'(a,/,a,/,a,/,a)')&
            '#','# Atom range','#','#'
          do i=1,ln_dat
          write(5000,'(1x,f11.6,3x,f11.6)')posx(i),sum_ldos(i)
          enddo
          write(*,*)
          write(*,*)'File PDOS_sumldos.dat created'
          write(*,*)
        goto 106

        !########## 
        !########## 
        !########## 
        !########## 
        ! new part of script from here

205     continue
        ! check file getdata.lammps exists
        ! dimer line 4 choose type
        ! honeycomb line 5 choose type

          inquire(file='getdata.lammps',exist=chk_filex)

          if(chk_filex==.true.)then

          open(6001, file='getdata.lammps',&
          form='formatted', status='old', err=995, access='sequential')
         
          read(6001,*) dummy
          ! read total atoms
          read(6001,*) num, dummy
          ! write(*,'(i6,1x,a5)') num, dummy
          ! only read atom types
          read(6001,*) maxid, dummy
          backspace(6001)
          ! go back one line in read file

          ! allocate memory
          allocate(id(num))
          allocate(typ_at(num))
          allocate(pos_x(num),pos_y(num),pos_z(num))
          allocate(num_ele(maxid))

          ! read atom # in maxid types
          read(6001,*) &
            (dummy, j=1, 3), (num_ele(j), j=1, maxid)

          ! read dummy lattice cell
          do i=1, 6
          read(6001,'(a)')dummy
          ! write(*,*)trim(dummy)
          enddo

          ! read position data
          do i=1, num
          read(6001,'(1x,i7,1x,i2,3(1x,f11.6))') &
            id(i), typ_at(i), pos_x(i), pos_y(i), pos_z(i)
          enddo

          ! sort bubble
          lo=1
          hi=num-1

          do j=lo, hi
          l=0
          do i=lo, hi
          if(pos_z(i)>pos_z(i+1))then
            ! ---------- swap z
            tmp_real=pos_z(i)
            pos_z(i)=pos_z(i+1)
            pos_z(i+1)=tmp_real
            ! ---------- swap x
            tmp_real=pos_x(i)
            pos_x(i)=pos_x(i+1)
            pos_x(i+1)=tmp_real
            ! ---------- swap y
            tmp_real=pos_y(i)
            pos_y(i)=pos_y(i+1)
            pos_y(i+1)=tmp_real
            ! ---------- swap typ_at
            tmp_int=typ_at(i)
            typ_at(i)=typ_at(i+1)
            typ_at(i+1)=tmp_int
            ! ---------- swap id
            tmp_int=id(i)
            id(i)=id(i+1)
            id(i+1)=tmp_int
            ! ---------- 
            l=l+1
          else
          endif
          enddo
          if(l.eq.0)exit
          hi=hi-1
          enddo
          ! sort end

          ! rearrange position without H atoms
          k=0 ! k is H atoms
          j=1
          do i=1, num
          if(typ_at(i).eq.2)then
            j=j
            k=k+1
          else
            id(j)=id(i)
            typ_at(j)=typ_at(i)
            pos_x(j)=pos_x(i)
            pos_y(j)=pos_y(i)
            pos_z(j)=pos_z(i)
            j=j+1
          endif
          enddo
          ! rearrange completed ##########

          ! calculate wide narrow atoms, start from line 1
          sort_num=num-num_ele(2)

          !----------lines 
          j=1
          ! j # of a new C dimer line
          do i=1, sort_num-1
          if((pos_z(i+1)-pos_z(i)) .gt. 1.2)then
            ! if C distance > 1.2 angstrom, new dimer
            ! not smart just use 1.2
            j=j+1
            ! from z to separate vertical line
          endif
          enddo

          ! save j into total dimer lines
          dim_ln=j
          ! --------------------
          
          ! ---------- width calculate
          allocate(width(dim_ln))
          j=0
          k=1
          ! k index of width
          ! current line #
          do i=1, sort_num-1
          if((pos_z(i+1)-pos_z(i)) .gt. 1.2)then
            ! if C distance > 1.2 angstrom, new dimer
            ! subs_ln current line substract last line # of C atoms
            subs_ln=i-j
            j=i
            width(k)=subs_ln
            ! write(*,'(i0)') j
            ! from z to separate vertical line

            k=k+1

          endif
          enddo
          ! # of C dimer line
          
          ! toppest line has same value with toppest -1 line
          if(k.eq.dim_ln)then
            width(dim_ln)=subs_ln
          endif

          ! -------------------- choose line to sum ldos

          !####################
          ! honeycomb lines choose type 5
          ! confirm honeycomb structure properly
          ! not smart for other structure
          if(chotyp.eq.5)then
            dim_ln = dim_ln / 2
            do i = 1, dim_ln
              width(i) = 2 * width(2*i-1)
              ! only first double line is twice than dimer line
            enddo
          endif
          !####################

          write(char_nufilnam,'(i0)') id(1)
          ! write integer into character, i0 is w/o void space

          ! do pre-read first
          inquire&
            (file='PDOS_'//trim(char_nufilnam)//'.dat',exist=chk_filex)
          if(chk_filex==.false.)then
            write(*,*)&
              'File "PDOS_'//trim(char_nufilnam)//'.dat" not exist'
          elseif(chk_filex==.true.)then
            ! file exist continue silently
          open(5001,file='PDOS_'//trim(char_nufilnam)//'.dat',&
            form='formatted',status='old',err=996,access='sequential')
          l=0
          k=0
          do 
          read(5001,'(a)',iostat=r2e)dummy
          ! read to end
          if(r2e.ne.0)then
            goto 2051
          else
            fnd_co=(index(dummy,'#')/=0)
            ! index for finding comment line
            if(fnd_co)then
              l=l+1
              !comment line
            else
              k=k+1
              !data line
            endif
          endif
          enddo

2051    continue
            ln_co=l-1 ! read file one comment line at bottom
            ln_dat=k
            close(5001)
          endif
          ! pre-read over

          ! check all files exist or not
        j=0
        do i=1, sort_num

          write(char_nufilnam,'(i0)') id(i)
          ! write integer into char

          inquire&
            (file='PDOS_'//trim(char_nufilnam)//'.dat',exist=chk_filex)
          if(chk_filex==.false.)then
            write(*,*)&
              'File "PDOS_'//trim(char_nufilnam)//'.dat" not exist'
            j=j+1
          endif

        enddo

        if(j.ne.0)then
          write(*,*)&
            "Files incompleted... Bye!"
          goto 2052
        endif

          allocate(posx(ln_dat))
          allocate(posy(ln_dat))
          allocate(sum_ldos(ln_dat))

          ln_at_lo=1
          ln_at_hi=0

        do j=1, dim_ln

          do k=1,ln_dat
          sum_ldos(k)=0
          enddo

        ! atom # by line increase
        ln_at_hi=ln_at_hi + width(j)
        ! do loop per line with atoms
        do i=ln_at_lo, ln_at_hi
          write(char_nufilnam,'(i0)') id(i)
          inquire&
            (file='PDOS_'//trim(char_nufilnam)//'.dat',exist=chk_filex)
          if(chk_filex==.true.)then

          open(10000+i,file='PDOS_'//trim(char_nufilnam)//'.dat',&
            form='formatted',status='old',err=996,access='sequential')

          do l=1,ln_co
          read(10000+i,'(a)')dummy
          enddo
          do l=1,ln_dat
          ! ###### ------ this line might be wrong if previous files
          ! ###### ------ changed, needs to find a better way
          read(10000+i,'(1x,f11.6,3x,f11.6)')posx(l),posy(l) ! fixed read
          sum_ldos(l)=sum_ldos(l)+posy(l)
          enddo
          endif
        enddo

          write(char_outnam,'(i0)') j

          ! sum_ldos should be devided by atoms in line

          open(5000+j, &
            file='PDOS_ldos_ln_'//trim(char_outnam)//'.dat',& 
            form='formatted',&
            status='unknown',err=994,access='sequential')

          ! use if logic to print respectively
          if(chotyp.eq.4)then
          write(5000+j,'(a,/,a,/,a,/,a,i0,a,/,a)')&
            '#','# dimer line', '#', '# normalized by ',width(j), &
            ' atoms','#'
          elseif(chotyp.eq.5)then
          write(5000+j,'(a,/,a,/,a,/,a,i0,a,/,a)')&
            '#','# honeycomb line', '#', '# normalized by ',width(j), &
            ' atoms','#'
          endif
          ! comment 4 for dimer line, 5 for honeycomb

          do l=1,ln_dat
          write(5000+j,'(1x,f11.6,3x,f11.6)')posx(l), &
            (sum_ldos(l)/width(j))
          enddo

          write(*,*)'PDOS_ldos_ln_'//trim(char_outnam)//'.dat created'

        ! ### warning : can not open too many files
        ! ###         : maybe memory? must close when not use
        !##############################
        do i=ln_at_lo, ln_at_hi
        close(10000+i)
        enddo

        ln_at_lo=ln_at_hi + 1

        close(5000+j)
        !##############################

        enddo
        !########## 
        !########## 

          write(*,*)
          write(*,'((a),i0)') " Total lines : ", dim_ln
          write(*,*)
          write(*,*)'Files completed...'
          write(*,*)
          !------------------------------

        deallocate(posx)
        deallocate(posy)
        deallocate(sum_ldos)

2052    continue
          deallocate(width)

          ! ----------

          deallocate(id)
          deallocate(typ_at)
          deallocate(pos_x,pos_y,pos_z)
          deallocate(num_ele)
        
          else
            goto 995
          endif

        goto 999

105     continue

        deallocate(atspe)
106     continue
        deallocate(posx)
        deallocate(posy)
        deallocate(sum_ldos)
        goto 999

994     continue
        write(*,*)' '
        write(*,*)&
          'Error: Cannnot open file ',&
          'PDOS_ldos_ln_'//trim(char_outnam)//'.dat'
        write(*,*)' '
        goto 999

995     continue
        write(*,*)' '
        write(*,*)&
          'Error: Cannnot open file getdata.lammps'
        write(*,*)&
          '(Please copy the relevant getdata.lammps file here)'
        write(*,*)' '
        goto 999

996     continue
        write(*,*)' '
        write(*,*)&
          'Error: Cannnot open file PDOS_'//trim(char_nufilnam)//'.dat'
        write(*,*)' '
        goto 999

997     continue
        write(*,*)' '
        write(*,*)&
          'Error: Cannnot open file PDOS_sumldos.dat'
        write(*,*)' '
        goto 999

998     continue
        write(*,*)' '
        write(*,*)&
          'Error: Cannnot open file PDOS_'//trim(atspe(i))//'.dat'
        write(*,*)' '
        goto 999

999     stop

      endprogram sumldos
