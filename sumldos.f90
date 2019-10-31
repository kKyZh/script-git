      program sumldos
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
        ! atom species and atoms use same variables
        real(8), allocatable :: posx(:), posy(:) ! read x and y of ldos
        real(8), allocatable :: sum_ldos(:) ! sum value of y axis
        character(2) contin ! choose continue or not
        character(8) char_chotyp ! choose type character
        character(16), allocatable :: atspe(:) ! atom species
        character(32) filnam ! check file name
        character(32) char_nufilnam! check file name
        character(128) dummy ! read file
        logical chk_filex ! check file exist or not
        logical fnd_co ! found the # symbol

        write(*,*)
        write(*,'(1x,3a)')'Running... sum the value of LDOS, ',&
                'then output a summarized *.dat file ',&
                '(Version --1.0 beta//May/13/2019//)'
        write(*,*)'Please run getldos before this script'

101     continue
        write(*,*)
        write(*,*)'Please choose type'
        write(*,*)'(1) Atom species;'
        write(*,*)'(2) Atom number (Individual);'
        write(*,*)'(3) Atom number (Range);'
        write(*,*)'(Q) Quit;'
        write(*,'(1x,a)',advance='no')'==>   '
        read(*,*,iostat=chk_chotyp)char_chotyp
        if(chk_chotyp.ne.0)then
          goto 101
        else
        select case (char_chotyp)
        case('1')
          read(char_chotyp,*)chotyp
          write(*,*)
          write(*,*)'You choose (1) Atom species;'
          goto 102
        case('2')
          read(char_chotyp,*)chotyp
          write(*,*)
          write(*,*)'You choose (2) Atom number (Individual);'
          goto 103
        case('3')
          read(char_chotyp,*)chotyp
          write(*,*)
          write(*,*)'You choose (3) Atom number (Range);'
          goto 104
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
        write(*,'(1x,a)',advance='no')'==>   '
        read(*,*,iostat=chk_nuatspe)nuatspe
        if(chk_nuatspe.ne.0)then
          write(*,*)
          write(*,*)'Error : Need a number;'
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
        write(*,'(1x,a)',advance='no')'==>   '
        read(*,*,iostat=chk_atspe)atspe(i)
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
        write(*,*)'Are you sure to continue? (Y/N)'
        write(*,'(1x,a)',advance='no')'==>   '
        read(*,*,iostat=chk_contin)contin
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
        write(*,'(1x,a)',advance='no')'==>   '
        read(*,*,iostat=chk_nuatspe)nuatspe
        if(chk_nuatspe.ne.0)then
          write(*,*)
          write(*,*)'Error : Need a number;'
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
        write(*,'(1x,a)',advance='no')'==>   '
        read(*,*,iostat=chk_atspe)atspe(i)
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
        write(*,*)'Are you sure to continue? (Y/N)'
        write(*,'(1x,a)',advance='no')'==>   '
        read(*,*,iostat=chk_contin)contin
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
        write(*,'(1x,a)',advance='no')'==>   '
        read(*,*,iostat=chk_minatnu)minatnu
        if(chk_minatnu.ne.0)then
          write(*,*)
          write(*,*)'Error : Need an integer, try again;'
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
        write(*,'(1x,a)',advance='no')'==>   '
        read(*,*,iostat=chk_maxatnu)maxatnu
        if(chk_maxatnu.ne.0)then
          write(*,*)
          write(*,*)'Error : Need an integer, try again;'
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
        write(*,'(1x,a)',advance='no')'==>   '
        read(*,*,iostat=chk_intvl)intvl
        if(chk_intvl.ne.0)then
          write(*,*)
          write(*,*)'Error : Need an integer, try again;'
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
        write(*,*)'Are you sure to continue? (Y/N)'
        write(*,'(1x,a)',advance='no')'==>   '
        read(*,*,iostat=chk_contin)contin
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
105     continue

        deallocate(atspe)
106     continue
        deallocate(posx)
        deallocate(posy)
        deallocate(sum_ldos)
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
