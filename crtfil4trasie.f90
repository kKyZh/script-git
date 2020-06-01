        program creatfile4transiestaorsiesta

        use jsu_readline
        use m_manual

        implicit none
        integer i,j,k
        integer chk,chk_1!check input value
        integer nutot_at, nutot_sp,nu_tb!number of tab
        integer kp_x, kp_y, kp_z
        integer sum_elct !sum of electrons in wavefunckpoints
        integer nu_ho !number of homo level for wavefunction
        integer chs_2,chs_3,chs_4,chs_6,chs_6_1 !choose ,chs_4_1        
        integer chs_t, chk_t ! choose for transiesta and check
        integer chs_e, chk_e ! atoms as electrode and its check
        integer chk_chs_filex ! check for choose file exist
        integer lo,hi ! for low and high variables
        integer posre_nam_chk, posre_chs_chk ! renew a position.fdf name
        integer status, rename, system ! rename a file
        integer re_nutot_sp, re_nutot_sp_chk !correct the atom types
        integer, allocatable :: l(:) ! revise the problem of l
        integer, allocatable :: tmp_l(:) ! revise the problem of l
        integer, allocatable :: nu_wv(:)!number of wavefunction 
        integer, allocatable :: nutot_el(:)!number of total element
        integer, allocatable :: nu_lb(:)!number of lable of element
        integer, allocatable :: elct_el(:)!electrons of each element
        real, allocatable :: cell(:)!position
        character(1) chs_filex ! choose y n q for file exist
        character(3), allocatable :: i_nu_lb(:)!transfer from i to char
        character(8), allocatable :: lb_el(:)!label of element
        character(8), allocatable :: tmp_lb_el(:)!label of element
        character(16), allocatable :: rp_vl(:)!constant repeat value
        character(16), allocatable :: i_cell(:)!transfer from i to cahr
        character(16) i_nutot_at, i_nutot_sp
        character(16) i_i !transfer i from i to char
        character(32) filename,tab
        character(32) filename1,filename2
        character(32) posre_nam, posre_chs ! renew a position.fdf name
        character(32) chs_1,chs_5 !choose
        character(32) i_chs_3,i_chs_4,i_chs_6 !choose
        character(32) i_chs_6_1 !choose,i_chs_4_1
        character(32) slt_2 !choose
        character(64) dummy
        logical chk_filex
        !----------------------------------
        integer lchk ! length of check
        integer nna, narg, lna, larg ! no correct name & number of
        ! argument & length of name & length of argument
        character(32) na ! options variable: name
        character(64) :: chk_filename ! check name is *.car
        !----------------------------------
        character(128) string ! userreadline
        integer check ! check for strain
        integer chk_psf
        integer m
        integer strain_chs, strain_lo, strain_hi
        integer len_strain_nam, len_fra_strain_nam
        integer len_fra_percent, len_int_strain_nam
        integer num_fil
        real strain_in, strain_i, real_check
        real strain_nam
        character(32) strain_chr_in, strain_chr_nam
        integer vasp_out
        integer fix_species
        real, allocatable :: z_lattice(:)
        !----------------------------------

        write(*,*)
        write(*,'(1x,6a)')'Running... Creating *.fdf input file ',&
          'for SIESTA/TranSIESTA... ',&
          'for INCAR (new added)... ',&
          '(Prepare *** POTCAR & KPOINTS *** HERE before INCAR) ',&
          '(Prepare *** *.psf & getdata.lammps *** before strain) ',&
          '(Version --2.30 //May/6/2020//)'
        ! since KPOINTS and POTCAR always same and easy to create this
        ! time, just prepare them before the INCAR for strain.
        ! later mayber write a script for all necessary files creation

        write(*,*)
        write(*,*)'Include manual option: -h '
        write(*,*)'By using ==> car2Lammps -h '
        write(*,*)'To see how to use it...'
        write(*,*)'Include use shell command'
        write(*,*)
        write(*,*)&
        '(Include reading TranSIESTA input file after sorted)'
          ! maybe crtfil4siesta will not be used anymore
        write(*,*)' '
        write(*,*)'This time only parts of parameters can be changed'

        ! ###### help options ######

        nna=0
        i=1
        lna=2

        narg=command_argument_count()

        do i=1,narg
        call get_command_argument(i,na)
          larg=len_trim(na)
          if(na(1:1)=='-')then
            do lna=2,larg
            if(na(lna:lna)=='h')then
              write(*,*)
              call manual
              write(*,*)
              goto 997
            endif
            enddo
            nna=nna+1
          endif
        enddo

        if(narg>0.and.narg==nna)then
          write(*,*)
          write(*,*)'------'
          write(*,*)'No available options'
          write(*,*)'call manual: -h'
          write(*,*)
          goto 997
        endif

        ! ###### help options finished ######
        ! ###### check variable has .fdf ######

        if(narg>0)then
          nna=0
        do i=1,narg
        call get_command_argument(i,chk_filename)
        ! for creat a tss_*.fdf file filename1 is the tse_*.*

        lchk=len_trim(chk_filename)
        if(chk_filename(lchk-3:lchk)=='.fdf')then
          filename1=chk_filename
          goto 97
        else
          nna=nna+1
        endif
        enddo
          if(narg==nna)then
          write(*,*)
          write(*,*)'------'
          write(*,*)'No available .fdf selected'
          write(*,*)'call manual: -h'
          write(*,*)
          goto 997
          endif
        endif

        ! ###### check variable has .fdf finished ######
97      continue

        write(*,*)
        write(*,*)'Running...   Checking files for writing'
        open(10,file='getdata.lammps', form='formatted', status='old',& 
                err=999, access='sequential')
        open(20,file='position.fdf', form='formatted',err=998,& 
                status='old', access='sequential')
        write(*,*)'Running...   Checking completed'
        write(*,*)
        write(*,*)'----------------------------------------------------'
        write(*,*)'List of all *.fdf POTCAR files : '
        write(*,*)
        call execute_command_line('ls *.fdf')
        call execute_command_line('ls POTCAR')
        write(*,*)'----------------------------------------------------'
        write(*,*)'Current path : '
        write(*,*)
        call execute_command_line('pwd')
        write(*,*)'----------------------------------------------------'
!------------------------------ check the file existed or nor -----
3000    continue
        write(*,*)
        write(*,*)'Please name the input file (Less than 32 characters)'
        write(*,'(2a)') &
          ' (only *.fdf files with entered file name, below, ', &
          'will be checked for existance or not)'
        call userreadline( filename, &
          '(file name without .extensions.) : ')
        inquire(file=''//trim(filename)//'.fdf',exist=chk_filex)
        if(chk_filex)then
3003    continue
          write(*,*)
          write(*,*)'---------- Warning ----------'
          write(*,*)'File '//trim(filename)//'.fdf existed,',&
          'do you want to overwritten it?'
          call userreadline( string, '(y:yes, n:no, q:quit) : ')
          read( string, *, iostat=chk_chs_filex)chs_filex
          if(chk_chs_filex .ne. 0) then
            goto 3003
          else
            select case (chs_filex)
            case('y','Y')
              goto 3001
            case('n','N')
              goto 3000
            case('q','Q')
              write(*,*)
              write(*,*)'You quit... Bye!!'
              write(*,*)
              goto 3002
            case default
              goto 3003
            endselect
          endif
        endif

3001    continue
        write(*,*)
        write(*,*)'Running...   Writing .fdf file'

        tab=achar(9)

!----------------------------read part--------------------------
        !--------------- siesta or transiesta
2001    continue
        write(*,*)
        write(*,*)'------------------- Information -------------------'
        write(*,*)'Please choose .fdf file type or INCAR'
        !######################################## add INCAR
        write(*,*)'(1) SIESTA -> w/wo INCAR; (2) TranSIESTA; (0)Quit;'
        !######################################## add INCAR
        call userreadline( string, '(Integer) : ')
        read( string, *, iostat=chk_t) chs_t
        if(chk_t.ne.0)then
          goto 2001
        elseif(chs_t.eq.0)then
          write(*,*)
          write(*,'(a,i0,a)')'You choose (',chs_t,') Quit;'
          write(*,*)
          write(*,*)'Bye...'
          write(*,*)
          goto 2000
        elseif(chs_t.eq.1)then
          write(*,*)
          write(*,'(a,i0,a)')'You choose (',chs_t,')SIESTA'
2002    continue
          write(*,*)
          write(*,*) 'Do you need VASP INCAR file?'
          call userreadline( string, '([1] yes, [2] no) : ')
          read( string, *, iostat=chk_t) vasp_out
          if(check .ne.0) then
            goto 2002
          elseif( vasp_out .ne. 1 .and. vasp_out .ne. 2) then
            goto 2002
          elseif( vasp_out .eq. 1) then
            inquire(file='KPOINTS',exist=chk_filex)
            if(chk_filex .eq. .false.) then
              write(*,*)
              write(*,*) 'ERROR : Prepare KPOINTS here before INCAR'
              write(*,*)
              goto 997
            endif
            inquire(file='POTCAR',exist=chk_filex)
            if(chk_filex .eq. .false.) then
              write(*,*)
              write(*,*) 'ERROR : Prepare POTCAR here before INCAR'
              write(*,*)
              goto 997
            endif
          endif
        elseif(chs_t==2)then
          write(*,*)
          write(*,'(a,i0,a)')'You choose (',chs_t,')TranSIESTA'
          open(50,file=filename1, form='formatted', status='old',& 
                  err=990, access='sequential')
      ! this filename1 should be tse_*.fdf input file (copy it when
      ! creating tss_*.fdf after copy s_position and tse_position file)
      !##### another way is using inquire
          filename2=filename1(1:len_trim(filename1)-4)
          ! delete '.fdf'

        elseif(chs_t/=0.or.chs_t/=1.or.chs_t/=2)then
          goto 2001
        endif

        !######################################## applied strain
        ! lattice and filename and position in input file

        ! if strain, getdata.lammps saved parameter
        ! read first line of getdata.lammps
        read(10,*) &
          dummy, fix_species,&
          strain_chs, num_fil, strain_lo, strain_hi,&
          strain_in, len_strain_nam, len_fra_strain_nam,&
          len_fra_percent
        allocate(z_lattice(num_fil))
        read(10,*) nutot_at ! total atom
        do i = 1, 7 + nutot_at
        read(10, '(a)') dummy
        enddo
        if( fix_species .eq. 2) then
          read(10, *) dummy, (z_lattice(i), i = 1, num_fil)
        endif
        rewind(10)
        ! ############################## strain or not
91      continue
        write(*,*)
        write(*,'(4a)') 'Do you need strain?', &
          ' (siestaInput.fdf -> siestaInput_*.fdf)', &
          ' (* strain without %)', &
          ' (INCAR -> mkdir strain_* -> ./strain_*/INCAR)'
        write(*,'(2a)') &
          'Check position files later!!!', &
          ' Uniaxial strain along Z axis for I-V !!!'
        if( strain_chs .eq. 1) then
          string = '1'
        elseif( strain_chs .eq. 2) then
          string = '2'
        endif
        !call userreadline( string, '(1. yes, 2. no) : ')
        read( string, *, iostat=check) strain_chs
        if( check .ne. 0) then
          goto 91
        elseif( strain_chs .eq. 1) then
          write(*,*)
          write(*,*) '1. yes, with strain'
              write(*,*)
              call execute_command_line(&
                'ls *.psf >/dev/null 2>/dev/null', &
                exitstat = chk_psf)
            if(chk_psf .ne. 0) then
              write(*,*) 'ERROR : Prepare *.psf before strain'
              write(*,*)
              goto 997
            elseif(chk_psf .eq. 0) then
              write(*,*) '*.psf will be copied to ./starin_*/'
            endif
199     continue
          write(*,*)
          write(*,*) 'Lowest strain (with %)'
          !call userreadline( string, '(Integer) : ')
          !read( string, *, iostat=check) strain_lo
          check = 0
          if( check .ne. 0) goto 199
          if( strain_lo .lt. 0) goto 199
          !only positive value availiable this time
198     continue
          write(*,*)
          write(*,*) 'Highest strain (with %)'
          !call userreadline( string, '(Integer) : ')
          !read( string, *, iostat=check) strain_hi
          check = 0
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
          !call userreadline( string, '(Decimal or Integer) : ')
          !read( string, *, iostat=check) strain_in
          check = 0
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
          write(*,*) 
          write(*,*) 'Total files : ', num_fil
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

          write(*,*)
          if( len_fra_percent .eq. 0) then
          write(*,'(1x, i0, a)') &
            int(strain_i), '%'
          else
          write(*,'(a,1x, f<len_strain_nam>.<len_fra_percent>,a)') &
            'Strain : ',strain_i, '%'
          endif
          write(*,'(2a)') 'file added : ',&
            trim(adjustl(strain_chr_nam))
          status = system( &
            'mkdir strain_'&
            //trim(adjustl(strain_chr_nam))//&
              ' 2>/dev/null')

          ! just show new directory created or not
          if( status .ne. 0) then
            write(*,'(3a)') 'directory for vasp : ',&
              trim(adjustl(strain_chr_nam)),&
              ' not created, directory already exists?'
          endif

          !############################## open write files
          open(50000+i, &
            file=&
            './strain_'//trim(adjustl(strain_chr_nam))//&
            '/'//trim(filename)//&
            '_'&
            //trim(adjustl(strain_chr_nam))//'.fdf',&
            status='unknown', &
            err=95, form='formatted', access='sequential') 

        call execute_command_line(&
          'cp *.psf ./strain_'&
          //trim(adjustl(strain_chr_nam))//'/')
        call execute_command_line(&
          'cp getdata.lammps ./strain_'&
          //trim(adjustl(strain_chr_nam))//'/')

          if(vasp_out .eq. 1) then
            open(80000+i, &
              file=&
              './strain_'//trim(adjustl(strain_chr_nam))//'/INCAR',&
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
        strain_i = 0.0
        strain_lo = 0.0
        strain_hi = 0.0
        strain_in = 0.0
        num_fil = 1
        strain_nam = (100 + strain_i) / 100.0
        len_strain_nam = 4
        len_fra_strain_nam = 2
        write(strain_chr_nam, &
          '(f<len_strain_nam>.<len_fra_strain_nam>)') strain_nam

        open(50000+i,&
          file=&
          './strain_'//trim(adjustl(strain_chr_nam))//&
          '/'//trim(filename)//'_'&
          //trim(adjustl(strain_chr_nam))//'.fdf',&
          status='unknown', &
                err=95, form='formatted', access='sequential') 

          if(vasp_out .eq. 1) then
            open(80000+i, &
              file=&
              './strain_'//trim(adjustl(strain_chr_nam))//'/INCAR',&
              status='unknown', &
              err=94, form='formatted', access='sequential')
          endif

        call execute_command_line(&
          'cp *.psf ./strain_'&
          //trim(adjustl(strain_chr_nam))//'/')
        call execute_command_line(&
          'cp getdata.lammps ./strain_'&
          //trim(adjustl(strain_chr_nam))//'/')

        write(*,*)
        write(*,*) 'Total files : ', num_fil

        ! ### not strain or no strain
        else
          goto 91
        endif
        ! ############################## strain or not finished
        !################################### applied strain finished
        !------------------- choose finishied chs_t assigned ----------

        ! read for nutot_el
        read(10,'(a)')dummy
        read(10,*)dummy!total atom
        read(10,*)nutot_sp
        rewind(10)

        allocate(nutot_el(nutot_sp))!x y z
        allocate(cell(3))
        allocate(rp_vl(3))
        allocate(i_cell(3))
        allocate(nu_wv(10))!10 waves will be calculated
        !read file 10 and 20       
        read(10,'(a)')dummy
        read(10,*)nutot_at!total atom
        read(10,*)nutot_sp,dummy,dummy,(nutot_el(i),i=1,nutot_sp)!total element species
        do j=1,3
        read(10,*)dummy,cell(j)
        !write(*,*)cell(j),dummy
        write(i_cell(j),*)cell(j)
        enddo
        rewind(10)

        allocate(lb_el(nutot_sp))!read from #20 file
        allocate(l(nutot_sp))
        allocate(tmp_lb_el(nutot_sp))!read from #20 file
        allocate(tmp_l(nutot_sp))
        allocate(nu_lb(nutot_sp))
        allocate(i_nu_lb(nutot_sp))
        allocate(elct_el(nutot_sp))
        
        write(i_nutot_at,*)nutot_at!transfer i to char
        write(i_nutot_sp,*)nutot_sp

!----------------------------read position new -----------------------
        !-------read from position.fdf sorted z axis for
        !transiesiesta but not the sorted element column
        lo=1
        hi=0
        read(20,'(a)')dummy
        do i=1,nutot_sp
        hi=hi+nutot_el(i)
        do j=lo,hi
        read(20,'(a36,1x,i2,(a))')dummy,l(i),lb_el(i)
        enddo
        lo=lo+nutot_el(i)
        enddo
        !-----------------------------------------------------------
        !#######################################################
        !---------------- renewed this part

        if(chs_t==2)then
6000    continue
          write(*,*)
          write(*,*)'Please correct the atom types for tss_*.fdf'
          call userreadline( string, '(Integer) : ')
          read( string , *, iostat=re_nutot_sp_chk) re_nutot_sp
          if(re_nutot_sp_chk/=0)then
            goto 6000
          else
            if(re_nutot_sp>nutot_sp.or.re_nutot_sp<1)then
              goto 6000
            else
              do i=1,re_nutot_sp
              loop2: do j=1,nutot_sp
              if(l(j)==i)exit loop2
              enddo loop2
                tmp_l(i)=l(j)
                tmp_lb_el(i)=lb_el(j)
              enddo
          endif
          endif

        write(*,*) '*********************************************'
        do i=1,re_nutot_sp
1001    continue
        write(*,*)
        write(*,'(1x,3a)')'Please input the element number of ',&
          trim(tmp_lb_el(i)), ' (which on chemical table)'
        call userreadline( string, '(Integer) : ')
        read( string, *, iostat=chk) nu_lb(i)
        if(chk/=0)then
        goto 1001
        else
        write(i_nu_lb(i),'(i3)')nu_lb(i)!transfer label number to char
        endif
        enddo
        else

        write(*,*) '*********************************************'
        do i=1,nutot_sp
1011    continue
        write(*,*)
        write(*,'(1x,3a)')'Please input the element number of ',& 
          trim(adjustl(lb_el(i))), ' (which in chemical table)'
        call userreadline( string, '(Integer) : ')
        read( string, *, iostat=chk) nu_lb(i)
        if(chk/=0)then
        goto 1011
        else
        write(i_nu_lb(i),'(i3)')nu_lb(i)!transfer label number to char
        endif
        enddo
        !---------------------------------------------------------
        endif
        !-----------------renewed part of renew a position.fdf name
500     continue   
        write(*,*)
        write(*,*)'###### Information for selection ######'
        write(*,*)'Do you want to rename the position.fdf file?'
        call userreadline( string, '(y: yes, n: no) : ')
        read( string, *, iostat=posre_chs_chk) posre_chs
        if(posre_chs_chk/=0)then
          goto 500
        else
          selectcase(posre_chs)
          case('y','Y')
510     continue
          write(*,*)
          write(*,*)'Please choose the name of new position.fdf'
          write(*,*)'(1) s_position.fdf ==> For SIESTA (Device)'
          write(*,*)&
            '(2) tse_position.fdf ==> For TranSIESTA (Electrode)'
          write(*,*)&
            '(3) tss_position.fdf ==> For TranSIESTA (Scattering)'
          call userreadline( string, '(Integer) : ')
          read( string, *, iostat=posre_nam_chk) posre_nam
            if(posre_nam_chk/=0)then
              goto 510
            else
            selectcase(posre_nam)
            case('1')
              write(*,*)
              write(*,*)'You choose (1) s_position.fdf'
              ! rename the position.fdf
              ! rename the getdata.lammps
              ! prepare for next step about tss_getdata.lammps and
              ! tss_position.fdf
          goto 501
            case('2')
              write(*,*)
              write(*,*)'You choose (2) tse_position.fdf'
          goto 501
            case('3')
              write(*,*)
              write(*,*)'You choose (3) tss_position.fdf'
          goto 501
            case default
          goto 510
            endselect
            endif
        case('n','N')
          goto 501
        case default
          goto 500
        endselect
        endif
501     continue

!--------------------- k-points for sampling ---------------
        write(*,*)
        write(*,'(1x,a)')'------- Input k-points for sampling --------'
1002    continue
        write(*,*)
        write(*,'(1x,a)')&
          'Please input how many k-points on x direction'
        call userreadline( string, '(Integer for x) : ')
        read( string, *, iostat=chk) kp_x
        if(chk/=0)then
        goto 1002
        endif

1003    continue
        write(*,*)
        write(*,'(1x,a)')&
          'Please input how many k-points on y direction'
        call userreadline( string, '(Integer for y) : ')
        read( string, *, iostat=chk) kp_y
        if(chk/=0)then
        goto 1003
        endif

1004    continue
        write(*,*)
        write(*,'(1x,a)')&
          'Please input how many k-points on z direction'
        call userreadline( string, '(Integer for z) : ')
        read( string, *, iostat=chk) kp_z
        if(chk/=0)then
        goto 1004
        endif

!---------------------- Output of Selected Wavefunctions --------------------
        !###############################################################
        sum_elct=0
        do i=1,nutot_sp
1005    continue
        write(*,*)
        write(*,*)'---- Output of Selected Wavefunctions -----'
        write(*,*)&
        'How many electrons will be calcualted in this section and',&
          ' where is HOMO'
        write(*,*)&
        '(Ex. Carbon has 2s and 2p, total in 4, to be calculated)'
        write(*,'(1x,3a)')'Please input the charge of ',trim(lb_el(i)),&
          ' atom '
        call userreadline( string, '(Integer) : ')
        read( string, *, iostat=chk) elct_el(i)
        if(chk/=0) goto 1005
        if( elct_el(i) .le. 0) goto 1005
        sum_elct=sum_elct+elct_el(i)*nutot_el(i)
        enddo

        nu_ho=sum_elct/2
        do i=1,10
        nu_wv(i)=nu_ho-5
        enddo
        do i=1,10
        nu_wv(i)=nu_wv(i)+i!start from ho -2
        enddo

        !---------------------------------------------------------
!----------------------------output part---------------------
!----------------------------initial definitions---------------------
        strain_i = strain_lo
        do m = 1, num_fil
        strain_nam = (100 + strain_i) / 100.0
        write(strain_chr_nam, &
          '(f<len_strain_nam>.<len_fra_strain_nam>)') strain_nam

        ! ====================================== INCAR write
        if(vasp_out .eq. 1) then
        write(80000+m, '(3a)') &
          'System', ' = ',&
          ''//trim(adjustl(filename))//'_'&
          //trim(adjustl(strain_chr_nam))//''
        write(80000+m, '(3a)') &
          'PREC', ' = ',&
          'Low'
        write(80000+m, '(3a)') &
          'LREAL', ' = ',&
          'Auto'
        write(80000+m, '(3a)') &
          'EDIFF', ' = ',&
          '0.1'
        write(80000+m, '(3a)') &
          'EDIFFG', ' = ',&
          '-0.1'
        write(80000+m, '(3a)') &
          'ISTART', ' = ',&
          '1'
        write(80000+m, '(3a)') &
          'ICHARG', ' = ',&
          '1'
        write(80000+m, '(3a)') &
          'ISYM', ' = ',&
          '2'
        write(80000+m, '(3a)') &
          'LWAVE', ' = ',&
          '.TRUE.'
        write(80000+m, '(3a)') &
          'LCHARG', ' = ',&
          '.TRUE.'
        write(80000+m, '(3a)') &
          'ISMEAR', ' = ',&
          '1'
        write(80000+m, '(3a)') &
          'NSIM', ' = ',&
          '4'
        write(80000+m, '(3a)') &
          'ENCUT', ' = ',&
          '350'
        write(80000+m, '(3a)') &
          'IBRION', ' = ',&
          '2'
        write(80000+m, '(3a)') &
          'ISIF', ' = ',&
          '4'
        write(80000+m, '(3a)') &
          'NSW', ' = ',&
          '25'
        write(80000+m, '(3a)') &
          'NELM', ' = ',&
          '120'
        write(80000+m, '(3a)') &
          'ISPIN', ' = ',&
          '2'
        write(80000+m, '(3a)') &
          'LORBIT', ' = ',&
          '11'
        write(80000+m, '(3a)') &
          'IALGO', ' = ',&
          '48'
        ! ====================================== copy KPOINTS
        call execute_command_line(&
          ! file exists no need yes | before copy tested once
          'cp KPOINTS ./strain_'&
          //trim(adjustl(strain_chr_nam))//&
          '/KPOINTS')
        ! ====================================== copy KPOINTS finihsed
        ! ====================================== copy POTCAR
        call execute_command_line(&
        !status = system(&
          'cp POTCAR ./strain_'&
          //trim(adjustl(strain_chr_nam))//&
          '/POTCAR')
        endif
        ! ====================================== copy POTCAR finihsed
        ! ====================================== INCAR write finished

        write(50000+m,'(a)')&
          '############# Initial definitions ###########'
        write(50000+m,'(6a)')'SystemLabel',&
          (trim(tab),nu_tb=1,2),&
          trim(adjustl(filename)), '_',&
          trim(adjustl(strain_chr_nam))
        write(50000+m,'(4a)')'NumberOfAtoms',(trim(tab),nu_tb=1,2),&
          trim(adjustl(i_nutot_at))
        !----------------renewed part
        if(chs_t==2)then
        write(50000+m,'(3a,i0)')&
          'NumberOfSpecies',(trim(tab),nu_tb=1,2),&
          (re_nutot_sp)

        else
        write(50000+m,'(4a)')'NumberOfSpecies',(trim(tab),nu_tb=1,2),&
          trim(adjustl(i_nutot_sp))
        !write(*,*)(nutot_el(i),i=1,3)
        endif
        write(50000+m,*)
        !#######################################################
        !---------------- renewed this part

        if(chs_t==2)then
          write(50000+m,'(a)')'%block ChemicalSpeciesLabel'
          do i=1,re_nutot_sp
          loop1: do j=1,nutot_sp
          if(tmp_l(j)==i)exit loop1
          enddo loop1
            write(50000+m,'(a,i0,a,i0,2a)')trim(tab),tmp_l(j),&
              trim(tab),nu_lb(j),&
              trim(tab),trim(adjustl(tmp_lb_el(j)))
          enddo
          write(50000+m,'(a)')'%endblock ChemicalSpeciesLabel'
          write(50000+m,*)
        else
          write(50000+m,'(a)')'%block ChemicalSpeciesLabel'
          do i=1, nutot_sp
          write(50000+m,'(a,i0,a,i0,2a)')trim(tab),l(i),&
            trim(tab),nu_lb(i),&
            trim(tab),trim(adjustl(lb_el(i)))
          enddo
          write(50000+m,'(a)')'%endblock ChemicalSpeciesLabel'
          write(50000+m,*)
        endif

!----------------------------pseudo-atomic oritals ---------------------
        write(50000+m,'(a)')&
          "####### Pseudo-Atomic Orbitals (PAO's)#######"
        write(50000+m,'(4a)')'PAO.BasisSize',&
          (trim(tab),nu_tb=1,2),'DZP'
        write(50000+m,'(4a)')'NetCharge',&
          (trim(tab),nu_tb=1,2),'0.000000'
        write(50000+m,*)
!----------------------------coordinates ---------------------
        write(50000+m,'(a)')"################ Coordinates #######"
        write(50000+m,'(3a)')'AtomicCoordinatesFormat',trim(tab),'Ang'
        !-----------------renewed part of renew a position.fdf name
          ! #################### rename write files
          selectcase(posre_chs)
          case('y','Y')
            selectcase(posre_nam)
            case('1')
              status=rename(&
              'position_'//trim(adjustl(strain_chr_nam))//'.fdf',&
              's_position_'//trim(adjustl(strain_chr_nam))//'.fdf')
              status=rename('getdata.lammps','s_getdata.lammps')
              ! rename the position.fdf
              ! rename the getdata.lammps
              ! prepare for next step about tss_getdata.lammps and
              ! tss_position.fdf
              write(50000+m,'(3a)')&
              'AtomicCoordinatesAndAtomicSpecies <'&
              ,trim(tab),&
              's_position_'//trim(adjustl(strain_chr_nam))//'.fdf'
            case('2')
            status=rename(&
            'position_'//trim(adjustl(strain_chr_nam))//'.fdf',&
            'tse_position_'//trim(adjustl(strain_chr_nam))//'.fdf')
            status=rename('getdata.lammps','tse_getdata.lammps')
            write(50000+m,'(3a)')&
            'AtomicCoordinatesAndAtomicSpecies <'&
            ,trim(tab),&
            'tse_position_'//trim(adjustl(strain_chr_nam))//'.fdf'
            case('3')
            status=rename(&
            'position_'//trim(adjustl(strain_chr_nam))//'.fdf',&
            'tss_position_'//trim(adjustl(strain_chr_nam))//'.fdf')
            status=rename('getdata.lammps','tss_getdata.lammps')
            write(50000+m,'(3a)')&
            'AtomicCoordinatesAndAtomicSpecies <',&
            trim(tab),&
            'tss_position_'//trim(adjustl(strain_chr_nam))//'.fdf'
            case default
            endselect
          case('n','N')
            if( strain_chs .eq. 1) then
          write(50000+m,'(3a)')'AtomicCoordinatesAndAtomicSpecies <',&
            trim(tab),&
            'position_'//trim(adjustl(strain_chr_nam))//'.fdf'
            elseif( strain_chs .eq. 2) then
          write(50000+m,'(3a)')'AtomicCoordinatesAndAtomicSpecies <',&
            trim(tab),'position.fdf'
            endif
          case default
          endselect
          ! #################### rename finished
        write(50000+m,*)
!---------------------structural (lattice) information ---------------
        write(50000+m,'(a)')&
          "##### Structural (lattace) information #####"
        write(50000+m,'(5a)')&
          'LatticeConstant',trim(tab),'1.0',trim(tab),'Ang'
        write(50000+m,'(a)')'%block LatticeParameters'
        do i=1,3
        rp_vl(i)='90.000000'
        enddo
        ! #################### uniaxial strain along Z axis for I-V
        !((trim(tab),(trim(adjustl(i_cell(i))))),i=1,3),& <- stupid!!!
        if( fix_species .eq. 2) then
        write(50000+m,'(3(1x, f11.6), 3(1x, a))')&
          cell(1), cell(2), z_lattice(m), &
          (trim(adjustl(rp_vl(i))),i=1,3)
        else
        write(50000+m,'(3(1x, f11.6), 3(1x, a))')&
          cell(1), cell(2), cell(3)*strain_nam, &
          (trim(adjustl(rp_vl(i))),i=1,3)
        endif
        ! #################### uniaxial strain along Z axis for I-V
        write(50000+m,'(a)')'%endblock LatticeParameters'
        write(50000+m,*)

        write(50000+m,'(a)')"######## k-points for sampling #########"
        write(50000+m,*)
        write(50000+m,'(a)')'%block kgrid_Monkhorst_Pack'
        write(50000+m,'(1x,i3,1x,a11)')kp_x,'  0   0 0.5'
        write(50000+m,'(1x,a3,1x,i3,1x,a7)')'  0',kp_y,'  0 0.5'
        write(50000+m,'(1x,a8,i3,1x,a3)')'  0   0 ',kp_z,'0.5'
        write(50000+m,'(a)')'%endblock kgrid_Monkhorst_Pack'
        write(50000+m,*)

        !########################## supercell
        if(chs_t==2)then
          write(50000+m,*)'###### one supercell for tss_*.fdf ######'
          write(50000+m,'(a)')'%block supercell'
          write(50000+m,'(a)')'1 0 0'
          write(50000+m,'(a)')'0 1 0'
          write(50000+m,'(a)')'0 0 1'
          write(50000+m,'(a)')'%endblock supercell'
          write(50000+m,*)
        endif

!--------------------- Exchange Correlation (XC) functionals ---------------
        write(50000+m,'(a)')&
          "### Exchange Correlation (XC) functionals ###"
        write(50000+m,'(3a)')'XC.functional',trim(tab),'GGA'
        write(50000+m,'(3a)')"XC.authors",trim(tab),'PBE'
        write(50000+m,'(3a)')"SpinPolarized",trim(tab),'F'

        if(nutot_at>=320)then
          write(50000+m,'(3a)')"MeshCutoff",trim(tab),'350.0 Ry'
        else
          write(50000+m,'(3a)')"MeshCutoff",trim(tab),'300.0 Ry'
        endif

        write(50000+m,'(3a)')"MaxSCFIterations",trim(tab),'250'
        write(50000+m,*)' '

!--------------------- Density Matrix (DM) ---------------
        write(50000+m,'(a)')&
          "########## Density Matrix (DM) ##########"
        write(50000+m,'(3a)')'DM.MixingWeight',trim(tab),'0.02'
        !############################## 0.1 can not converged some times
        write(50000+m,'(3a)')'DM.Tolerance',trim(tab),'0.001'
        ! DM.tolerance no unit
        ! 0.00001 may be the default value
        write(50000+m,'(3a)')'DM.NumberPulay',trim(tab),'3'
        !##### 2 is converged 4 or others are not checked
        write(50000+m,'(3a)')&
          'DM.RequireEnergyConvergence',trim(tab),'T'
        ! maybe similar with dm.tolerance
        write(50000+m,'(3a)')'DM.EnergyTolerance',trim(tab),'0.001 eV'
        ! dm energytolerance has unit
        ! 0.00001 may be the default value
        write(50000+m,*)' '

!-------------------- Solution Method For Eigenvalues ---------------
        write(50000+m,'(a)')&
          "##### Solution Method for Eigenvalues #####"
        if(chs_t==1)then
        write(50000+m,'(3a)')'SolutionMethod',trim(tab),'diagon'
        elseif(chs_t==2)then
        write(50000+m,'(3a)')'SolutionMethod',trim(tab),'transiesta'
        ! transiesta second step is transiesta
        ! give a choice for siesta or transiesta
        endif
        ! ------------------------------------ select Transiesta or siesta
        write(50000+m,'(3a)')'Diag.DivideAndConquer',trim(tab),'T'
        write(50000+m,'(3a)')'Diag.ParallelOverK',trim(tab),'F'
        ! T is not suitable for CCMS but sicvm maybe ok
        write(50000+m,*)

!-------------------- Occupation of Electronic States ---------------
        write(50000+m,'(a)')&
          "##### Occupation of Electronic States #####"
        if(nutot_at>=320)then
          write(50000+m,'(3a)')&
            'ElectronicTemperature',trim(tab),'450.0 K'
        else
          write(50000+m,'(3a)')&
            'ElectronicTemperature',trim(tab),'300.0 K'
        endif
        write(50000+m,*)

!-------------------- Molecular Dynamics (MD) ---------------
        write(50000+m,'(a)')"####### Molecular Dynamics (MD) ########"
        write(50000+m,'(3a)')'MD.TypeOfRun',trim(tab),'CG'
        ! coordiante optimazation CG
        !######## renewed 
        if(chs_t==1)then
          if(nutot_at>=500)then
            write(50000+m,'(3a)')'MD.NumCGsteps',trim(tab),'0'
          elseif(nutot_at>=320.and.nutot_at<500)then
            write(50000+m,'(3a)')'MD.NumCGsteps',trim(tab),'0'
          elseif(nutot_at>0.and.nutot_at<320)then
            write(50000+m,'(3a)')'MD.NumCGsteps',trim(tab),'200'
          endif
          ! try using gpu to relax structure
          !######## 
        elseif(chs_t==2)then
        write(50000+m,'(3a)')'MD.NumCGsteps',trim(tab),'000'
        endif

        write(50000+m,'(3a)')'MD.VariableCell',trim(tab),'T'
        write(50000+m,'(3a)')'MD.ConstantVolume',trim(tab),'F'
        write(50000+m,'(3a)')'MD.MaxForceTol',trim(tab),'0.04 eV/Ang'
        ! 0.04 may be the default value 0.03 or 0.02 maybe enough for
        ! full relaxation
        write(50000+m,'(3a)')'MD.MaxCGDispl',trim(tab),'0.2 Bohr'
        ! 0.02 for displacement every CG less is more reliable but take
        ! longer time
        write(50000+m,*)

!---------------------- BandLinesScale --------------------
        write(50000+m,'(a)')"####### BandLinesScale ########"
        write(50000+m,'(3a)')'BandLinesScale',trim(tab),&
                        'ReciprocalLatticeVectors'
        write(50000+m,*)
        write(50000+m,'(a)')'%block BandLines'
        write(50000+m,'(9a)')'1',trim(tab),'0.000000',trim(tab),&
                '0.000000',trim(tab),'0.000000',trim(tab),&
                '\Gamma'!from gamma
        write(50000+m,'(8a)')'4',trim(tab),'0.000000',trim(tab),&
                '0.000000',trim(tab),'0.500000',trim(tab)
                !to 20 can be changed
                ! 5 maybe enought for drawing
        write(50000+m,'(a)')'%endblock BandLines'
        write(50000+m,*)

        write(50000+m,'(a)')&
                "##### Output of Selected Wavefunctions ######"
        write(50000+m,'(3a)')'WaveFuncKPointsScale',trim(tab),'pi/a'
                                !unit of k-points
        write(50000+m,'(a)')'%block WaveFuncKPoints'
        write(50000+m,'(3(1x,a),10(1x,i5))')'0.000','0.000'&
          ,'0.000',&
         ((nu_wv(i)),i=1,10)
        write(50000+m,'(a)')'%endblock WaveFuncKPoints'
        write(50000+m,*)
        !----.band.wfsx. full.wfsx energy band index selection -------
        write(50000+m,'(a)')&
          "####### fullbz.wfsx energy selection ########"
        write(50000+m,'(a)')'WFS.WriteForBands T'
        write(50000+m,'(a)')'WFS.EnergyMin     -7.00 eV'
        write(50000+m,'(a)')'WFS.EnergyMax     0.00  eV'
        write(50000+m,*)

        write(50000+m,'(a)')&
          "##### fullbz.wfsx .band.wfsx. band index selection ########"
        write(50000+m,'(2a,i0)')'WFS.BandMin',trim(tab),nu_wv(1)
        write(50000+m,'(2a,i0)')'WFS.BandMax',trim(tab),nu_wv(10)
        write(50000+m,*)

!---------------------- Density of States (DOS) --------------------
        write(50000+m,'(a)')"####### Density of States (DOS) ########"
        write(50000+m,'(a)')'%block ProjectedDensityOfStates'
        write(50000+m,'(9a)')'-7.00',trim(tab),'0.00',trim(tab),&
                '0.02',trim(tab),'3000',trim(tab),'eV'
                !settle from -7 to 0 and 0.01 resolution
                !3000 points between 0.02?
        write(50000+m,'(a)')'%endblock ProjectedDensityOfStates'
        write(50000+m,*)
        
!------------- Local Density of States (DOS) -----------------
        write(50000+m,'(a)')&
          "####### Local Density of States (LDOS) ########"
        write(50000+m,'(a)')'%block LocalDensityOfStates'
        write(50000+m,'(5a)')'-7.00',trim(tab),'0.00',trim(tab),'eV'
                !local density of states from -7 to 0
        write(50000+m,'(a)')'%endblock LocalDensityOfStates'
        write(50000+m,*)

!------------- Output options -----------------
        write(50000+m,'(a)')"####### Output options ########"
        write(50000+m,'(3a)')'WriteCoorStep',trim(tab),'T'
        write(50000+m,'(3a)')'WriteForces',trim(tab),'T'
        write(50000+m,'(3a)')'WriteKpoints',trim(tab),'T'
        write(50000+m,'(3a)')'WriteCoorXmol',trim(tab),'T'
        write(50000+m,'(3a)')'WriteCoorInitial',trim(tab),'T'
        write(50000+m,'(3a)')'WriteEigenvalues',trim(tab),'T'
        write(50000+m,'(3a)')'WriteDM',trim(tab),'T'
        write(50000+m,'(3a)')'WriteBands',trim(tab),'T'
        write(50000+m,'(3a)')'WriteKbands',trim(tab),'T'
        write(50000+m,'(3a)')'WriteWaveFunctions',trim(tab),'T'
        write(50000+m,'(3a)')'WriteCoorCerius',trim(tab),'T'
        write(50000+m,'(3a)')'WriteMDXmol',trim(tab),'T'
        write(50000+m,'(3a)')'WriteMDhistory',trim(tab),'T'
        write(50000+m,'(3a)')'WriteDenchar',trim(tab),'T'
        write(50000+m,'(3a)')'COOP.Write',trim(tab),'T'
        write(50000+m,'(3a)')'WriteMullikenPop',trim(tab),'1'
        write(50000+m,*)

!------------- Save options -----------------
        write(50000+m,'(a)')"####### Save options ########"
        write(50000+m,'(3a)')'DM.UseSaveDM',trim(tab),'T'
        write(50000+m,'(3a)')'MD.UseSaveCG',trim(tab),'T'
        write(50000+m,'(3a)')'MD.UseSaveXV',trim(tab),'T'
                        ! .XV and .xyz different?
        write(50000+m,*)

!------------------------------ for transiesta -------------------
      ! transiesta use the same with siesta first and then add
      ! transiesta parameters for electrodes and chenge the diagon to
      ! transiesta (solution method)
        if(chs_t==2)then ! selected transiesta file created 
        write(50000+m,'(a)')"##### Transiesta/tbtrans information #####"
        write(50000+m,'(a)')"##### GF Option #####"
        write(50000+m,'(3a)')&
          'TS.ComplexContour.Emin',trim(tab),'-30.0 eV'
        write(50000+m,'(3a)')'TS.ComplexContour.NPoles',trim(tab),'03'
        write(50000+m,'(3a)')&
          'TS.ComplexContour.NCircle',trim(tab),'30'
        write(50000+m,'(3a)')'TS.ComplexContour.NLine',trim(tab),'10'
        write(50000+m,*)

        write(50000+m,'(a)')"##### Bias Contour Options #####"
        write(50000+m,'(3a)')'TS.BiasContour.NumPoints',trim(tab),'10'
        write(50000+m,'(3a)')&
          'TS.BiasContour.Eta',trim(tab),'0.000001 Ry'
        write(50000+m,*)

        write(50000+m,'(a)')"##### TS Voltage #####"
        write(50000+m,'(3a)')'TS.Voltage',trim(tab),'0.000000 eV'
        ! maybe give the choose for change the parametter
        write(50000+m,*)

        write(50000+m,'(a)')"##### TBT Options #####"
        write(50000+m,'(3a)')'TS.TBT.HSFile',trim(tab),&
          ''//trim(filename)//'.TSHS'
        ! tbtrans options for plot a graph
        write(50000+m,'(3a)')'TS.TBT.Emin',trim(tab),'-5.0 eV'
        write(50000+m,'(3a)')'TS.TBT.Emax',trim(tab),'+5.0 eV'
        write(50000+m,'(3a)')'TS.TBT.NPoints',trim(tab),'100'
        write(50000+m,'(3a)')'TS.TBT.NEigen',trim(tab),'3'
        write(50000+m,'(3a)')'###TS.TBT.PDOSFrom',trim(tab),'-7'
        write(50000+m,'(3a)')'###TS.TBT.PDOSto',trim(tab),'0'
        ! PDOSfrom and PDOSto need integers
        write(50000+m,'(3a)')'TS.TBT.CalcIeig',trim(tab),'T'
        write(50000+m,*)

        write(50000+m,'(a)')"##### write hamiltonian #####"
        write(50000+m,'(3a)')'TS.SaveHS',trim(tab),'T'
        write(50000+m,*)

!------------------------------ electrode atoms selection --------------
        !write(*,*)&
        !  'Please choose how many atoms as electrode for .TSHS'
        ! keep this part maybe
!2100    continue
!        read(5,*,iostat=chk_e)chs_e
!        if(chk_e/=0)then
!          write(*,*)''
!          write(*,*)&
!          'Please enter the correct integer for electrode atoms'
!          goto 2100
!        else
!        endif

        !rewed version needs to select the tse_* file at $1 variable
        ! keep the enter selection for atoms

!------------------------------------------------------------------------

        !####### read tse_*.fdf file for number of atoms
        do i=1,2
        read(50,'(a)')dummy
        enddo

        ! a15 ==> read dummy "a" is for all length of dummy
        !     ==> read dummy "tab" space is a1
        !     ==> a2 is two tab spaces
        read(50,'(a15,i)')dummy,chs_e
        !####### read tse_*.fdf file for number of atoms

        write(50000+m,'(a)')"##### left electrode #####"
        write(50000+m,'(3a)')'TS.HSFileLeft',trim(tab),&
                        ''//trim(filename2)//'.TSHS'
        write(50000+m,'(2a,i0)')'TS.NumUsedAtomsLeft',trim(tab),chs_e
        write(50000+m,'(3a)')'TS.BufferAtomsLeft',trim(tab),'0'
        write(50000+m,*)

        write(50000+m,'(a)')"##### right electrode #####"
        write(50000+m,'(3a)')'TS.HSFileRight',trim(tab),&
                        ''//trim(filename2)//'.TSHS'
        write(50000+m,'(2a,i0)')'TS.NumUsedAtomsRight',trim(tab),chs_e
        write(50000+m,'(3a)')'TS.BufferAtomsRight',trim(tab),'0'
        write(50000+m,*)
        
        ! need to be add many options for creating.
        endif

        strain_i = strain_i + strain_in
        rewind(50000+m)
        close(50000+m)
        rewind(80000+m)
        close(80000+m)
        enddo
        ! enddo strain write close file 50000+m

        close(50)
        !###################################################
!------------- finished ----------------
        if(chs_t==1)then
        write(*,*)
        write(*,*)'-------- For SIESTA --------'
        if( strain_chs .eq. 1) then
        call execute_command_line(&
          'cp strain_'//trim(adjustl(strain_chr_nam))//&
          '/'//trim(filename)//'_'&
          //trim(adjustl(strain_chr_nam))//&
          '.fdf '//trim(filename)//'.fdf')
        if( vasp_out .eq. 1) then
        call execute_command_line(&
          'cp ./strain_'//trim(adjustl(strain_chr_nam))//&
          '/INCAR INCAR') 
        endif
        elseif( strain_chs .eq. 2) then
        call execute_command_line(&
          'cp ./strain_'//trim(adjustl(strain_chr_nam))//&
          '/'//trim(filename)//'_'&
          //trim(adjustl(strain_chr_nam))//&
          '.fdf '//trim(filename)//'.fdf')
        if( vasp_out .eq. 1) then
        call execute_command_line(&
          'cp ./strain_'//trim(adjustl(strain_chr_nam))//&
          '/INCAR INCAR') 
        endif
        endif
        write(*,*)
        elseif(chs_t==2)then
        write(*,*)
        write(*,*)'-------- For TranSIESTA ---------'
        write(*,*)
        endif
        write(*,'(a)')'------ Details of input file--------'
        write(*,*)
        write(*,'(a)')'------- WaveFuncKPoints ------'
        write(*,*)
        write(*,'(1x,a,i5)')'Total electrons = ',sum_elct
                        !total electrons less than 100k
        write(*,'(1x,a,i5)')'HOMO is at ',nu_ho
                        !total electrons less than 100k
        write(*,'(3(1x,a),10(1x,i5))')'0.000','0.000'&
            ,'0.000',&
           ((nu_wv(i)),i=1,10)
        write(*,*)
        
        if(chs_t==2)then
        write(*,'(a)')'------- TranSIESTA ------'
        write(*,*)
        write(*,'(2a,i0)')&
          'Atoms as electrode for transiesta',trim(tab),chs_e
        write(*,*)
        else
        endif

        write(*,'(a)')'------ End of Details --------'

        write(*,*)
        write(*,*)'Running...   Writing completed'
        write(*,*)
        write(*,*)trim(filename),'.fdf file is created completely'
        write(*,*)
        write(*,*)'Please check the file'
        write(*,*)

996     continue

        deallocate(cell)
        deallocate(i_cell)
        deallocate(rp_vl)
        deallocate(elct_el)
        deallocate(nu_wv)
        deallocate(nutot_el)
        deallocate(lb_el)
        deallocate(tmp_l)
        deallocate(tmp_lb_el)
        deallocate(l)
        deallocate(nu_lb)
        deallocate(i_nu_lb)
        deallocate(z_lattice)

2000    continue
3002    continue
        close(20)        
        close(10)        
        goto 997

999     continue
        write(*,*)
        write(*,*)'Error: Cannnot find file "getdata.lammps".'
        write(*,*)'===> Please run getdumpf90 to get the file'
        write(*,*)
        goto 997

998     continue
        write(*,*)
        write(*,*)'Error: Cannnot find file "position.fdf".'
        write(*,*)'===> Please run getdumpf90 to get the file'
        write(*,*)
        goto 997

990     continue
        write(*,*)
        write(*,'(1x,2a)')&
        'Error: Lack of tse_*.fdf input file for ',&
        'writing tse_*.TSHS into tss_*.fdf'
        write(*,*)
        goto 997 

95      continue
        write(*,*)
        write(*,*) 'Error: Cannot create *.fdf files.'
        write(*,*)
        goto 997

94      continue
        write(*,*)
        write(*,*) 'Error: Cannot create INCAR files.'
        write(*,*)
        goto 997

997     stop

        endprogram creatfile4transiestaorsiesta
