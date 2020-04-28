      program crtfil4strf90 
        implicit none
        integer i,j,k
        integer status, rename ! rename a file
        integer chk_emin, chk_emax ! check for min max
        integer chk_intl ! check for interval
        integer lemin ! length of emin
        integer lemax ! length of emax
        integer lintl ! length of intl
        integer lrang ! length of rang
        integer lsuffix ! length of suffix
        integer nu_file ! how many files will be created
        integer tb_nu_file ! tbtrans file number
        integer r2e ! read to end check
        integer fnd_1, fnd_2 ! first & second found line index
        integer nu_tb ! number of tab
        integer tot_ln ! total lines of tss_*.fdf
        !----------------------------------------------------
        integer chs_direc, chs_typ ! choose direction and type
        integer chk_chs_direc, chk_chs_typ ! check 
        real emin, emax ! min max range
        real intl ! interval of range
        real rang ! range between min and max
        real suffix ! suffix eV
        character(1) ichs_typ ! type of stress
        character(1) schs_typ ! sign of stress type + -
        character(4) targs(6) ! 6 target stress 0.0 include - sign
        character(32) filename ! from argument
        character(32) tmp_filna ! temp file name 
        character(32) tb_filna ! tbtrans file name 
        character(32) iemin ! write emin in char
        character(32) iemax ! write emax in char
        character(32) iintl ! write intl in char
        character(32) irang ! write rang in char
        character(32) isuffix ! write suffix in char
        character(32) tab ! tab
        character(128) targ_ln ! read dummy for each line
        logical fnd_ln_1 ! true or false for found the line
        logical fnd_ln_2 ! true or false for found the line

        write(*,*)
        write(*,'(1x,3a)')'Running... Creating *.fdf input(s) file ',&
                'for applying pressure (SIESTA) ... ',&
                '(Version --1.2 beta//Apr./24/2019//)'
              ! similar with crtfil4tbtf90 scrip
        
        !###### check argument ######

        call get_command_argument(1,filename)

        tab=achar(9)

        if(len_trim(filename)==0)then
          write(*,*)
          write(*,'(1x,2a)')&
            'Error : Please include the s_*.fdf input file for ',&
            'creating *.fdf input file(s) with applied pressure.'
          write(*,*)
          goto 999
        elseif&
          (filename(len_trim(filename)-3:len_trim(filename))/='.fdf')&
          then
          write(*,*)
          write(*,'(1x,2a)')&
            'Error : Please select the correct s_*.fdf ',&
            'input file for running SIESTA with applied pressure.'
          write(*,*)
          goto 999
        else
        endif

        !###### read arguments ######
        ! needs to be considered
        ! direction
        ! real 1.0 can automatically be changed into int 1

        do i=1,6
        targs(i)='0.0'
        enddo

101     continue
        write(*,*)
        write(*,*)'Please select the type of applied pressure.'
        write(*,*)'(1) Compressive stress'
        write(*,*)'(2) Tensile stress'
        ! manual says an opposite way
        ! probably, manual is wrong
        ! minus is tensile pressure, plus is compressive pressure
        write(*,'(a)',advance='no')'==>    '
        read(*,*,iostat=chk_chs_typ)chs_typ
        if(chk_chs_typ/=0)then
          goto 101
        elseif(chs_typ>2.or.chs_typ<1)then
          goto 101
        elseif(chs_typ==1)then
          ichs_typ='c'
          schs_typ=''
          ! plus
        elseif(chs_typ==2)then
          ichs_typ='t'
          schs_typ='-'
          ! minus
        endif

100     continue
        write(*,*)
        write(*,*)'Please choose the direction of applied pressure.'
        write(*,*)'(Uniaxial)'
        write(*,*)'(1) xx'
        write(*,*)'(2) yy'
        write(*,*)'(3) zz'
        write(*,*)'(4) xy'
        write(*,*)'(5) xz'
        write(*,*)'(6) yz'
        write(*,'(a)',advance='no')'==>    '
        read(*,*,iostat=chk_chs_direc)chs_direc
        if(chk_chs_direc/=0)then
          goto 100
        elseif(chs_direc>6.or.chs_direc<1)then
          goto 100
        else
          select case (chs_direc)
          case(1)
            if(chs_typ==2)then
              targs(1)='-1.0'
            else
              targs(1)='1.0'
            endif
          case(2)
            if(chs_typ==2)then
              targs(2)='-1.0'
            else
              targs(2)='1.0'
            endif
          case(3)
            if(chs_typ==2)then
              targs(3)='-1.0'
            else
              targs(3)='1.0'
            endif
          case(4)
            if(chs_typ==2)then
              targs(4)='-1.0'
            else
              targs(4)='1.0'
            endif
          case(5)
            if(chs_typ==2)then
              targs(5)='-1.0'
            else
              targs(5)='1.0'
            endif
          case(6)
            if(chs_typ==2)then
              targs(6)='-1.0'
            else
              targs(6)='1.0'
            endif
          endselect
        endif
        ! find a easier way for labelling

        ! ------ minimum ------

102     continue
        write(*,*)
        write(*,*)'Please enter the minimum value of pressure.'
        write(*,*)'(0 GPa < MIN <= 10 GPa)'
        write(*,*)'(0 GPa is the original input file)'
        write(*,'(a)',advance='no')'==>    '
        read(*,*,iostat=chk_emin)emin
        if(chk_emin/=0)then
          goto 102
        elseif(emin<=0.or.emin>10)then
          goto 102
        else
          write(iemin,'(f5.2)')emin
          lemin=len_trim(adjustl(iemin))
          !write(iemin,*)emin
          ! no format will print ******** 
          ! not so clear
          ! adjustl 8 no adjustl 11
        endif

        ! ------ maximum ------
        
103     continue
        write(*,*)
        write(*,*)'Please enter the maximum value of pressure.'
        write(*,*)'(MIN <= MAX <= 10 GPa)'
        write(*,'(a)',advance='no')'==>    '
        read(*,*,iostat=chk_emax)emax
        if(chk_emax/=0)then
          goto 103
        elseif(emax<emin.or.emax>10)then
          goto 103
        else
          write(iemax,'(f5.2)')emax
          lemax=len_trim(adjustl(iemax))
          ! adjustl 8 no adjustl 11
        endif
        
        rang=emax-emin
          write(irang,'(f5.2)')rang
          lrang=len_trim(adjustl(irang))

        if(rang==0)then
        write(*,*)
        write(*,'(1x,a,f<lemin>.2,a)')&
        'Only 1 file will be created with applied pressure: ',emin,&
        ' GPa'
          nu_file=1
        else
        ! ------ interval ------

104     continue
        write(*,*)
        write(*,'(1x,a,f<lemin>.2,a,f<lemax>.2)')&
          'Please enter the interval of range from: ',emin,' to ',&
          emax
        write(*,*)'(0 GPa < Interval <= 10 GPa)'
        write(*,'(a)',advance='no')'==>    '
        read(*,*,iostat=chk_intl)intl
        if(chk_intl/=0)then
          goto 104
        elseif(intl>rang.or.intl<=0)then
          goto 104
        else
          write(iintl,'(f5.2)')intl
          lintl=len_trim(adjustl(iintl))
          ! adjustl 8 no adjustl 11
        endif

        ! ------ calculate how many files we need ------
        nu_file=int(rang/intl)
        if(mod(rang,intl)/=0)then
          nu_file=nu_file+2
        else
          nu_file=nu_file+1
        endif

        endif

        ! ------ prepared finished ------
        tmp_filna=filename(1:len_trim(filename)-4)
        tb_nu_file=50
        !initial label for filenumber
        open(50,file=filename,&
          form='formatted',err=997,& 
          status='old', access='sequential')
        !###### search the line of systemlabel and voltage
        j=0
        fnd_1=0
        fnd_2=0
        do
        read(50,'(a)',iostat=r2e)targ_ln
        if(r2e/=0)then
          goto 20
        else
          j=j+1
          fnd_ln_1=(index(targ_ln,'SystemLabel')/=0)
          fnd_ln_2=(index(targ_ln,'MD.MaxForceTol')/=0)
          if(fnd_ln_1)then
            fnd_1=j
          elseif(fnd_ln_2)then
            fnd_2=j
          else
          endif
        endif
        enddo

20      continue
        rewind(50)
        if(fnd_2==0)then
        ! if not found the second search
          fnd_2=j
          tot_ln=j
        else
          tot_ln=j
        endif

        suffix=emin

        !###### change the line of systemlabel and voltage
        write(*,*)
        write(*,*)'--------------------------------------------'
        write(*,*)
        write(*,*)'### Files created ###'
        write(*,*)

        do i=1,nu_file

        write(isuffix,'(f5.2)')suffix
        lsuffix=len_trim(adjustl(isuffix))
        write(isuffix,'(f<lsuffix>.2)')suffix
        ! there is an easier way maybe

        tb_filna=&
          ''//trim(tmp_filna)//'_'&
          //trim(ichs_typ)//''//trim(isuffix)//'GPa'

        tb_nu_file=50+i

        ! changed label
        open(tb_nu_file,file=''//trim(tb_filna)//'.fdf',&
          form='formatted',err=998,& 
          status='unknown', access='sequential')

        write(*,*)''//trim(tb_filna)//'.fdf'

        do j=1,fnd_1-1
        read(50,'(a)')targ_ln
        write(tb_nu_file,'(a)')trim(targ_ln)
        enddo

        read(50,'(a)')targ_ln
        write(tb_nu_file,'(4a)')'SystemLabel',&
          (trim(tab),nu_tb=1,2),trim(adjustl(tb_filna))

        do j=fnd_1+1,fnd_2
        read(50,'(a)')targ_ln
        write(tb_nu_file,'(a)')trim(targ_ln)
        enddo

        write(tb_nu_file,'(4a)')'MD.MaxStressTol',trim(tab),&
          trim(isuffix),' GPa'
        write(tb_nu_file,'(4a)')'MD.TargetPressure',trim(tab),&
          trim(isuffix),' GPa'
        write(tb_nu_file,'(a)')'%block MD.TargetStress'
        write(tb_nu_file,'(6(1x,a))')&
          (trim(targs(k)),k=1,6)
        write(tb_nu_file,'(a)')'%endblock MD.TargetStress'

        do j=fnd_2+1,tot_ln
        read(50,'(a)')targ_ln
        write(tb_nu_file,'(a)')trim(targ_ln)
        enddo

        suffix=suffix+intl
        rewind(50)
        enddo
        
        write(*,*)
        write(*,*)'--------------------------------------------'
        write(*,*)

        goto 999

997     continue
        write(*,*)
        write(*,'(1x,a)')&
          'Error : Can not open s_*.fdf input file.'
        write(*,*)
        goto 999

998     continue
        write(*,*)
        write(*,'(1x,a)')&
          'Error : Can not creat input file with applied stress.'
        write(*,*)
        goto 999

999     stop

      endprogram crtfil4strf90
