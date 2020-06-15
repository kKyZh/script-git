      program crtfil4tbtf90

        use jsu_readline

        implicit none
        integer i,j
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
        integer posp_emin ! index position of char_*
        integer posp_emax ! index position of char_*
        integer posp_intl ! index position of char_*
        integer posp_rang ! index position of char_*
        integer posp_suffix ! index position of char_*
        integer deci_emin ! decimals number of char_*
        integer deci_emax ! decimals number of char_*
        integer deci_intl ! decimals number of char_*
        integer deci_rang ! decimals number of char_*
        integer tmp_deci_intl ! decimals number of char_*
        integer int_rang ! total length of char_*
        integer totlen_emin ! total length of char_*
        integer totlen_emax ! total length of char_*
        integer totlen_intl ! total length of char_*
        integer totlen_rang ! total length of char_*
        integer totlen_isuffix ! total length of char_*
        integer tmp_totlen_intl ! total length of char_*
        ! not clear the meaning with dp
        real emin, emax ! min max range
        real intl ! interval of range
        real rang ! range between min and max
        real suffix ! suffix eV
        real tmp_chk ! substitute for mod and modulo
        character(32) filename ! from argument
        character(32) tmp_filna ! temp file name 
        character(32) tb_filna ! tbtrans file name 
        character(32) iemin ! write emin in char
        character(32) char_emin ! char input emin
        character(32) iemax ! write emax in char
        character(32) char_emax ! char input emax
        character(32) iintl ! write intl in char
        character(32) char_intl ! char input interval
        character(32) irang ! write rang in char
        character(32) char_rang ! char input interval
        character(32) isuffix ! write suffix in char
        character(32) tab ! tab
        character(128) targ_ln ! read dummy for each line
        character(128) string ! readline
        logical fnd_ln_1 ! true or false for found the line
        logical fnd_ln_2 ! true or false for found the line
        logical chk_filex ! check file exist or not

        write(*,*)
        write(*,'(1x,3a)')'Running... Creating *.fdf input(s) file ',&
                'for TBTrans (Plural) ... ',&
                '(Version --1.96 beta//May/16/2019//)'
        
        !###### check argument ######

        call get_command_argument(1,filename)
        inquire(file=filename,exist=chk_filex)

        tab=achar(9)

        if(len_trim(filename)==0)then
          write(*,*)
          write(*,'(1x,2a)')&
            'Error : Please include the tss_*.fdf input file for ',&
            'creating TBTrans input files.'
          write(*,*)
          goto 999
        elseif&
          (filename(len_trim(filename)-3:len_trim(filename))/='.fdf')&
          then
          write(*,*)
          write(*,'(1x,2a)')&
            'Error : Please select the correct tss_*.fdf ',&
            'input file for creating TBTrans input files.'
          write(*,*)
          goto 999
        elseif(chk_filex==.false.)then
          write(*,*)
          write(*,'(1x,2a)')&
            'Error : Please select the correct tss_*.fdf ',&
            'input file for creating TBTrans input files.'
          write(*,*)
          goto 999
        else
        endif

        !###### read arguments ######
        ! ------ minimum ------

100     continue
        write(*,*)
        write(*,*)'Please enter the minimum of Voltage in real.'
        write(*,*)'(min < 100 eV)'
        call userreadline( char_emin, ' : ' )
        !read(*,*)char_emin
        read(char_emin,*,iostat=chk_emin)emin
        if(chk_emin/=0)then
          goto 100
        elseif(emin>=100.or.emin<=-100)then
          goto 100
        else
          posp_emin=index(trim(char_emin),'.')
          if(posp_emin.eq.0)then
            deci_emin=1
            totlen_emin=len_trim(char_emin) + 2
          else
            deci_emin=&
            len_trim(char_emin(posp_emin+1:len_trim(char_emin)))
            totlen_emin=len_trim(char_emin)
          endif
          write(char_emin,'(f<totlen_emin>.<deci_emin>)')emin
          !write(iemin,*)emin
          ! no format will print ******** 
          ! not so clear
          ! lemin=len_trim(adjustl(iemin))
          ! adjustl 8 no adjustl 11
        endif

        ! ------ maximum ------
        
101     continue
        write(*,*)
        write(*,*)'Please enter the maximum of Voltage in real.'
        write(*,'(1x,a,f<totlen_emin>.<deci_emin>,a)')&
          '(',emin,' eV <= max < 100 eV)'
        call userreadline( char_emax, ' : ' )
        !read(*,*)char_emax
        read(char_emax,*,iostat=chk_emax)emax
        if(chk_emax/=0)then
          goto 101
        elseif(emax<emin.or.emax>=100)then
          goto 101
        else
          posp_emax=index(trim(char_emax),'.')
          if(posp_emax.eq.0)then
            deci_emax=1
            totlen_emax=len_trim(char_emax) + 2
          else
            deci_emax=&
            len_trim(char_emax(posp_emax+1:len_trim(char_emax)))
            totlen_emax=len_trim(char_emax)
          endif
          write(char_emax,'(f<totlen_emax>.<deci_emax>)')emax
        endif
        
        int_rang=max((totlen_emax-deci_emax),(totlen_emin-deci_emin))
        deci_rang=max(deci_emax,deci_emin)
        totlen_rang=int_rang+deci_rang
        rang=emax-emin
        write(char_rang,'(f<totlen_rang>.<deci_rang>)')rang

        ! ------ interval ------
        intl=0
        if(rang==0)then
          nu_file=1
          totlen_intl=totlen_rang
          deci_intl=deci_rang
          write(char_intl,'(f<totlen_intl>.<deci_intl>)')intl
        else

102     continue

        write(*,*)
        write(*,'(1x,a,f<totlen_rang>.<deci_rang>,a)')&
          'Please enter the interval which can divide ',&
          rang,' eV'
          write(*,'(1x,a,f<totlen_rang>.<deci_rang>,a)')&
            '(0.0 eV < Interval < ',rang,' eV)'
        call userreadline( char_intl, ' : ' )
        !read(*,*)char_intl
        read(char_intl,*,iostat=chk_intl)intl
        if(chk_intl/=0)then
          goto 102
        elseif(intl>rang.or.intl<=0)then
          goto 102
        elseif(int(mod(rang,intl))/=0)then
          ! mod is not working very well at 0.1 and 0.2
          ! modulo is not working when rang 0.9 
          ! E-08 very small
          goto 102
        else
          posp_intl=index(trim(char_intl),'.')
          if(posp_intl.eq.0)then
            deci_intl=1
            totlen_intl=len_trim(char_intl) + 2
          else
            deci_intl=&
            len_trim(char_intl(posp_intl+1:len_trim(char_intl)))
            totlen_intl=len_trim(char_intl)
          endif
          write(char_intl,'(f<totlen_intl>.<deci_intl>)')intl
        endif
        ! ------ calculate how many files we need ------
        ! not perfect but okay for this time
        nu_file=int(rang/intl)
        nu_file=nu_file+1
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
          fnd_ln_2=(index(targ_ln,'TS.Voltage')/=0)
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
          write(*,*)
          write(*,*)'Error : Lack of TS.Voltage'
          write(*,*)'Please check the input file'
          write(*,*)
          goto 999
        else
          tot_ln=j
        endif
          suffix=emin
          tmp_deci_intl=deci_intl
          tmp_totlen_intl=totlen_intl

        !###### change the line of systemlabel and voltage
        write(*,*)
        write(*,*)'--------------------------------------------'
        write(*,*)
        write(*,*)'### Files created ###'
        write(*,*)

        do i=1,nu_file
        deci_intl=tmp_deci_intl
        totlen_intl=tmp_totlen_intl
          if((totlen_emax.gt.totlen_intl)&
            .and.(suffix<0))then
            totlen_intl=totlen_emax
          elseif((totlen_emin.gt.totlen_intl)&
            .and.(suffix<0))then
            totlen_intl=totlen_emin
          endif
          if(deci_emax.gt.deci_intl)then
            deci_intl=deci_emax
          elseif(deci_emin.gt.deci_intl)then
            deci_intl=deci_emin
          endif
          write(isuffix,*)suffix
          posp_suffix=index(adjustl(trim(isuffix)),'.')
          totlen_isuffix=posp_suffix+deci_intl
        write(isuffix,'(f<totlen_isuffix>.<deci_intl>)')suffix
        tb_filna=''//trim(tmp_filna)//'_'//trim(isuffix)//'v'
        tb_nu_file=50+i
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

        do j=fnd_1+1,fnd_2-1
        read(50,'(a)')targ_ln
        write(tb_nu_file,'(a)')trim(targ_ln)
        enddo

        read(50,'(a)')targ_ln
        write(tb_nu_file,'(4a)')'TS.Voltage',trim(tab),&
          trim(isuffix),' eV'

        do j=fnd_2+1,tot_ln
        read(50,'(a)')targ_ln
        write(tb_nu_file,'(a)')trim(targ_ln)
        enddo
        
        suffix=suffix+intl
        rewind(50)
        enddo
        
        write(*,*)
        write(*,*)'--------------------------------------------'

        close(50)
        do i=1,nu_file
        tb_nu_file=50+i
        close(tb_nu_file)
        enddo

        write(*,*)
        write(*,*)'### List of all *.fdf files ###'
        write(*,*)
        call system('ls *.fdf')
        write(*,*)
        write(*,*)'--------------------------------------------'
        write(*,*)

        goto 999

997     continue
        write(*,*)
        write(*,'(1x,a)')&
          'Error : Can not open tss_*.fdf input file.'
        write(*,*)
        goto 999

998     continue
        write(*,*)
        write(*,'(1x,a)')&
          'Error : Can not creat TBTrans input file.'
        write(*,*)
        goto 999

999     stop

      endprogram crtfil4tbtf90
