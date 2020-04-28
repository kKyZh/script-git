      program getiv2dat

        !#########################################################
        !### this is the sub script of getiv2dat (bash script) ###
        !#########################################################

        integer i,j,k,l
        integer chk_arg, r2e ! check argument & read to end
        integer totfl, ind_nufl ! total files and the number of opened file
        integer ind_strnu ! index nuber to fiind _ character
        integer, allocatable :: nufl(:)
        real(8) vlt, curnt ! voltage and current
        character(4) ctotfl ! char of total files
        character(16) cvlt, ccurnt ! char V and I
        character(32), allocatable :: inpt_flnam(:) 
        character(32) otpt_flnam ! argument name & iv file
        character(32) dummy ! dummy read
        character(64) trg_ln ! target line
        logical fnd_ln ! found line

        write(*,*)
        write(*,'(1x,3a)')'Running... get I-V value after TBTrans, ',&
                'output a summarized *.dat file ',&
                '(Version --1.0 beta//May/01/2019//)'
        ! not include check the name of input file

        chk_arg=command_argument_count()
        if(chk_arg==0)then
          write(*,*)
          write(*,*)'Error : Need *.out files'
          write(*,*)
          stop
        else
        endif

        call get_command_argument(1, ctotfl)
        read(ctotfl,*)totfl
        ind_nufl=totfl+1
        allocate(inpt_flnam(ind_nufl))
        allocate(nufl(totfl))
        do i=2,ind_nufl
        call get_command_argument(i, inpt_flnam(i))
        enddo
        ind_strnu=scan(trim(inpt_flnam(2)),"_")
        otpt_flnam=''//inpt_flnam(2)(1:ind_strnu)//'iv.dat'

        do i=1,totfl
        nufl(i)=10+i
        enddo

        open(2000,file=otpt_flnam, status='unknown',&
          form='formatted', access='sequential', err=97)
        write(2000,300)

        do i=1,totfl
        open(nufl(i),file=inpt_flnam(i+1), status='old',&
          form='formatted', access='sequential', err=98)

        j=0
        do
        read(nufl(i),'(a)',iostat=r2e)trg_ln
        if(r2e/=0)then
          goto 100
        else
          j=j+1
          fnd_ln=(index(trg_ln, 'Voltage, Current(A) =')/=0)
          if(fnd_ln)then
            backspace(nufl(i))
            read(nufl(i),*)(dummy,l=1,3), vlt, curnt
            k=j
          endif
        endif
        enddo

100     continue
        write(2000,"(2(1x,f11.6))")vlt, curnt*1E06

        enddo
        write(*,*)
        write(*,*)'File ',trim(otpt_flnam),' created.'
        write(*,*)

300     format("#",/,"#  Voltage, Current(uA) : ",/,"#")

        goto 99
        
97      continue
          write(*,*)
          write(*,*)'Error : Cannot create *_iv.dat file'
          write(*,*)
          goto 99

98      continue
          write(*,*)
          write(*,*)'Error : Cannot open *.out files'
          write(*,*)
          goto 99

99      continue 
          deallocate(inpt_flnam)
          deallocate(nufl)
        stop

      endprogram getiv2dat
