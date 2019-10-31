        program creatfile4transiestaorsiesta

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
        integer m ! for new position.fdf read variable
        integer lo,hi ! for low and high variables
        integer posre_nam_chk, posre_chs_chk ! renew a position.fdf name
        integer status, rename ! rename a file
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
        character(8), allocatable :: rp_vl(:)!constant repeat value
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

        write(*,*)
        write(*,'(1x,3a)')'Running... Creating *.fdf input file ',&
                'for SIESTA/TranSIESTA... ',&
                '(Version --2.22 //May/17/2019//)'
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
            else
            endif
        enddo

        if(narg>0.and.narg==nna)then
          write(*,*)
          write(*,*)'------'
          write(*,*)'No available options'
          write(*,*)'call manual: -h'
          write(*,*)
          goto 997
        else
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

        write(*,*)' '
        write(*,*)'Running...   Checking files for writing'
        open(10,file='getdata.lammps', form='formatted', status='old',& 
                err=999, access='sequential')
        open(20,file='position.fdf', form='formatted',err=998,& 
                status='old', access='sequential')
        write(*,*)'Running...   Checking completed'
        write(*,*)
        write(*,*)'----------------------------------------------------'
        write(*,*)'List of all *.fdf files : '
        write(*,*)
        call execute_command_line('ls *.fdf')
        write(*,*)'----------------------------------------------------'
        write(*,*)'Current path : '
        write(*,*)
        call execute_command_line('pwd')
        write(*,*)'----------------------------------------------------'
        write(*,*)
!------------------------------ check the file existed or nor -----
        write(*,*)'Please name the input file (Less than 32 characters)'

3000    continue
        read(*,*)filename
        inquire(file=''//trim(filename)//'.fdf',exist=chk_filex)
        if(chk_filex)then
          write(*,*)''
          write(*,*)'---------- Warning ----------'
          write(*,*)'File '//trim(filename)//'.fdf existed,',&
          'do you want to overwritten it?'
          write(*,*)'y for yes, n for no, q for quit'

3003    continue
          read(5,*,iostat=chk_chs_filex)chs_filex
          if(chk_chs_filex/=0)then
            write(*,*)''
            write(*,*)'Please choose y for yes, n for no, q for quit'
            goto 3003
          else
            select case (chs_filex)
            case('y','Y')
              goto 3001
            case('n','N')
              write(*,*)''
              write(*,*)'Then Please enter a new name'
              goto 3000
            case('q','Q')
              write(*,*)''
              write(*,*)'You quit... Bye!!'
              write(*,*)''
              goto 3002
            case default
            write(*,*)''
            write(*,*)'Please choose y for yes, n for no, q for quit'
              goto 3003
            endselect
          endif

        else
        endif

3001    continue
        open(30,file=''//trim(filename)//'.fdf', form='formatted', &
                status='unknown',access='sequential')
!---------------------------read part---------------------------

        write(*,*)' '
        write(*,*)trim(filename),'.fdf file created'
        write(*,*)' '
        write(*,*)'Running...   Writing .fdf file'

        tab=achar(9)

!----------------------------read part--------------------------
        !--------------- siesta or transiesta
        write(*,*)' '
        write(*,*)'------------------- Information -------------------'
        write(*,*)'Please choose .fdf file type'
        write(*,*)'(1)SIESTA      (2)TranSIESTA     (0)Quit'

2001    continue
        read(5,*,iostat=chk_t)chs_t
        if(chk_t/=0)then
          write(*,*)''
          write(*,*)'Please enter the correct number'
          write(*,*)'(1)SIESTA      (2)TranSIESTA     (0)Quit'
          goto 2001
        elseif(chs_t==0)then
          write(*,*)''
          write(*,'(a,i0,a)')'You choose (',chs_t,')Quit'
          write(*,*)''
          write(*,*)'Bye...'
          write(*,*)''
          goto 2000
        elseif(chs_t==1)then
          write(*,*)''
          write(*,'(a,i0,a)')'You choose (',chs_t,')SIESTA'
        elseif(chs_t==2)then
          write(*,*)''
          write(*,'(a,i0,a)')'You choose (',chs_t,')TranSIESTA'

        open(50,file=filename1, form='formatted', status='old',& 
                err=990, access='sequential')
        ! this filename1 should be tse_*.fdf input file (copy it when
        ! creating tss_*.fdf after copy s_position and tse_position file)
        !##### another way is using inquire

        filename2=filename1(1:len_trim(filename1)-4)
        ! delete '.fdf'

        elseif(chs_t/=0.or.chs_t/=1.or.chs_t/=2)then
          write(*,*)''
          write(*,*)'Please enter the correct number'
          write(*,*)'(1)SIESTA      (2)TranSIESTA     (0)Quit'
          goto 2001
        else
        endif
        !------------------- choose finishied chs_t assigned ----------

        ! read for nutot_el
        read(10,'(a)')dummy
        read(10,*)dummy!total atom
        read(10,*)nutot_sp
        rewind(10)

        allocate(nutot_el(nutot_sp))!x y z
        !read file 10 and 20       
        read(10,'(a)')dummy
        read(10,*)nutot_at!total atom
        read(10,*)nutot_sp,dummy,dummy,(nutot_el(i),i=1,nutot_sp)!total element species

        allocate(lb_el(nutot_sp))!read from #20 file
        allocate(l(nutot_sp))
        allocate(tmp_lb_el(nutot_sp))!read from #20 file
        allocate(tmp_l(nutot_sp))
        allocate(nu_lb(nutot_sp))
        allocate(i_nu_lb(nutot_sp))
        
        write(i_nutot_at,*)nutot_at!transfer i to char
        write(i_nutot_sp,*)nutot_sp

!----------------------------read position new -----------------------
        !-------read from position.fdf sorted z axis for
        !transiesiesta but not the sorted element column
        lo=1
        hi=0
        m=0
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
          write(*,*)''
          write(*,*)'Please correct the atom types for tss_*.fdf'
6000    continue
          read(*,*,iostat=re_nutot_sp_chk)re_nutot_sp
          if(re_nutot_sp_chk/=0)then
            write(*,*)''
            write(*,*)'Please enter the correct value'
            goto 6000
          else
            if(re_nutot_sp>nutot_sp.or.re_nutot_sp<1)then
              write(*,*)''
              write(*,*)'Please enter the correct range of atom types'
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

        do i=1,re_nutot_sp
        write(*,*)' '
        write(*,'(1x,2a)')'Please input the element number of ',&
                                trim(tmp_lb_el(i))

1001    continue
        read(5,*,iostat=chk)nu_lb(i)
        if(chk/=0)then
        write(*,*)' '
        write(*,*)'Please input integer for labelling the element'
        goto 1001
        else
        write(i_nu_lb(i),'(i3)')nu_lb(i)!transfer label number to char
        endif
        enddo

        else

        do i=1,nutot_sp
        write(*,*)' '
        write(*,'(1x,2a)')'Please input the element number of ',&
                                trim(lb_el(i))

1011    continue
        read(5,*,iostat=chk)nu_lb(i)
        if(chk/=0)then
        write(*,*)' '
        write(*,*)'Please input integer for labelling the element'
        goto 1011
        else
        write(i_nu_lb(i),'(i3)')nu_lb(i)!transfer label number to char
        endif
        enddo
        !---------------------------------------------------------
        endif
        !---------------------------------------------------------
!----------------------------output part---------------------
!----------------------------initial definitions---------------------

        write(30,'(a)')'############# Initial definitions ###########'
        write(30,'(4a)')'SystemLabel',&
                        (trim(tab),nu_tb=1,2),trim(adjustl(filename))
        write(30,'(4a)')'NumberOfAtoms',(trim(tab),nu_tb=1,2),&
                        trim(adjustl(i_nutot_at))
        !----------------renewed part
        if(chs_t==2)then
        write(30,'(3a,i0)')'NumberOfSpecies',(trim(tab),nu_tb=1,2),&
                        (re_nutot_sp)
        else

        write(30,'(4a)')'NumberOfSpecies',(trim(tab),nu_tb=1,2),&
                        trim(adjustl(i_nutot_sp))
        !write(*,*)(nutot_el(i),i=1,3)
        endif

        write(30,*)' '
        !#######################################################
        !---------------- renewed this part

        if(chs_t==2)then
              write(30,'(a)')'%block ChemicalSpeciesLabel'
              do i=1,re_nutot_sp
              loop1: do j=1,nutot_sp
              if(tmp_l(j)==i)exit loop1
              enddo loop1
                write(30,'(a,i0,a,i0,2a)')trim(tab),tmp_l(j),&
                  trim(tab),nu_lb(j),&
                  trim(tab),trim(adjustl(tmp_lb_el(j)))
              enddo

              write(30,'(a)')'%endblock ChemicalSpeciesLabel'
              write(30,*)''
        else

        write(30,'(a)')'%block ChemicalSpeciesLabel'
        do i=1, nutot_sp
        write(30,'(a,i0,a,i0,2a)')trim(tab),l(i),&
                trim(tab),nu_lb(i),&
                trim(tab),trim(adjustl(lb_el(i)))
        enddo
        write(30,'(a)')'%endblock ChemicalSpeciesLabel'
        write(30,*)' '
        endif

!----------------------------pseudo-atomic oritals ---------------------
        write(30,'(a)')"####### Pseudo-Atomic Orbitals (PAO's)#######"
        write(30,'(4a)')'PAO.BasisSize',&
                        (trim(tab),nu_tb=1,2),'DZP'
        write(30,'(4a)')'NetCharge',&
                        (trim(tab),nu_tb=1,2),'0.000000'
        write(30,*)' '
        
!----------------------------coordinates ---------------------
        write(30,'(a)')"################ Coordinates #######"
        write(30,'(3a)')'AtomicCoordinatesFormat',trim(tab),'Ang'
        
        !-----------------renewed part of renew a position.fdf name
        write(*,*)''
        write(*,*)'###### Information for selection ######'
        write(*,*)'Do you want to rename the position.fdf file?'
        write(*,*)'Please enter y for yes, n for no'
        
500     continue   
        read(*,*,iostat=posre_chs_chk)posre_chs
        if(posre_chs_chk/=0)then
          write(*,*)''
          write(*,*)'Please enter y for yes, n for no'
          goto 500
        else
          selectcase(posre_chs)
          case('y','Y')
            write(*,*)''
            write(*,*)'Please choose the name of new position.fdf'
            write(*,*)'(1) s_position.fdf ==> For SIESTA (Device)'
            write(*,*)&
              '(2) tse_position.fdf ==> For TranSIESTA (Electrode)'
            write(*,*)&
              '(3) tss_position.fdf ==> For TranSIESTA (Scattering)'
510     continue
            read(*,*,iostat=posre_nam_chk)posre_nam
              if(posre_nam_chk/=0)then
                write(*,*)''
            write(*,*)'Please choose the correct number below'
            write(*,*)'(1) s_position.fdf ==> For SIESTA (Device)'
            write(*,*)&
              '(2) tse_position.fdf ==> For TranSIESTA (Electrode)'
            write(*,*)&
              '(3) tss_position.fdf ==> For TranSIESTA (Scattering)'
                goto 510

              else
                selectcase(posre_nam)
              case('1')
                write(*,*)''
                write(*,*)'You choose (1) s_position.fdf'
                status=rename('position.fdf','s_position.fdf')
                status=rename('getdata.lammps','s_getdata.lammps')
                ! rename the position.fdf
                ! rename the getdata.lammps
                ! prepare for next step about tss_getdata.lammps and
                ! tss_position.fdf
                write(30,'(3a)')'AtomicCoordinatesAndAtomicSpecies <'&
                        ,trim(tab),'s_position.fdf'
            goto 501
              case('2')
                write(*,*)''
                write(*,*)'You choose (2) tse_position.fdf'
                status=rename('position.fdf','tse_position.fdf')
                status=rename('getdata.lammps','tse_getdata.lammps')
                write(30,'(3a)')'AtomicCoordinatesAndAtomicSpecies <'&
                        ,trim(tab),'tse_position.fdf'
            goto 501
              case('3')
                write(*,*)''
                write(*,*)'You choose (3) tss_position.fdf'
                status=rename('position.fdf','tss_position.fdf')
                status=rename('getdata.lammps','tss_getdata.lammps')
                write(30,'(3a)')'AtomicCoordinatesAndAtomicSpecies <'&
                        ,trim(tab),'tss_position.fdf'
            goto 501
          case default
                write(*,*)''
            write(*,*)'Please choose the correct number below'
            write(*,*)'(1) s_position.fdf ==> For SIESTA (Device)'
            write(*,*)&
              '(2) tse_position.fdf ==> For TranSIESTA (Electrode)'
            write(*,*)&
              '(3) tss_position.fdf ==> For TranSIESTA (Scattering)'
                goto 510
              endselect

              endif
          case('n','N')
        write(30,'(3a)')'AtomicCoordinatesAndAtomicSpecies <'&
                        ,trim(tab),'position.fdf'
            goto 501
          case default
            write(*,*)''
            write(*,*)'Please enter y for yes, n for no'
            goto 500
          endselect
        endif

501     continue

        write(30,*)' '

!---------------------structural (lattice) information ---------------
        allocate(cell(3))
        allocate(rp_vl(3))
        allocate(i_cell(3))
        write(30,'(a)')"##### Structural (lattace) information #####"
        write(30,'(5a)')'LatticeConstant',trim(tab),'1.0',trim(tab),&
                        'Ang'
        write(30,'(a)')'%block LatticeParameters'
        do j=1,3
        read(10,*)dummy,cell(j)
        !write(*,*)cell(j),dummy
        write(i_cell(j),*)cell(j)
        enddo
        do i=1,3
        rp_vl(i)='90.0000'
        enddo
        write(30,'(12a)')&
                ((trim(tab),(trim(adjustl(i_cell(i))))),i=1,3),&
                ((trim(tab),(trim(adjustl(rp_vl(i))))),i=1,3)
        write(30,'(a)')'%endblock LatticeParameters'
        write(30,*)' '

!--------------------- k-points for sampling ---------------
        write(*,*)''
        write(*,'(1x,a)')'------- Input k-points for sampling --------'
        write(*,'(1x,a)')'Please input how many k-points on x direction'

1002    continue
        read(5,*,iostat=chk)kp_x
        write(*,*)' '
        if(chk/=0)then
        write(*,'(1x,2a)')'Please input correct number for k-poionts ',&
                        'on x direction'
        goto 1002
        else
        endif

        write(*,'(1x,a)')'Please input how many k-points on y direction'
1003    continue
        read(5,*,iostat=chk)kp_y
        write(*,*)' '
        if(chk/=0)then
        write(*,'(1x,2a)')'Please input correct number for k-poionts ',&
                        'on y direction'
        goto 1003
        else
        endif

        write(*,'(1x,a)')'Please input how many k-points on z direction'
1004    continue
        read(5,*,iostat=chk)kp_z
        write(*,*)' '
        if(chk/=0)then
        write(*,'(1x,2a)')'Please input correct number for k-poionts ',&
                        'on z direction'
        goto 1004
        else
        endif

        write(30,'(a)')"######## k-points for sampling #########"
        write(30,*)' '
        write(30,'(a)')'%block kgrid_Monkhorst_Pack'
        write(30,'(1x,i3,1x,a11)')kp_x,'  0   0 0.5'
        write(30,'(1x,a3,1x,i3,1x,a7)')'  0',kp_y,'  0 0.5'
        write(30,'(1x,a8,i3,1x,a3)')'  0   0 ',kp_z,'0.5'
        write(30,'(a)')'%endblock kgrid_Monkhorst_Pack'
        write(30,*)' '

        !########################## supercell
        if(chs_t==2)then
          write(30,*)'###### one supercell for tss_*.fdf ######'
          write(30,'(a)')'%block supercell'
          write(30,'(a)')'1 0 0'
          write(30,'(a)')'0 1 0'
          write(30,'(a)')'0 0 1'
          write(30,'(a)')'%endblock supercell'
          write(30,*)''
        else
        endif

!--------------------- Exchange Correlation (XC) functionals ---------------
        write(30,'(a)')"### Exchange Correlation (XC) functionals ###"
        write(30,'(3a)')'XC.functional',trim(tab),'GGA'
        write(30,'(3a)')"XC.authors",trim(tab),'PBE'
        write(30,'(3a)')"SpinPolarized",trim(tab),'F'

        if(nutot_at>=320)then
          write(30,'(3a)')"MeshCutoff",trim(tab),'350.0 Ry'
        else
          write(30,'(3a)')"MeshCutoff",trim(tab),'300.0 Ry'
        endif

        write(30,'(3a)')"MaxSCFIterations",trim(tab),'250'
        write(30,*)' '

!--------------------- Density Matrix (DM) ---------------
        write(30,'(a)')"########## Density Matrix (DM) ##########"
        write(30,'(3a)')'DM.MixingWeight',trim(tab),'0.02'
        !############################## 0.1 can not converged some times
        write(30,'(3a)')'DM.Tolerance',trim(tab),'0.001'
        ! DM.tolerance no unit
        ! 0.00001 may be the default value
        write(30,'(3a)')'DM.NumberPulay',trim(tab),'3'
        !##### 2 is converged 4 or others are not checked
        write(30,'(3a)')'DM.RequireEnergyConvergence',trim(tab),'T'
        ! maybe similar with dm.tolerance
        write(30,'(3a)')'DM.EnergyTolerance',trim(tab),'0.001 eV'
        ! dm energytolerance has unit
        ! 0.00001 may be the default value
        write(30,*)' '

!-------------------- Solution Method For Eigenvalues ---------------
        write(30,'(a)')"##### Solution Method for Eigenvalues #####"
        if(chs_t==1)then
        write(30,'(3a)')'SolutionMethod',trim(tab),'diagon'
        elseif(chs_t==2)then
        write(30,'(3a)')'SolutionMethod',trim(tab),'transiesta'
        ! transiesta second step is transiesta
        ! give a choice for siesta or transiesta
        else
        endif
        ! ------------------------------------ select Transiesta or siesta
        write(30,'(3a)')'Diag.DivideAndConquer',trim(tab),'T'
        write(30,'(3a)')'Diag.ParallelOverK',trim(tab),'F'
        ! T is not suitable for CCMS but sicvm maybe ok
        write(30,*)' '

!-------------------- Occupation of Electronic States ---------------
        write(30,'(a)')"##### Occupation of Electronic States #####"
        if(nutot_at>=320)then
          write(30,'(3a)')'ElectronicTemperature',trim(tab),'450.0 K'
        else
          write(30,'(3a)')'ElectronicTemperature',trim(tab),'300.0 K'
        endif
        write(30,*)''

!-------------------- Molecular Dynamics (MD) ---------------
        write(30,'(a)')"####### Molecular Dynamics (MD) ########"
        write(30,'(3a)')'MD.TypeOfRun',trim(tab),'CG'
        ! coordiante optimazation CG
        !######## renewed 
        if(chs_t==1)then
          if(nutot_at>=500)then
            write(30,'(3a)')'MD.NumCGsteps',trim(tab),'2'
          elseif(nutot_at>=320.and.nutot_at<500)then
            write(30,'(3a)')'MD.NumCGsteps',trim(tab),'60'
          elseif(nutot_at>0.and.nutot_at<320)then
            write(30,'(3a)')'MD.NumCGsteps',trim(tab),'200'
          else
          endif
        elseif(chs_t==2)then
        write(30,'(3a)')'MD.NumCGsteps',trim(tab),'000'
        else
        endif

        write(30,'(3a)')'MD.VariableCell',trim(tab),'T'
        write(30,'(3a)')'MD.ConstantVolume',trim(tab),'F'
        write(30,'(3a)')'MD.MaxForceTol',trim(tab),'0.04 eV/Ang'
        ! 0.04 may be the default value 0.03 or 0.02 maybe enough for
        ! full relaxation
        write(30,'(3a)')'MD.MaxCGDispl',trim(tab),'0.2 Bohr'
        ! 0.02 for displacement every CG less is more reliable but take
        ! longer time
        write(30,*)' '

!---------------------- BandLinesScale --------------------
        write(30,'(a)')"####### BandLinesScale ########"
        write(30,'(3a)')'BandLinesScale',trim(tab),&
                        'ReciprocalLatticeVectors'
        write(30,*)' '
        write(30,'(a)')'%block BandLines'
        write(30,'(9a)')'1',trim(tab),'0.000000',trim(tab),&
                '0.000000',trim(tab),'0.000000',trim(tab),&
                '\Gamma'!from gamma
        write(30,'(8a)')'4',trim(tab),'0.000000',trim(tab),&
                '0.000000',trim(tab),'0.500000',trim(tab)
                !to 20 can be changed
                ! 5 maybe enought for drawing
        write(30,'(a)')'%endblock BandLines'
        write(30,*)' '

!---------------------- Output of Selected Wavefunctions --------------------
        allocate(elct_el(nutot_sp))
        allocate(nu_wv(6))!6 waves will be calculated
        sum_elct=0
        write(*,*)'---- Output of Selected Wavefunctions -----'
        write(*,*)' '
        write(*,*)&
        'How many electrons will be calcualted in this section and',&
                        ' where is HOMO'
        write(*,*)' '
        write(*,*)&
        '(Ex. Carbon has 2s and 2p, total in 4, to be calculated)'
        write(*,*)' '
        !###############################################################
        do i=1,nutot_sp
        write(*,'(1x,3a)')'Please input the charge of ',trim(lb_el(i)),&
                ' atom '
1005    continue
        read(5,*,iostat=chk)elct_el(i)
        write(*,*)' '
        if(chk/=0)then
        write(*,*)'Please input the integer for',&
                ' calculating the wavefunction'
        goto 1005
        else
        sum_elct=sum_elct+elct_el(i)*nutot_el(i)
        endif
        enddo

        nu_ho=sum_elct/2

        do i=1,6
        nu_wv(i)=nu_ho-3
        enddo
        do i=1,6
        nu_wv(i)=nu_wv(i)+i!start from ho -2
        enddo

        write(30,'(a)')&
                "##### Output of Selected Wavefunctions ######"
        write(30,'(3a)')'WaveFuncKPointsScale',trim(tab),'pi/a'
                                !unit of k-points
        write(30,'(a)')'%block WaveFuncKPoints'
        write(30,'(3(1x,a),6(1x,i5))')'0.000','0.000'&
                        ,'0.000',&
                       ((nu_wv(i)),i=1,6)
        write(30,'(a)')'%endblock WaveFuncKPoints'
        write(30,*)' '
        !----.band.wfsx. full.wfsx energy band index selection -------
        write(30,'(a)')"####### fullbz.wfsx energy selection ########"
        write(30,'(a)')'WFS.WriteForBands T'
        write(30,'(a)')'WFS.EnergyMin     -7.00 eV'
        write(30,'(a)')'WFS.EnergyMax     0.00  eV'
        write(30,*)' '

        write(30,'(a)')&
          "##### fullbz.wfsx .band.wfsx. band index selection ########"
        write(30,'(2a,i0)')'WFS.BandMin',trim(tab),nu_wv(1)
        write(30,'(2a,i0)')'WFS.BandMax',trim(tab),nu_wv(6)
        write(30,*)' '

!---------------------- Density of States (DOS) --------------------
        write(30,'(a)')"####### Density of States (DOS) ########"
        write(30,'(a)')'%block ProjectedDensityOfStates'
        write(30,'(9a)')'-7.00',trim(tab),'0.00',trim(tab),&
                '0.02',trim(tab),'3000',trim(tab),'eV'
                !settle from -7 to 0 and 0.01 resolution
                !3000 points between 0.02?
        write(30,'(a)')'%endblock ProjectedDensityOfStates'
        write(30,*)' '
        
!------------- Local Density of States (DOS) -----------------
        write(30,'(a)')"####### Local Density of States (LDOS) ########"
        write(30,'(a)')'%block LocalDensityOfStates'
        write(30,'(5a)')'-7.00',trim(tab),'0.00',trim(tab),'eV'
                !local density of states from -7 to 0
        write(30,'(a)')'%endblock LocalDensityOfStates'
        write(30,*)' '

!------------- Output options -----------------
        write(30,'(a)')"####### Output options ########"
        write(30,'(3a)')'WriteCoorStep',trim(tab),'T'
        write(30,'(3a)')'WriteForces',trim(tab),'T'
        write(30,'(3a)')'WriteKpoints',trim(tab),'T'
        write(30,'(3a)')'WriteCoorXmol',trim(tab),'T'
        write(30,'(3a)')'WriteCoorInitial',trim(tab),'T'
        write(30,'(3a)')'WriteEigenvalues',trim(tab),'T'
        write(30,'(3a)')'WriteDM',trim(tab),'T'
        write(30,'(3a)')'WriteBands',trim(tab),'T'
        write(30,'(3a)')'WriteKbands',trim(tab),'T'
        write(30,'(3a)')'WriteWaveFunctions',trim(tab),'T'
        write(30,'(3a)')'WriteCoorCerius',trim(tab),'T'
        write(30,'(3a)')'WriteMDXmol',trim(tab),'T'
        write(30,'(3a)')'WriteMDhistory',trim(tab),'T'
        write(30,'(3a)')'WriteDenchar',trim(tab),'T'
        write(30,'(3a)')'COOP.Write',trim(tab),'T'
        write(30,'(3a)')'WriteMullikenPop',trim(tab),'1'
        write(30,*)' '

!------------- Save options -----------------
        write(30,'(a)')"####### Save options ########"
        write(30,'(3a)')'DM.UseSaveDM',trim(tab),'T'
        write(30,'(3a)')'MD.UseSaveCG',trim(tab),'T'
        write(30,'(3a)')'MD.UseSaveXV',trim(tab),'T'
                        ! .XV and .xyz different?
        write(30,*)' '

!------------------------------ for transiesta -------------------
      ! transiesta use the same with siesta first and then add
      ! transiesta parameters for electrodes and chenge the diagon to
      ! transiesta (solution method)
        if(chs_t==2)then ! selected transiesta file created 
        write(30,'(a)')"##### Transiesta/tbtrans information #####"
        write(30,'(a)')"##### GF Option #####"
        write(30,'(3a)')'TS.ComplexContour.Emin',trim(tab),'-30.0 eV'
        write(30,'(3a)')'TS.ComplexContour.NPoles',trim(tab),'03'
        write(30,'(3a)')'TS.ComplexContour.NCircle',trim(tab),'30'
        write(30,'(3a)')'TS.ComplexContour.NLine',trim(tab),'10'
        write(30,*)' '

        write(30,'(a)')"##### Bias Contour Options #####"
        write(30,'(3a)')'TS.BiasContour.NumPoints',trim(tab),'10'
        write(30,'(3a)')'TS.BiasContour.Eta',trim(tab),'0.000001 Ry'
        write(30,*)''

        write(30,'(a)')"##### TS Voltage #####"
        write(30,'(3a)')'TS.Voltage',trim(tab),'0.000000 eV'
        ! maybe give the choose for change the parametter
        write(30,*)' '

        write(30,'(a)')"##### TBT Options #####"
        write(30,'(3a)')'TS.TBT.HSFile',trim(tab),&
          ''//trim(filename)//'.TSHS'
        ! tbtrans options for plot a graph
        write(30,'(3a)')'TS.TBT.Emin',trim(tab),'-5.0 eV'
        write(30,'(3a)')'TS.TBT.Emax',trim(tab),'+5.0 eV'
        write(30,'(3a)')'TS.TBT.NPoints',trim(tab),'100'
        write(30,'(3a)')'TS.TBT.NEigen',trim(tab),'3'
        write(30,'(3a)')'###TS.TBT.PDOSFrom',trim(tab),'-7'
        write(30,'(3a)')'###TS.TBT.PDOSto',trim(tab),'0'
        ! PDOSfrom and PDOSto need integers
        write(30,'(3a)')'TS.TBT.CalcIeig',trim(tab),'T'
        write(30,*)' '

        write(30,'(a)')"##### write hamiltonian #####"
        write(30,'(3a)')'TS.SaveHS',trim(tab),'T'
        write(30,*)' '
!------------------------------ electrode atoms selection --------------
        write(*,*)'Please choose how many atoms as electrode for .TSHS'

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

        write(30,'(a)')"##### left electrode #####"
        write(30,'(3a)')'TS.HSFileLeft',trim(tab),&
                        ''//trim(filename2)//'.TSHS'
        write(30,'(2a,i0)')'TS.NumUsedAtomsLeft',trim(tab),chs_e
        write(30,'(3a)')'TS.BufferAtomsLeft',trim(tab),'0'
        write(30,*)' '

        write(30,'(a)')"##### right electrode #####"
        write(30,'(3a)')'TS.HSFileRight',trim(tab),&
                        ''//trim(filename2)//'.TSHS'
        write(30,'(2a,i0)')'TS.NumUsedAtomsRight',trim(tab),chs_e
        write(30,'(3a)')'TS.BufferAtomsRight',trim(tab),'0'
        write(30,*)' '
        
        ! need to be add many options for creating.
        else
        endif

        close(50)
        !###################################################
!------------- finished ----------------
        if(chs_t==1)then
        write(*,*)' '
        write(*,*)'-------- For SIESTA --------'
        write(*,*)' '
        elseif(chs_t==2)then
        write(*,*)' '
        write(*,*)'-------- For TranSIESTA ---------'
        write(*,*)' '
        else
        endif
        write(*,'(a)')'------ Details of input file--------'
        write(*,*)' '
        write(*,'(a)')'------- WaveFuncKPoints ------'
        write(*,*)' '
        write(*,'(1x,a,i5)')'Total electrons = ',sum_elct
                        !total electrons less than 100k
        write(*,'(1x,a,i5)')'HOMO is at ',nu_ho
                        !total electrons less than 100k
        write(*,'(3(1x,a),6(1x,i5))')'0.000','0.000'&
                        ,'0.000',&
                       ((nu_wv(i)),i=1,6)
        write(*,*)' '
        
        if(chs_t==2)then
        write(*,'(a)')'------- TranSIESTA ------'
        write(*,*)''
        write(*,'(2a,i0)')&
          'Atoms as electrode for transiesta',trim(tab),chs_e
        write(*,*)' '
        else
        endif

        write(*,'(a)')'------ End of Details --------'

        write(*,*)' '
        write(*,*)'Running...   Writing completed'
        write(*,*)' '
        write(*,*)trim(filename),'.fdf file is created completely'
        write(*,*)' '
        write(*,*)'Please check the file'
        write(*,*)' '

996     continue

        deallocate(cell)
        deallocate(i_cell)
        deallocate(rp_vl)
        deallocate(elct_el)
        deallocate(nu_wv)

2000    continue
        deallocate(nutot_el)
        deallocate(lb_el)
        deallocate(tmp_l)
        deallocate(tmp_lb_el)
        deallocate(l)
        deallocate(nu_lb)
        deallocate(i_nu_lb)

        close(30)        

3002    continue
        close(20)        
        close(10)        

        goto 997

999     continue
        write(*,*)' '
        write(*,*)'Error: Cannnot find file "getdata.lammps".'
        write(*,*)'===> Please run getdumpf90 to get the file'
        write(*,*)' '
        goto 997

998     continue
        write(*,*)' '
        write(*,*)'Error: Cannnot find file "position.fdf".'
        write(*,*)'===> Please run getdumpf90 to get the file'
        write(*,*)' '
        goto 997

990     continue
        write(*,*)''
        write(*,'(1x,2a)')&
        'Error: Lack of tse_*.fdf input file for ',&
        'writing tse_*.TSHS into tss_*.fdf'
        write(*,*)''
        goto 997 

997     stop

        contains
         subroutine manual()
           write(*,*)
           write(*,*)'    ########################'
           write(*,*)'    #                      #'
           write(*,*)'    #        Manual        #'
           write(*,*)'    #                      #'
           write(*,*)'    ########################'
           write(*,*)
           write(*,*)'How to run the whole process ==>'
           write(*,*)
           write(*,*)'------------ SIESTA ------------'
           write(*,*)
           write(*,*)'1. car2Lammpsf90'
           write(*,*)'    ==> Need *.car file'
           write(*,*)'    ==> Create data.lammps'
           write(*,*)
           write(*,*)'2. crtfil4lammpsf90'
           write(*,*)'    ==> Need data.lammps'
           write(*,*)'    ==> Create in.min'
           write(*,*)
           write(*,*)'3. copy CH.airebo potential file'
           write(*,*)'    ==> For C-H bonding length'
           write(*,*)
           write(*,*)'4. lammps (crt4run_sh)'
           write(*,*)'    ==> Need in.min'
           write(*,*)'    ==> Create dump.GNR'
           write(*,*)
           write(*,*)'5. getdumpf90'
           write(*,*)'    ==> Need dump.GNR'
           write(*,*)'    ==> Create getdata.lammps'
           write(*,*)'    ==> Create position.fdf'
           write(*,*)
           write(*,*)'6. crtfil4trasief90'
           write(*,*)'    ==> Need getdata.lammps'
           write(*,*)'    ==> Need position.fdf'
           write(*,*)'    ==> Create s_*.fdf input file'
           write(*,*)'        (Need to change name to s_*.fdf/lammps)'
           write(*,*)
           write(*,*)'7. copy and rename C.psf & H.psf'
           write(*,*)
           write(*,*)'--- TranSIESTA for Electrode ---'
           write(*,*)
           write(*,*)'1. car2Lammpsf90'
           write(*,*)'    ==> Need *.car file'
           write(*,*)'    ==> Create data.lammps'
           write(*,*)
           write(*,*)'2. crtfil4lammpsf90'
           write(*,*)'    ==> Need data.lammps'
           write(*,*)'    ==> Create in.min'
           write(*,*)
           write(*,*)'3. copy CH.airebo potential file'
           write(*,*)'    ==> For C-H bonding length'
           write(*,*)
           write(*,*)'4. lammps (crt4run_sh)'
           write(*,*)'    ==> Need in.min'
           write(*,*)'    ==> Create dump.GNR'
           write(*,*)
           write(*,*)'5. getdumpf90'
           write(*,*)'    ==> Need dump.GNR'
           write(*,*)'    ==> Create getdata.lammps'
           write(*,*)'    ==> Create position.fdf'
           write(*,*)
           write(*,*)'6. crtfil4trasief90'
           write(*,*)'    ==> Need getdata.lammps'
           write(*,*)'    ==> Need position.fdf'
           write(*,*)'    ==> Create tse_*.fdf input file'
           write(*,*)'        (Need to change name to tse_*.fdf/lammps)'
           write(*,*)
           write(*,*)'7. copy and rename C.psf & H.psf'
           write(*,*)
           write(*,*)'--- TranSIESTA for Scattering ---'
           write(*,*)
           write(*,*)'1. getdumpf90'
           write(*,*)'    ==> Rename number of atom types respectively' 
           write(*,*)
           write(*,*)'2. copy'
           write(*,'(1x,3a)')&
             '    ==> Copy tse_getdata.lammps, tse_position.fdf,',&
             ' tse_*.TSHS, tse_*.fdf, tse_*.xyz, tse_*.STRUCT_OUT ',&
             ' (After relaxation) to the directory of SIESTA run'
           write(*,*)'    (After rename, copy all files recommanded)'
           write(*,*)
           write(*,*)'3. mer4fdff90'
           write(*,*)'    ==> Need s_getdata.lammps'
           write(*,*)'    ==> Need s_position.fdf'
           write(*,*)'    ==> Need tse_getdata.lammps'
           write(*,*)'    ==> Need tse_position.fdf'
           write(*,*)'    ==> Create getdata.lammps'
           write(*,*)'    ==> Create position.fdf'
           write(*,*)'    (Only merge position coordinates not relaxed)'
           write(*,*)
           write(*,*)'4. crtfil4trasief90'
           write(*,*)'    ==> Need tse_*.fdf input file'
           write(*,*)'    ==> Need getdata.lammps'
           write(*,*)'    ==> Need position.fdf'
           write(*,*)'    ==> Create tss_*.fdf input file'
           write(*,*)'    ==> Create tss_position.fdf'
           write(*,*)'    (After mer4fdff90 run)'
           write(*,*)'    (Need to choose transiesta)'
           write(*,*)'    (Need to correct atom types)'
           write(*,*)'    (Need to change name to tss_*.fdf/lammps)'
           write(*,*)
           write(*,*)'5. xyz2fdff90'
           write(*,*)'    ==> Need s_*.xyz'
           write(*,*)'    ==> Need tse_*.xyz'
           write(*,*)'    ==> Need s_*.STRUCT_OUT'
           write(*,*)'    ==> Need tse_*.STRUCT_OUT'
           write(*,*)'    ==> Need tss_*.fdf input file'
           write(*,*)'    (*.xyz files have an order)'
           write(*,*)'    (First is from SIESTA)'
           write(*,*)'    (Second is from TranSIESTA)'
           write(*,*)'    (Correct coordinates after relaxation)'
           write(*,*)'    (Correct cell vectors after relaxation)'
           write(*,*)
           write(*,*)'------ Copy & Submit job ------'
           write(*,*)
           write(*,*)'1. crt4run_sh'
           write(*,*)
           write(*,*)'2. run'
           write(*,*)
           write(*,*)'#########################################'
           write(*,*)
         endsubroutine manual

        endprogram creatfile4transiestaorsiesta
