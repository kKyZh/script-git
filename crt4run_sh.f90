        program crt4run_shf90

          use jsu_readline

        implicit none
        integer i, j, k, l
        integer chk,chk_1!check input value
        integer chs_2,chs_3,chs_4,chs_6,chs_6_1 !choose ,chs_4_1        
        integer chs_10, chk_10 ! choose for transiesta or siesta
        integer chs_fil ! choose how many files for tbtran
        integer mod_nu_que_fil ! remainder of que file
        integer nu_que_fil ! number of que files
        integer nu_job_fil ! number of total job files before run
        integer nu_job_fil_hi, nu_job_fil_lo ! low to high jobs
        integer nu_fil ! input .fdf files for tbtrans
        character(128) string ! user input interoperate with C
        character(32) chr_nu_que_fil ! write integer into character
        character(32) filename1 
        ! for first siesta or transiesta electrode
        character(32) filename2 ! for transiesta electrode or scatting
        character(32) filename3 ! for transiesta scatting
        character(32) tab ! for tab format
        character(128) chs_1,chs_5 !choose
        character(32) i_chs_3,i_chs_4,i_chs_6 !choose
        character(32) i_chs_6_1 !choose,i_chs_4_1
        character(32) slt_2 !choose
        character(32), allocatable :: filename(:) ! filename array

        ! the interface is not necessory
        !interface
        !  logical function check_filename2(x)
        !    character(32),intent(in),optional :: x
        !  endfunction check_filename2
        !endinterface

        ! filename2 option for transiesta
        ! optional argument can not in main unit
        nu_fil=command_argument_count()
        allocate(filename(nu_fil))
        call get_command_argument(1,filename1)
        if(len_trim(filename1)==1)then
          if(filename1=='2')then
            read(filename1,*)chs_10
            do i=2,nu_fil
            call get_command_argument(i,filename(i))
            filename(i)=filename(i)(:(len_trim(filename(i))-4))
            enddo
          elseif(filename1=='6')then
            read(filename1,*)chs_10
            call get_command_argument(2,filename2)
          endif
        else
        call get_command_argument(2,filename2)
        call get_command_argument(3,filename3)
        if(check_filename3(filename3))then
          ! siesta and transiesta
          chs_10=1
            filename1=filename1(:(len_trim(filename1)-4))
            filename2=filename2(:(len_trim(filename2)-4))
            filename3=filename3(:(len_trim(filename3)-4))
            !delete .fdf or in.
        else
        if(check_filename1(filename1))then
          if(check_filename2(filename2))then
            ! transiesta
            chs_10=5
            filename1=filename1(:(len_trim(filename1)-4))
            filename2=filename2(:(len_trim(filename2)-4))
            !delete .fdf or in.
          else
            ! siesta
            chs_10=4
            filename1=filename1(:(len_trim(filename1)-4))
          endif
        else
          ! lammps
          chs_10=3
            filename1=filename1(4:)
        endif
        endif
        endif
        ! give a function to achieve auto selecting transiesta or siesta

        ! little be messy because of use transfer from integer to
        ! character, use i0 format is better
        write(*,*)
        write(*,'(1x,a)')'---------- INFORMATION: FOR CCMS ------------'
        write(*,*)' '
        write(*,*)'Creating a shell scrip for CCMS ==> run.sh'
        write(*,*)'(version-2.41 //Mar/1/2020/)'
        write(*,*)''
        if(chs_10==1)then
          write(*,*)'------ For Full SIESTA and TranSIESTA ------'
        elseif(chs_10==6)then
          write(*,*)'------ For Denchar ------'
        elseif(chs_10==5)then
          write(*,*)'------ For TranSIESTA ------'
        elseif(chs_10==4)then
          write(*,*)'------ For SIESTA ------'
        elseif(chs_10==3)then
          write(*,*)'------ For Lammps ------'
        elseif(chs_10==2)then
          write(*,*)'------ For TBTrans ------'
          write(*,*)'Warning : Only parallel by k-points'
          write(*,*)
          write(*,*)'########################################'
          write(*,*)'This included new function : run_#.sh'
          write(*,*)'Run qsub-multi for multi-jobs submission'
          write(*,*)'########################################'
          write(*,*)
        else
        endif

        !###### denchar for a quick run ######
        if(chs_10==6)then

        ! open files separately and close in 1099
        open(40,file='run.sh', form='formatted', err=9999, & 
                status='unknown', access='sequential')

        write(40,'(a)')'#!/bin/sh'
        write(40,'(a)')'#PBS -q DP_002'
        write(40,'(a)')'#PBS -l walltime=00:30:00' 
        write(40,'(a)')'#PBS -l select=2'
        write(40,'(a)')'#PBS -N Denchar'
        write(40,'(a)')'cd ${PBS_O_WORKDIR}'
        write(40,*)''
        goto 1100
        else
        endif
        !###### ----------------------------------------- ######

        !###### lammps for a quick run to get CH distance ######
        if(chs_10==3)then

        ! open files separately and close in 1099
        open(40,file='run.sh', form='formatted', err=9999, & 
                status='unknown', access='sequential')

        write(40,'(a)')'#!/bin/sh'
        write(40,'(a)')'#PBS -q DP_002'
        write(40,'(a)')'#PBS -l walltime=00:30:00' 
        write(40,'(a)')'#PBS -l select=2'
        write(40,'(a)')'#PBS -N lammps_CH'
        write(40,'(a)')'cd ${PBS_O_WORKDIR}'
        write(40,*)''
        goto 1100
        else
        endif
        !###### ----------------------------------------- ######

        !########## for tbtran, multi queue files

          nu_job_fil = nu_fil - 1
          ! nu_fil is the number of command line, file should -1

        if(chs_10==2)then
          write(*,*)'How many tasts in one script of submission?'
          write(*,*)'(For boosting tbtrans)'
          write(*,'(a,i0)') ' 0 < job in one file <= ', nu_job_fil

800     continue
        !  write(*,'(1x,a)', advance='no') '(Integer) ==>  '
        call userreadline(string, '(Integer) ==> ')
        read(string,*,iostat=chk)chs_fil
        if(chk/=0)then
        write(*,*)
        write(*,*)'Please input an integer'
        goto 800
        else

          if(chs_fil.lt.1.or.chs_fil.gt.nu_job_fil)then
            write(*,*)
            write(*,'(a,i0)') ' 0 < job in one file <= ', nu_job_fil
            goto 800
          elseif(chs_fil.ge.1.or.chs_fil.le.nu_job_fil)then
            ! mod_nu_que_fil is the remainder of que file
            ! nu_que_fil jobs in script for multi submission
            ! nu_job_fil jobs in command line include one command
            mod_nu_que_fil = mod(nu_job_fil,chs_fil)
            if(mod_nu_que_fil.ne.0)then
            nu_que_fil = nu_job_fil / chs_fil +1
            elseif(mod_nu_que_fil.eq.0)then
            nu_que_fil = nu_job_fil / chs_fil
            endif
            !write(*,*) nu_que_fil, nu_job_fil, mod_nu_que_fil
            ! chs_fil jobs per nu_que_fil jobs + mod_nu_que_fil jobs
            ! from nu_job_fil files
          endif

        endif
        endif

        !########## for tbtran, multi queue files
        
        ! queue type select ---------------------------------------
        write(*,*)' '
        write(*,*)'Which queue do you want to choose (This time only 3)'
        write(*,*)'(1) DP_002,  (2) P_016,  (3) C_002, (4) (VM)'
        ! should use logical to write different selection ==> combine
        ! two parts

994     continue

        call userreadline(string, '(Integer) ==> ')
        read(string,*,iostat=chk)chs_2
        if(chk/=0)then
        write(*,*)
        write(*,*)'Please input an integer'
        goto 994
        else

        select case(chs_2)
        case(1)

        ! ### open file after choose queue type
        open(40,file='run.sh', form='formatted', err=9999, & 
                status='unknown', access='sequential')
        write(40,'(a)')'#!/bin/sh'
        ! ### open finished

        write(*,*)' '
        write(*,*)'You choose (1) DP_002'
        slt_2='DP_002'
        write(40,'(2a)')'#PBS -q ', trim(slt_2)
        
        ! wall time select ------------------------------------
        write(*,*)' '
        write(*,*)'Please input walltime, how many minutes?',&
        ' (30 mins limited, only 2 numbers limited)'
        write(6,*)'30'
        write(40,'(a)')'#PBS -l walltime=00:30:00' 

        !read(5,*,iostat=chk_1)chs_3
        !if(chk_1/=0)then
        !write(*,*)'Please input an integer'
        !else
        !write(i_chs_3,'(i2.2)')chs_3
        !write(40,'(4a)')'#PBS -l walltime=','00:',trim(i_chs_3),&
        !                                ':00' 
        !endif

        case(2)

        ! ### open file after choose queue type
        open(40,file='run.sh', form='formatted', err=9999, & 
                status='unknown', access='sequential')
        write(40,'(a)')'#!/bin/sh'
        ! ### open finished

        write(*,*)' '
        write(*,*)'You choose (2) P_016'
        slt_2='P_016'
        write(40,'(2a)')'#PBS -q ', trim(slt_2)
        
        ! wall time select ------------------------------------
        write(*,*)' '
        write(*,*)'Please input walltime, how many hours?',&
        ' (72 hours limited, only 2 numbers limited)'
801     continue
        call userreadline(string, '(Integer) ==> ')
        read(string,*,iostat=chk_1)chs_3
        if(chk_1/=0)then
        write(*,*)
        write(*,*)'Please input an integer'
        goto 801
        else
        write(i_chs_3,'(i2.2)')chs_3 ! i2.2 fill 0 automatically
        write(40,'(3a)')'#PBS -l walltime=',trim(i_chs_3),':00:00'
        endif

        case(3)

        ! ### open file after choose queue type
        open(40,file='run-gpu.sh', form='formatted', err=9999, & 
                status='unknown', access='sequential')
        write(40,'(a)')'#!/bin/sh'
        ! ### open finished

        write(*,*)
        write(*,*)'You choose (3) C_002'
        slt_2='C_002'
        write(40,'(2a)')'#PBS -q ', trim(slt_2)
        
        ! wall time select ------------------------------------
        write(*,*)' '
        write(*,*)'Please input walltime, how many hours?',&
        ' (72 hours limited, only 2 numbers limited)'
802     continue
        call userreadline(string, '(Integer) ==> ')
        read(string,*,iostat=chk_1)chs_3
        if(chk_1/=0)then
        write(*,*)
        write(*,*)'Please input an integer'
        goto 802
        else
        write(i_chs_3,'(i2.2)')chs_3 ! i2.2 fill 0 automatically
        write(40,'(3a)')'#PBS -l walltime=',trim(i_chs_3),':00:00'
        endif

        case default
        write(*,*)' '
        write(*,*)'Please choose 1, 2, 3 for the queue type'
        write(*,*)'(4 is under construction)'
        goto 994

        endselect
        endif

        write(*,*)' '
        write(*,'(1x,3a)') & 
          'Please input how many nodes you want?', &
          ' (TBTrans prefer 1 node,', &
          ' and cores will be entered later)'
        write(*,*)'Limit: DP_002 (2), P_016 (16), C_002(2)'
        ! no option for cpus only nodes can be chosen
        ! cpu 36 is the best and with -j for siesta and transiesta?
        if(chs_2==1)then
        write(*,*)'2'
        write(40,'(a)')'#PBS -l select=2'
        i_chs_6_1='36'
        i_chs_6='72'
          goto 990
        endif

993     continue

        call userreadline(string, '(Integer) ==> ')
        read(string,*,iostat=chk)chs_4 
        if(chk/=0)then
        write(*,*)
        write(*,*)'Please input an integer'
        goto 993
        else
        write(i_chs_4,'(i2)')chs_4

        write(40,'(2a)')'#PBS -l select=',adjustl(trim(i_chs_4))

        if(chs_10.eq.2)then

        ! tbtrans parallel by k, k = (1,1,1)
        ! chs_10 for tbtrans different cores per node
        ! chs_6_1 cores per node, chs_4 nodes, chs_6 total cores

899     continue
        write(*,*)
        write(*,*) 'Please enter cores per node'
        !write(*, '(1x, a)', advance='no') '( 1 <-> 36 ) : '
        call userreadline(string, &
          '(Integer : 1 <-> 36 ) ==> ')
        read(string,*, iostat= chk) chs_6_1
        if (chk /= 0) then
          goto 899
        else
        if ( chs_6_1 .ge. 1 .and. chs_6_1 .le. 36 ) then
          chs_6=chs_6_1*chs_4
        else
          goto 899
        endif
        endif

        write(i_chs_6_1,'(i4)')chs_6_1
        write(i_chs_6,'(i4)')chs_6
        else
        chs_6_1=36
        chs_6=chs_6_1*chs_4
        write(i_chs_6_1,'(i4)')chs_6_1
        write(i_chs_6,'(i4)')chs_6
        endif

        endif

990     continue
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
        write(*,*)'Please input the name of task?'
        call userreadline(string, ' ==> ')
        read(string,*)chs_5
        write(40,'(2a)')'#PBS -N ',trim(chs_5)
        
        if(chs_2==2.or.chs_2==3)then
        write(40,'(a)')'#PBS -m be'
        write(40,'(a)')'#PBS -M zhang.qinqiang@rift.mech.tohoku.ac.jp'
        else
        endif

        write(40,'(a)')'cd ${PBS_O_WORKDIR}'
        write(40,*)' '

        !----------------------------- choose run siesta or transiesta
1100    continue
        select case(chs_10)

        case(1)
        write(*,*)''
        write(*,*)'You choose (1)SIESTA and TranSIESTA'
        write(*,*)''
        goto 1001

        case(2)
        write(*,*)''
        write(*,*)'You choose (2)TBTrans'
        write(*,*)''
        goto 1002

        case(3)
        write(*,*)''
        write(*,*)'You choose (3)LAMMPS'
        write(*,*)''
        goto 1003

        case(4)
        write(*,*)''
        write(*,*)'You choose (4)SIESTA'
        write(*,*)''
        goto 1004

        case(5)
        write(*,*)''
        write(*,*)'You choose (5)TranSIESTA'
        write(*,*)''
        goto 1005

        case(6)
        write(*,*)''
        write(*,*)'You choose (6)Denchar'
        write(*,*)''
        goto 1006

        endselect

        ! ------------------ for aprun all siesta -----------------
1001    continue
        ! ---- s file fully relaxed
        write(40,'(9a)')'aprun -n ',adjustl(trim(i_chs_6)),&
                ' -N ',adjustl(trim(i_chs_6_1)),' -j 1 ',&
        '/work/app/SIESTA/current/Obj/siesta < ',&
        ''//trim(filename1)//'.fdf',' > ',&
        ''//trim(filename1)//'.out'

        ! ---- tse file fully relaxed
        write(40,'(9a)')'aprun -n ',adjustl(trim(i_chs_6)),&
                ' -N ',adjustl(trim(i_chs_6_1)),' -j 1 ',&
        '/work/app/SIESTA/current/Obj/transiesta < ',&
        ''//trim(filename2)//'.fdf',' > ',&
        ''//trim(filename2)//'.out'

        ! ---- .xyz to a new .fdf and revise tss.fdf

        write(40,'(2a)')'xyz2fdff90 '//trim(filename1)//'.xyz',&
          ' '//trim(filename2)//'.xyz '//trim(filename3)//'.fdf'

        ! ---- tss
        write(40,'(9a)')'aprun -n ',adjustl(trim(i_chs_6)),&
                ' -N ',adjustl(trim(i_chs_6_1)),' -j 1 ',&
        '/work/app/SIESTA/current/Obj/transiesta < ',&
        ''//trim(filename3)//'.fdf',' > ',&
        ''//trim(filename3)//'.out'

        goto 1099

        ! ------------------ for aprun tbtrans -----------------
1002    continue

        if(nu_que_fil.eq.1)then

        ! ------------------ for mpirun tbtrans -----------------
          ! ---------- DP_002 and P_016 first run in same script
        if(chs_2==2.or.chs_2==1)then

          ! due to stack limit in CPU perhaps
          write(40,'(a)') 'ulimit -s unlimited'
          
        do i=2,nu_fil
        write(40,'(9a)')'aprun -n ',adjustl(trim(i_chs_6)),&
                ' -N ',adjustl(trim(i_chs_6_1)),' -j 1 ',&
        '/work/app/SIESTA/current/Util/TBTrans_rep/tbtrans < ',&
        ''//trim(filename(i))//'.fdf',' > ',&
        ''//trim(filename(i))//'.out'
        enddo
        
        ! ------------------ for mpirun tbtrans -----------------
        ! first gpu C_002
        elseif(chs_2==3)then

          ! due to stack limit in GPU for tbtrans
          write(40,'(a)') 'ulimit -s unlimited'

        do i=2,nu_fil
          write(40,'(9a)')'mpirun -np ',adjustl(trim(i_chs_6)),&
                  ' -ppn ',adjustl(trim(i_chs_6_1)),&
          ' -hostfile $PBS_NODEFILE ',&
        '/usr/local/app/SIESTA/current/Util/TBTrans_rep/tbtrans < ',&
          ''//trim(filename(i))//'.fdf',' > ',&
          ''//trim(filename(i))//'.out'
        enddo

        endif

        ! ############################## multi tbtran files
        elseif(nu_que_fil.ge.2)then

          if(chs_2==1)then
            ! first is normal, from second use loop

          nu_job_fil_lo = 2
          nu_job_fil_hi = 1 + chs_fil

          ! nu_job_fil_lo, nu_job_fil_hi write in submit file

          ! due to stack limit in CPU perhaps
          write(40,'(a)') 'ulimit -s unlimited'
          
        do i= nu_job_fil_lo, nu_job_fil_hi
        write(40,'(9a)')'aprun -n ',adjustl(trim(i_chs_6)),&
                ' -N ',adjustl(trim(i_chs_6_1)),' -j 1 ',&
        '/work/app/SIESTA/current/Util/TBTrans_rep/tbtrans < ',&
        ''//trim(filename(i))//'.fdf',' > ',&
        ''//trim(filename(i))//'.out'
        enddo
        
        ! because is too conplicated to rewrite this script
        ! first file is always run.sh
        ! start loop for multi queue file

        do i = 1, nu_que_fil -1
        ! total file will be total -1 excluded the run.sh

        nu_job_fil_lo = nu_job_fil_hi + 1
        nu_job_fil_hi = nu_job_fil_hi + chs_fil

        ! nu_job_fil_lo from 2
        if(nu_job_fil_hi.ge.nu_fil)then
          nu_job_fil_hi = nu_fil
        endif

        write(chr_nu_que_fil,'(i0)') i

        ! ### open file after choose queue type
        open(10000+i,file='run_'//trim(chr_nu_que_fil)//'.sh', &
          form='formatted', err=9999, & 
                status='unknown', access='sequential')
        write(10000+i,'(a)')'#!/bin/sh'
        ! ### open finished

        ! queue type
        write(10000+i,'(2a)')'#PBS -q ', trim(slt_2)
        
        ! wall time select ------------------------------------
        write(10000+i,'(a)')'#PBS -l walltime=00:30:00' 

        ! job nodes
        write(10000+i,'(a)')'#PBS -l select=2'

        ! job name
        write(10000+i,'(4a)')'#PBS -N ',trim(chs_5),'_',&
          trim(chr_nu_que_fil)

        ! go work directory not sure the meaning of it
        write(10000+i,'(a)')'cd ${PBS_O_WORKDIR}'
        write(10000+i,*)

          ! nu_job_fil_lo, nu_job_fil_hi write in submit file

          ! due to stack limit in CPU perhaps
          write(10000+i,'(a)') 'ulimit -s unlimited'

        do j = nu_job_fil_lo, nu_job_fil_hi
          
        write(10000+i,'(9a)')'aprun -n ',adjustl(trim(i_chs_6)),&
                ' -N ',adjustl(trim(i_chs_6_1)),' -j 1 ',&
        '/work/app/SIESTA/current/Util/TBTrans_rep/tbtrans < ',&
        ''//trim(filename(j))//'.fdf',' > ',&
        ''//trim(filename(j))//'.out'

        enddo

        write(*,'(3a)') ' File : run_',trim(chr_nu_que_fil), &
          '.sh created'

        enddo

        do i=1, nu_que_fil -1
        close(10000+i)
        enddo

        write(*,'(a)') ' File : run.sh created'

        write(*,*)
        write(*,'(a,i0)') ' Total tasks : ', nu_job_fil
        write(*,'(a,i0)') ' Total files : ', nu_que_fil
        write(*,'(a,i0)') &
          ' Tasks per file (Except the last file): ',chs_fil
        
        ! ############################## from now P_016

          elseif(chs_2==2)then
            ! first is normal, from second use loop

          nu_job_fil_lo = 2
          nu_job_fil_hi = 1 + chs_fil

          ! nu_job_fil_lo, nu_job_fil_hi write in submit file

          ! due to stack limit in CPU perhaps
          write(40,'(a)') 'ulimit -s unlimited'
          
        do i= nu_job_fil_lo, nu_job_fil_hi
        write(40,'(9a)')'aprun -n ',adjustl(trim(i_chs_6)),&
                ' -N ',adjustl(trim(i_chs_6_1)),' -j 1 ',&
        '/work/app/SIESTA/current/Util/TBTrans_rep/tbtrans < ',&
        ''//trim(filename(i))//'.fdf',' > ',&
        ''//trim(filename(i))//'.out'
        enddo
        
        ! because is too conplicated to rewrite this script
        ! first file is always run.sh
        ! start loop for multi queue file

        do i = 1, nu_que_fil -1
        ! total file will be total -1 excluded the run.sh

        nu_job_fil_lo = nu_job_fil_hi + 1
        nu_job_fil_hi = nu_job_fil_hi + chs_fil

        ! nu_job_fil_lo from 2
        if(nu_job_fil_hi.ge.nu_fil)then
          nu_job_fil_hi = nu_fil
        endif

        write(chr_nu_que_fil,'(i0)') i

        ! ### open file after choose queue type
        open(10000+i,file='run_'//trim(chr_nu_que_fil)//'.sh', &
          form='formatted', err=9999, & 
                status='unknown', access='sequential')
        write(10000+i,'(a)')'#!/bin/sh'
        ! ### open finished

        ! queue type
        write(10000+i,'(2a)')'#PBS -q ', trim(slt_2)
        
        ! wall time select ------------------------------------
        write(10000+i,'(3a)')'#PBS -l walltime=',trim(i_chs_3),':00:00'

        ! job nodes
        write(10000+i,'(2a)')'#PBS -l select=',adjustl(trim(i_chs_4))

        ! job name
        write(10000+i,'(4a)')'#PBS -N ',trim(chs_5),'_',&
          trim(chr_nu_que_fil)

        ! send email
        write(10000+i,'(a)')'#PBS -m be'
        write(10000+i,'(a)') & 
          '#PBS -M zhang.qinqiang@rift.mech.tohoku.ac.jp'

        ! go work directory not sure the meaning of it
        write(10000+i,'(a)')'cd ${PBS_O_WORKDIR}'
        write(10000+i,*)

          ! nu_job_fil_lo, nu_job_fil_hi write in submit file

          ! due to stack limit in CPU perhaps
          write(10000+i,'(a)') 'ulimit -s unlimited'

        do j = nu_job_fil_lo, nu_job_fil_hi
          
        write(10000+i,'(9a)')'aprun -n ',adjustl(trim(i_chs_6)),&
                ' -N ',adjustl(trim(i_chs_6_1)),' -j 1 ',&
        '/work/app/SIESTA/current/Util/TBTrans_rep/tbtrans < ',&
        ''//trim(filename(j))//'.fdf',' > ',&
        ''//trim(filename(j))//'.out'

        enddo

        write(*,'(3a)') ' File : run_',trim(chr_nu_que_fil), &
          '.sh created'

        enddo

        do i=1, nu_que_fil -1
        close(10000+i)
        enddo

        write(*,'(a)') ' File : run.sh created'

        write(*,*)
        write(*,'(a,i0)') ' Total tasks : ', nu_job_fil
        write(*,'(a,i0)') ' Total files : ', nu_que_fil
        write(*,'(a,i0)') &
          ' Tasks per file (Except the last file): ',chs_fil

        ! ############################## gpu multi tbtrans
          elseif(chs_2==3)then
            ! first is normal, from second use loop

          nu_job_fil_lo = 2
          nu_job_fil_hi = 1 + chs_fil

          ! nu_job_fil_lo, nu_job_fil_hi write in submit file

          ! due to stack limit in GPU for tbtrans
          write(40,'(a)') 'ulimit -s unlimited'
          
        do i= nu_job_fil_lo, nu_job_fil_hi
        write(40,'(9a)')'mpirun -np ',adjustl(trim(i_chs_6)),&
                ' -ppn ',adjustl(trim(i_chs_6_1)),&
          ' -hostfile $PBS_NODEFILE ',&
        '/usr/local/app/SIESTA/current/Util/TBTrans_rep/tbtrans < ',&
        ''//trim(filename(i))//'.fdf',' > ',&
        ''//trim(filename(i))//'.out'
        enddo
        
        ! because is too conplicated to rewrite this script
        ! first file is always run.sh
        ! start loop for multi queue file

        do i = 1, nu_que_fil -1
        ! total file will be total -1 excluded the run.sh

        nu_job_fil_lo = nu_job_fil_hi + 1
        nu_job_fil_hi = nu_job_fil_hi + chs_fil

        ! nu_job_fil_lo from 2
        if(nu_job_fil_hi.ge.nu_fil)then
          nu_job_fil_hi = nu_fil
        endif

        write(chr_nu_que_fil,'(i0)') i

        ! ### open file after choose queue type
        open(10000+i,&
          file='run-gpu_'//trim(chr_nu_que_fil)//'.sh', &
          form='formatted', err=9999, & 
                status='unknown', access='sequential')
        write(10000+i,'(a)')'#!/bin/sh'
        ! ### open finished

        ! queue type
        write(10000+i,'(2a)')'#PBS -q ', trim(slt_2)
        
        ! wall time select ------------------------------------
        write(10000+i,'(3a)')'#PBS -l walltime=',trim(i_chs_3),':00:00'

        ! job nodes
        write(10000+i,'(2a)')'#PBS -l select=',adjustl(trim(i_chs_4))

        ! job name
        write(10000+i,'(4a)')'#PBS -N ',trim(chs_5),'_',&
          trim(chr_nu_que_fil)

        ! send email
        write(10000+i,'(a)')'#PBS -m be'
        write(10000+i,'(a)') & 
          '#PBS -M zhang.qinqiang@rift.mech.tohoku.ac.jp'

        ! go work directory not sure the meaning of it
        write(10000+i,'(a)')'cd ${PBS_O_WORKDIR}'
        write(10000+i,*)

          ! nu_job_fil_lo, nu_job_fil_hi write in submit file

          ! due to stack limit in GPU for tbtrans
          write(10000+i,'(a)') 'ulimit -s unlimited'

        do j = nu_job_fil_lo, nu_job_fil_hi
          
        write(10000+i,'(9a)')'mpirun -np ',adjustl(trim(i_chs_6)),&
                ' -ppn ',adjustl(trim(i_chs_6_1)),&
          ' -hostfile $PBS_NODEFILE ',&
        '/usr/local/app/SIESTA/current/Util/TBTrans_rep/tbtrans < ',&
        ''//trim(filename(j))//'.fdf',' > ',&
        ''//trim(filename(j))//'.out'

        enddo

        write(*,'(3a)') ' File : run-gpu_',trim(chr_nu_que_fil), &
          '.sh created'

        enddo

        do i=1, nu_que_fil -1
        close(10000+i)
        enddo

        write(*,'(a)') ' File : run-gpu.sh created'

        write(*,*)
        write(*,'(a,i0)') ' Total tasks : ', nu_job_fil
        write(*,'(a,i0)') ' Total files : ', nu_que_fil
        write(*,'(a,i0)') &
          ' Tasks per file (Except the last file): ',chs_fil

          endif
        ! ############################## multi tbtran files

        endif

        goto 1099

        ! ------------------ for aprun lammps -----------------
1003    continue
        write(40,'(2a)')'aprun -n 72 -N 36 -j 1 ',& 
          '/work/app/LAMMPS/current/src/lmp_intel_omp < in.min'
        ! maybe future revise for a real another lammps run

        goto 1099

        ! ------------------ for aprun siesta -----------------
1004    continue
        if(chs_2==2.or.chs_2==1)then
        write(40,'(9a)')'aprun -n ',adjustl(trim(i_chs_6)),&
                ' -N ',adjustl(trim(i_chs_6_1)),' -j 1 ',&
        '/work/app/SIESTA/current/Obj/siesta < ',&
        ''//trim(filename1)//'.fdf',' > ',&
        ''//trim(filename1)//'.out'

        ! ------------------ for mpirun siesta -----------------
        elseif(chs_2==3)then
        write(40,'(9a)')'mpirun -np ',adjustl(trim(i_chs_6)),&
                ' -ppn ',adjustl(trim(i_chs_6_1)),&
                ' -hostfile $PBS_NODEFILE ',&
        '/usr/local/app/SIESTA/current/Obj/siesta < ',&
        ''//trim(filename1)//'.fdf',' > ',&
        ''//trim(filename1)//'.out'

        endif

        goto 1099

        ! ------------------ for aprun transiesta -----------------
        ! ---- transiesta original for the electrode
1005    continue

        ! ---- electrode run is siesta
        ! ---- individual transiesta run only for the scatting
        if(chs_2==2.or.chs_2==1)then
        write(40,'(9a)')'aprun -n ',adjustl(trim(i_chs_6)),&
                ' -N ',adjustl(trim(i_chs_6_1)),' -j 1 ',&
        '/work/app/SIESTA/current/Obj/transiesta < ',&
        ''//trim(filename2)//'.fdf',' > ',&
        ''//trim(filename2)//'.out'

        ! ------------------ for mpirun transiesta -----------------
        elseif(chs_2==3)then
        write(40,'(9a)')'mpirun -np ',adjustl(trim(i_chs_6)),&
                ' -ppn ',adjustl(trim(i_chs_6_1)),&
                ' -hostfile $PBS_NODEFILE ',&
        '/usr/local/app/SIESTA/current/Obj/transiesta < ',&
        ''//trim(filename2)//'.fdf',' > ',&
        ''//trim(filename2)//'.out'

        endif

        goto 1099

        ! ------------------ for aprun Denchar -----------------
1006    continue
        write(40,'(3a)')'aprun -n 72 -N 36 -j 1 ',& 
          '/work/app/SIESTA/current/Util/Denchar/Src/denchar < ',&
          'denchar.fdf > denchar.out'

        goto 1099

        ! ------------------ aprun part completed ---------------------

1099    continue
        write(*,*)' '

        if(nu_que_fil.ge.2)then
        write(*,*)'Shell scrip for CCMS created'
        else
        write(*,*)&
          'Shell scrip for CCMS created ==> run.sh or run-gpu.sh'
        endif
        write(*,*)' '
        write(*,*)&
          'You can execute "run" command for submitting task now'
        write(*,*)' '
        close(40)

        goto 999

9999    continue
        write(*,*)
        write(*,*)&
          'Error: Cannot open run.sh or run-gpu.sh'
        write(*,*)' '
        goto 999

999     continue
        deallocate(filename)
        stop

        ! -------- use function for check filename2 exist not
        contains
          !################# 3 #################
          logical function check_filename3(x)
            implicit none
            character(32), intent(in), optional :: x
            if(len_trim(x)==0)then
              check_filename3 = .false.
            else
              check_filename3 = .true.
            endif
            endfunction check_filename3
          !################# 2 #################
          logical function check_filename2(x)
            implicit none
            character(32), intent(in), optional :: x
            if(len_trim(x)==0)then
              check_filename2 = .false.
            else
              check_filename2 = .true.
            endif
          end function check_filename2

          !################# 1 #################
          ! use logical to check filename1 include .fdf or in.
          ! it is better to use integer function. maybe
          logical function check_filename1(x)
            implicit none
            integer L_C ! length of check
            character(32), intent(in) :: x
            character(32) a, b
            a='.fdf'
            b='in.'
            L_C=len_trim(x)-len_trim(a)
            if((x(L_C+1:L_C+len_trim(a))).eq.a)then
              check_filename1=.true. ! do siesta or transiesta with .fdf
            return
            elseif(index(trim(x), trim(b)).eq.1)then
              check_filename1=.false. ! do lammps with in.
            return
            else
              write(*,*)''
              write(*,*)&
                'Error ==> The input files are not *.fdf or in.*!!!'
              write(*,*)''
            goto 998
            endif
            
998         stop

            endfunction check_filename1

        endprogram crt4run_shf90
