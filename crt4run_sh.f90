        program crt4run_shf90
        implicit none
        integer i
        integer chk,chk_1!check input value
        integer chs_2,chs_3,chs_4,chs_6,chs_6_1 !choose ,chs_4_1        
        integer chs_10, chk_10 ! choose for transiesta or siesta
        integer nu_fil ! input .fdf files for tbtrans
        character(32) filename1 
        ! for first siesta or transiesta electrode
        character(32) filename2 ! for transiesta electrode or scatting
        character(32) filename3 ! for transiesta scatting
        character(32) tab ! for tab format
        character(32) chs_1,chs_5 !choose
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
        write(*,*)'(version-2.2 //May/12/2019/)'
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
        else
        endif

        open(40,file='run.sh', form='formatted',& 
                status='unknown', access='sequential')

        !###### denchar for a quick run ######
        if(chs_10==6)then
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

        write(40,'(a)')'#!/bin/sh'
        
        ! queue type select ---------------------------------------
        write(*,*)' '
        write(*,*)'Which queue do you want to choose (This time only 2)'
        write(*,*)'(1) DP_002,  (2) P_016'
        ! should use logical to write different selection ==> combine
        ! two parts

994     continue

        read(5,*,iostat=chk)chs_2
        if(chk/=0)then
        write(*,*)' '
        write(*,*)'Please input an integer'
        goto 994
        else
        select case(chs_2)
        case(1)
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
        write(*,*)' '
        write(*,*)'You choose (2) P_016'
        slt_2='P_016'
        write(40,'(2a)')'#PBS -q ', trim(slt_2)
        
        ! wall time select ------------------------------------
        write(*,*)' '
        write(*,*)'Please input walltime, how many hours?',&
        ' (72 hours limited, only 2 numbers limited)'
        read(5,*,iostat=chk_1)chs_3
        if(chk_1/=0)then
        write(*,*)'Please input an integer'
        else
        write(i_chs_3,'(i2.2)')chs_3 ! i2.2 fill 0 automatically
        write(40,'(3a)')'#PBS -l walltime=',trim(i_chs_3),':00:00'
        endif

        case default
        write(*,*)' '
        write(*,*)'Please choose 1 or 2 for the queue type'
        goto 994
        endselect
        endif

        write(*,*)' '
        write(*,*)'Please input how many nodes you want?'
        write(*,*)'Limit: DP_002 (2), P_016 (16)'
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

        read(5,*,iostat=chk)chs_4 
        if(chk/=0)then
        write(*,*)' '
        write(*,*)'Please input an integer'
        goto 993
        else
        write(i_chs_4,'(i2)')chs_4

        write(40,'(2a)')'#PBS -l select=',adjustl(trim(i_chs_4))

        chs_6_1=36
        chs_6=chs_6_1*chs_4
        write(i_chs_6_1,'(i4)')chs_6_1
        write(i_chs_6,'(i4)')chs_6
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
        read(5,*)chs_5
        write(40,'(2a)')'#PBS -N ',trim(chs_5)
        
        if(chs_2==2)then
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
        goto 1001

        case(2)
        write(*,*)''
        write(*,*)'You choose (2)TBTrans'
        goto 1002

        case(3)
        write(*,*)''
        write(*,*)'You choose (3)LAMMPS'
        goto 1003

        case(4)
        write(*,*)''
        write(*,*)'You choose (4)SIESTA'
        goto 1004

        case(5)
        write(*,*)''
        write(*,*)'You choose (5)TranSIESTA'
        goto 1005

        case(6)
        write(*,*)''
        write(*,*)'You choose (6)Denchar'
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
        do i=2,nu_fil
        write(40,'(9a)')'aprun -n ',adjustl(trim(i_chs_6)),&
                ' -N ',adjustl(trim(i_chs_6_1)),' -j 1 ',&
        '/work/app/SIESTA/current/Util/TBTrans_rep/tbtrans < ',&
        ''//trim(filename(i))//'.fdf',' > ',&
        ''//trim(filename(i))//'.out'
        enddo

        goto 1099

        ! ------------------ for aprun lammps -----------------
1003    continue
        write(40,'(2a)')'aprun -n 72 -N 36 -j 1 ',& 
          '/work/app/LAMMPS/current/src/lmp_intel_omp < in.min'
        ! maybe future revise for a real another lammps run

        goto 1099

        ! ------------------ for aprun siesta -----------------
1004    continue
        write(40,'(9a)')'aprun -n ',adjustl(trim(i_chs_6)),&
                ' -N ',adjustl(trim(i_chs_6_1)),' -j 1 ',&
        '/work/app/SIESTA/current/Obj/siesta < ',&
        ''//trim(filename1)//'.fdf',' > ',&
        ''//trim(filename1)//'.out'

        goto 1099

        ! ------------------ for aprun transiesta -----------------
        ! ---- transiesta original for the electrode
1005    continue

        ! ---- electrode run is siesta
        ! ---- individual transiesta run only for the scatting
        write(40,'(9a)')'aprun -n ',adjustl(trim(i_chs_6)),&
                ' -N ',adjustl(trim(i_chs_6_1)),' -j 1 ',&
        '/work/app/SIESTA/current/Obj/transiesta < ',&
        ''//trim(filename2)//'.fdf',' > ',&
        ''//trim(filename2)//'.out'

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
        write(*,*)'Shell scrip for CCMS created ==> run.sh'
        write(*,*)' '
        write(*,*)&
          'You can execute "run" command for submitting task now'
        write(*,*)' '
        close(40)

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
