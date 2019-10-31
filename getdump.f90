        program getdump2fdf
        
        implicit none
        
        integer :: num, num_data
        integer :: i, j, step, at, get_num, nx, ny, nz, format_co
        integer :: numv, maxid, getdata_num,order
        integer :: tempid 
        integer :: check
        integer :: num_ele_low,num_ele_hi
        integer status, rename
        integer posre_nam_chk, posre_chs_chk ! renew a position.fdf name
        integer, allocatable :: id(:),sortid(:)
        integer, pointer :: num_ele(:)
        integer, allocatable :: nw_id(:)! new add change number
        real :: cell_lo_re, cell_hi_re
        real,allocatable :: cellx(:), celly(:), cellz(:)
        real,allocatable :: posx2(:,:), posy2(:,:), posz2(:,:)
        real,allocatable :: posx1(:), posy1(:), posz1(:)
        real, allocatable :: cell_lo(:,:), cell_hi(:,:)
        real, allocatable :: val(:,:,:)
        character(32) posre_nam, posre_chs ! renew a position.fdf name
        character(128) :: filename, dummy
        character(36), allocatable :: sortchar(:)
        character(64) :: tempchar
        character(len=:), allocatable :: name_ele(:)
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

        write(*,*)
        write(*,'(1x,2a)')'Running... getdump file from dump.GNR... ',&
                '(Version --2.12 beta//May/17/2019//)'
        write(*,*)
        write(*,*)'Include manual option: -h '
        write(*,*)'By using ==> car2Lammps -h '
        write(*,*)'To see how to use it...'

        ! ###### help options ######

        nna=0
        i=1
        lna=2

        narg=command_argument_count()
        if(narg==0)then
          write(*,*)
          write(*,*)'call manual: -h'
          write(*,*)
          goto 99
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
                goto 99
              endif
              enddo
              nna=nna+1
            else
            endif
        enddo
        endif

        if(narg==nna)then
          write(*,*)
          write(*,*)'------'
          write(*,*)'No available options'
          write(*,*)'call manual: -h'
          write(*,*)
          goto 99
        else
        endif

        ! ###### help options finished ######
        ! ###### check variable has .car ######

        do i=1,narg
        call get_command_argument(i,chk_filename)
        !lchk=len_trim(chk_filename)
        ! keep for future upgrade
        if(chk_filename=='dump.GNR')then
          filename=chk_filename
          goto 97
        endif
        enddo

        ! ###### check variable has .car finished ######

97      continue

        open(10,file=filename, status='old',&
                err=98, form='formatted', access='sequential')
        open(20,file='tempdata.dat', status='unknown', &
                form='formatted', access='sequential') 
        open(30,file='tempdata2.dat', status='unknown', &
                form='formatted', access='sequential') 
        open(40,file='getdata.lammps', status='unknown', &
                form='formatted', access='sequential') 
        open(50,file='position.fdf', status='unknown', &
                form='formatted', access='sequential') 
               
        maxid = 1
        num_data = 0
        step = 1
        num = 1 
        i=1
        j=1
        
        read(10,'(A128)')dummy
        read(10,'(i6)')step
        read(10,'(A128)')dummy
        read(10,'(i6)')num
        read(10,'(A128)')dummy

                allocate(id(num))
                allocate(num_ele(num))

        write(*,*)' '
        write (*,'(1x,2a)')'Please input how many variables (integer)',&
          ' in one row?'
        write(*,*)' '
        write (*,'(1x,3a)')'(8 variables in one row at dump.GNR file,',&
        ' or "0" to run Lammps2fdf directly, "-1" to quit the job,',&
        ' "-2" to sort Z axis for Transiesta)' 

49      continue

        read (5,*,iostat=check)order
!----- need to be changed use ascii for recognize characters
        if(check/=0)then
        write(*,*)''
        write(*,'(1x,2a)')&
        'Please input "3" to "8" to choose variables ',&
        '(or "0" to run Lammps2fdf, "-1" to quit the job',&
        ' "-2" to sort Z axis for Transiesta)' 
        write(*,*)order
        goto 49
        else
                if(order.eq.0)then
        write(*,*)' '
        write(*,*)'Please be sure you already run this program before'
        goto 300
                
                elseif(order.eq.-1)then
        write(*,*)' '
                write(*,*) 'You quit the job'
        write(*,*)' '
        goto 500
                
                elseif(order.eq.-2)then
        write(*,*)' '
                write(*,*) 'Sort the value of Z axis for Transiesta'
        write(*,*)' '
        goto 700

                elseif(order.lt.-1)then
        write(*,*)' '
                write(*,*) 'no data selected, try again'
        goto 49
        
                elseif (order.lt.3.and.order.gt.0) then
        write(*,*)' '
                write(*,*) 'no data selected, try again'
        goto 49
                elseif (order.gt.8)then
        write(*,*)' '
                write(*,*) 'over the data selected range, try again'
        goto 49
                else
        numv = order-2
                endif
        
        endif

        rewind(10)

61      continue
        read(10,'(A128)',end=100)dummy
        read(10,'(i6)')step
        read(10,'(A128)')dummy
        read(10,'(i6)')num
        read(10,'(A128)')dummy


        num_data = num_data+1
        write(20,'(A70,i2)')'Tempdata for getdump to obtain &
                        getdata.lammps. Data of number ',num_data
        
        do i=1,maxid
                num_ele(i)=0
        end do

        do i=1, 3
                allocate(cell_lo(i,num_data),cell_hi(i,num_data))
                read(10,*)cell_lo(i,num_data),cell_hi(i,num_data)
         !       write(*,*)sizeof(cell_lo),sizeof(cell_hi)
                write(20,*)cell_lo(i,num_data), cell_hi(i,num_data)
                deallocate(cell_lo,cell_hi) 
        end do
        
        read(10,'(A128)')dummy

                do i=1, num
                allocate(val(numv,i,num_data))
                read(10,*)at,id(i),(val(j,i,num_data),j=1,numv)
                write(20,'(1x,i7,1x,i2,3(1x,f11.6))')&
                        at,id(i),(val(j,i,num_data),j=1,numv)
                        if (i==1) then 
                                maxid=id(i)
                        else if (id(i) >= maxid) then
                                        maxid=id(i)
                        else
                        end if

                num_ele(id(i))=num_ele(id(i))+1
                deallocate(val) 
        end do
                
        go to 61
        
100     continue

        rewind(20) 

        i=1
        j=1

        write(*,*)' '
        write(*,*)'Which number of data do you want to get'
        write(*,*)' '
        write(*,'(1x,a16,1x,i2)')'Number of data =',num_data
        write(*,'(a2,1x,i2,1x,a18)')' (',num_data,'is the final data)'
81      continue
        read(5,*,iostat=check) getdata_num
        if(check>0)then
        write(*,*)'Please input the correct number'
                goto 81
        else
                if (num_data<getdata_num) then
        write(*,*)' '
        write(*,*)'too large value of function number, &
                        please input again'
                goto 81
                elseif (getdata_num<1)then
        write(*,*)' '
        write(*,*)'too small value of function number, &
                        please input again'
                goto 81
                else
                endif
        endif
        write(*,*)' '
        write(*,*)'Format of coordination of atom position'
        write(*,*)' '
        write(*,*)'(1) relative coordinates (.car type) &
                        (2) absolute coordinates (.xyz type)'
70      continue

        read(5,*,iostat=check) format_co
        if(check>0)then
                write(*,*)'Please input the correct number'
        goto 70
        else
        do while (format_co/=1.and.format_co/=2)
        write(*,*)' '
        write(*,*)'wrong format number, please input 1 or 2'
        goto 70
        end do
        endif
        write(*,*)' '
        write(*,*)'Where is the position of column x in the output data'

71      continue
        
        read(5,*,iostat=check) order
        if(check>0)then
                write(*,*)'Please input the correct number'
        goto 71
        else
                if (order.lt.3) then
                write(*,*)' '
                write(*,*) 'too small of x value, try again'
                goto 71
                elseif (order.gt.8)then
                write(*,*)' '
                write(*,*) 'too large of x value, try again'
                goto 71
                else
                endif
        nx = order - 2
        endif

        write(*,*)' '
        write(*,*)'Where is the position of column y in the output data'
72      continue
        read(5,*,iostat=check) order
        if(check>0)then
                write(*,*)'Please input the correct number'
        goto 72
        else
                if (order.lt.3) then
                write(*,*)' '
                write(*,*) 'too small of y value, try again'

                goto 72
                elseif (order.gt.8)then
                write(*,*)' '
                write(*,*) 'too large of y value, try again'

                goto 72
            elseif(order.eq.nx+2)then
                write(*,*)' '
                write(*,*) 'You already specified this value, ',&
                'please enter another one'

                goto 72
                else
                endif
        ny = order - 2
        endif

        write(*,*)' '
        write(*,*)'Where is the position of column z in the output data'
73      continue
        read(5,*,iostat=check) order
        if(check>0)then
                write(*,*)'Please input the correct number'
        goto 73
        else
                if (order.lt.3) then
                write(*,*)' '
                write(*,*) 'too small of z value, try again'
                goto 73
                elseif (order.gt.8)then
                write(*,*)' '
                write(*,*) 'too large of z value, try again'
                goto 73
            elseif(order.eq.nx+2.or.order.eq.ny+2)then
                write(*,*)' '
                write(*,*) 'You already specified this value, ',&
                'please enter another one'

                goto 73
                else
                endif
        nz = order - 2
        end if
        
        get_num = 1
        allocate(cellx(get_num),celly(get_num),cellz(get_num))

80      continue
        read(20,*,end=200)dummy
        allocate(cell_lo(i,get_num),cell_hi(i,get_num))
        do i=1,3
                read(20,*)cell_lo(i,get_num),cell_hi(i,get_num)
                if (i==nx) then
        cellx(get_num) = abs(cell_hi(nx,get_num)-cell_lo(nx,get_num))
                else if(i==ny) then
        celly(get_num) = abs(cell_hi(ny,get_num)-cell_lo(ny,get_num))
                else if(i==nz) then
        cellz(get_num) = abs(cell_hi(nz,get_num)-cell_lo(nz,get_num))
                else
                write(*,*)cell_lo(i,get_num),cell_hi(i,get_num)
                end if
        enddo
        
        if (getdata_num==get_num) then
                write(30,*)'# Position data (not sorted)'
                write(30,'(i8,1x,a5)')num,'atoms'
                write(30,'(i3,1x,a10,1x,3(i6))')maxid, 'atom types',&
                (num_ele(j),j=1,maxid)
                write(30,'(f11.7,1x,f11.7,1x,a7)')&
                  cell_lo(nx,get_num), cell_hi(nx,get_num),'xlo xhi'
                write(30,'(f11.7,1x,f11.7,1x,a7)')&
                  cell_lo(ny,get_num), cell_hi(ny,get_num),'ylo yhi'
                write(30,'(f11.7,1x,f11.7,1x,a7)')&
                  cell_lo(nz,get_num), cell_hi(nz,get_num),'zlo zhi'
                write(30,*)' '
                write(30,*)'Atoms'
                write(30,*)' '
        else 
        endif

        do i=1,num
                allocate(val(numv,i,get_num))
                !unefficient allocate here, weird
                allocate(posx2(i,get_num),posy2(i,get_num),&
                                posz2(i,get_num))
                read(20,'(1x,i7,1x,i2,3(1x,f11.6))')&
                        at,id(i),(val(j,i,get_num),j=1,numv)
                posx2(i,get_num) = val(nx,i,get_num)
                posy2(i,get_num) = val(ny,i,get_num)
                posz2(i,get_num) = val(nz,i,get_num)
                
                if (format_co==2.and.getdata_num==get_num) then
                posx2(i,get_num)=val(nx,i,get_num)*cellx(get_num)+&
                        cell_lo(nx,get_num)
                posy2(i,get_num)=val(ny,i,get_num)*celly(get_num)+&
                        cell_lo(ny,get_num)
                posz2(i,get_num)=val(nz,i,get_num)*cellz(get_num)+&
                        cell_lo(nz,get_num)
                        write(30,'(1x,i7,1x,i2,3(1x,f11.6))')&
                        at, id(i), posx2(i,getdata_num),&
                        posy2(i,getdata_num),posz2(i,getdata_num)
                else if (format_co==1.and.getdata_num==get_num) then
                        write(30,'(1x,i7,1x,i2,3(1x,f11.6))')&
                        at, id(i), val(nx,i,getdata_num),&
                        val(ny,i,getdata_num),val(nz,i,getdata_num)
                else
                endif

                deallocate(posx2,posy2,posz2)
                deallocate(val) 
        enddo
                
        deallocate(cell_lo,cell_hi) 
        get_num=get_num+1  
        go to 80

200     continue

        write(*,*)' '
        write(*,*)'size of supercell'
        write(*,*)'x =',cellx(getdata_num)
        write(*,*)'y =',celly(getdata_num)
        write(*,*)'z =',cellz(getdata_num)
        write(*,*)' '
        write(*,'(1x,a16,1x,i2)')'Number of data =',num_data
        write(*,*) 'number of obtained data =',getdata_num
        write(*,*)' '
        write(*,*) "total number of atoms =", num
        write(*,*)'total types of atoms =',maxid
        do i=1,maxid
                write(*,'(1x,a8,1x,i2,1x,a1,1x,i6)')'type (',i,')&
                        element', num_ele(i) 
        enddo        
        
        close(20,status='delete')
        rewind(30)

        read(30,'(a16)')dummy
        write(40,'(a16)')dummy
        read(30,'(i8,1x,a5)')num, dummy
        write(40,'(i8,1x,a5)')num, dummy
        read(30,'(i3,1x,a29)')maxid, dummy
        write(40,'(i3,1x,a29)')maxid, dummy
        read(30,'(f11.7,1x,f11.7,1x,a7)')&
                cell_lo_re, cell_hi_re, dummy
        write(40,'(f11.7,1x,f11.7,1x,a7)')&
                cell_lo_re, cell_hi_re, dummy
        read(30,'(f11.7,1x,f11.7,1x,a7)')&
                cell_lo_re, cell_hi_re, dummy
        write(40,'(f11.7,1x,f11.7,1x,a7)')&
                cell_lo_re, cell_hi_re, dummy
        read(30,'(f11.7,1x,f11.7,1x,a7)')&
                cell_lo_re, cell_hi_re, dummy
        write(40,'(f11.7,1x,f11.7,1x,a7)')&
                cell_lo_re, cell_hi_re, dummy
        read(30,'(a16)')dummy
        write(40,'(a16)')dummy
        read(30,'(a16)')dummy
        write(40,'(a16)')dummy
        read(30,'(a16)')dummy
        write(40,'(a16)')dummy
        
        allocate(sortid(num),sortchar(num))
        do i=1,num
        read(30,'(9x,i2,a36)')sortid(i),sortchar(i)
        enddo
         

        do i=1,num
        do j=1,num
                if(j>=2.and.j<=num&
                .and.sortid(j-1)>sortid(j))then
                tempid=sortid(j)
                sortid(j)=sortid(j-1)
                sortid(j-1)=tempid
                tempchar=sortchar(j)
                sortchar(j)=sortchar(j-1)
                sortchar(j-1)=tempchar
        else
        endif
        enddo
        enddo

        do i=1,num
        write(40,'(1x,i7,1x,i2,a36)')i,sortid(i),sortchar(i)
        enddo
        
        deallocate(sortid,sortchar)
        deallocate(cellx,celly,cellz)
              
        write(*,*)' '
        write(*,*)'file completed...'
        write(*,*)'getdata.lammps created...'
        write(*,*)' '
        write(*,*)'Running...   Outputing the data to position.fdf'
        write(*,*)' '

        goto 400

300     continue

        rewind(40)
        
        write(*,*)' '
        write(*,*)'Running...   Outputing the data to position.fdf'
        write(*,*)' '

                read(40,'(a)')dummy
                read(40,'(i8,1x,a5)')num, dummy
                read(40,'(i3,1x,a10,1x,3(i6))')maxid, dummy,&
                (num_ele(j),j=1,maxid)

400     continue

        rewind(40)

        allocate(character(32)::name_ele(maxid)) 
        do i=1,maxid
        write(*,*)''
        write(*,'(a,i0,a,i0)')'Please input the label of element '&
                ,i,', Amount = ',num_ele(i)
        read(5,*)name_ele(i)
        !-------------- for transiesta get unknown string length
        if(i==1)then
        len_dump=sizeof(trim(name_ele(i)))
        elseif(len_trim(name_ele(i))>len_dump)then
          len_dump=len_trim(name_ele(i))
        else
        endif
        enddo
        !len_dump is fixed after this

        do i=1,9
        read(40,'(a)')dummy
        enddo
        num_ele_low=1
        num_ele_hi=0
        write(50,*)'%block AtomicCoordinatesAndAtomicSpecies' 
        do i=1,maxid
                num_ele_hi=num_ele_hi+num_ele(i)
                do j=num_ele_low,num_ele_hi

        read(40,'(9x,i2,a36)')id(j),dummy
        write(50,'(a36,1x,i2,1x,a<len_dump>)')dummy,id(j),name_ele(i)
                enddo 
        num_ele_low=num_ele_low+num_ele(i)
        enddo
        write(50,*)'%endblock AtomicCoordinatesAndAtomicSpecies' 

        write(*,*)' '
        write(*,*)'Outputing completed'
        write(*,*)'position.fdf created...'
        write(*,*)' '

        !------ change atom number in position.fdf
        write(*,*)'------------ Information ------------'
        write(*,*)'This part is for corresponding the atom number ',&
        'between the s_position.fdf and tse_position.fdf'
        write(*,*)'-------------------------------------'
        write(*,*)'Do you want to change the number of atoms?'
        write(*,*)'Please choose y for yes, n for no'
801     continue
        read(5,*,iostat=check)chs
        if(check/=0)then
          write(*,*)''
          write(*,*)'Please choose y for yes, n for no'
          goto 801
        else
          select case(chs)
          case('y','Y')
            goto 803
          case('n','N')
            write(*,*)''
            write(*,*)"Don't want to change... Next step"
            write(*,*)''
            goto 802
          case default
            write(*,*)''
            write(*,*)'Please choose y for yes, n for no'
            goto 801
          endselect
        endif

803     continue
        rewind(50)
        open(60,file='tmp_position.fdf', status='unknown', &
                form='formatted', access='sequential') 

        allocate(nw_id(maxid))
        do i=1, maxid
        write(*,*)''
        write(*,'(2a)')&
        'Please enter the number of ',trim(name_ele(i))
        write(*,'(2a)')'Less than two characters ',&
        '(First two will be counted)'
804     continue
        read(*,'(i2)',iostat=check)nw_id(i) ! new id number integer
        if(check/=0)then
          write(*,*)''
          write(*,*)'Please enter the number again'
          goto 804
        else
        endif
        enddo

        read(50,'(a)')dummy
        write(60,'(a)')dummy

        num_ele_low=1
        num_ele_hi=0
        do i=1,maxid
          num_ele_hi=num_ele_hi+num_ele(i)
            do j=num_ele_low,num_ele_hi
              read(50,'(a36)')dummy
              write(60,'(a36,1x,i2,1x,(a))')dummy,nw_id(i),name_ele(i)
            enddo 
        num_ele_low=num_ele_low+num_ele(i)
        enddo
        read(50,'(a)')dummy
        write(60,'(a)')dummy
        deallocate(nw_id)
        close(60)
        
        status=rename('tmp_position.fdf','position.fdf')


        !-----------------renewed part of renew a position.fdf name
        write(*,*)''
        write(*,*)'###### Information for selection ######'
        write(*,*)'Do you want to rename the position.fdf file?'
        write(*,*)'Please enter y for yes, n for no'
        
830     continue   
        read(*,*,iostat=posre_chs_chk)posre_chs
        if(posre_chs_chk/=0)then
          write(*,*)''
          write(*,*)'Please enter y for yes, n for no'
          goto 830
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
840     continue
            read(*,*,iostat=posre_nam_chk)posre_nam
              if(posre_nam_chk/=0)then
                write(*,*)''
            write(*,*)'Please choose the correct number below'
            write(*,*)'(1) s_position.fdf ==> For SIESTA (Device)'
            write(*,*)&
              '(2) tse_position.fdf ==> For TranSIESTA (Electrode)'
            write(*,*)&
              '(3) tss_position.fdf ==> For TranSIESTA (Scattering)'
                goto 840

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
            goto 802
              case('2')
                write(*,*)''
                write(*,*)'You choose (2) tse_position.fdf'
                status=rename('position.fdf','tse_position.fdf')
                status=rename('getdata.lammps','tse_getdata.lammps')
            goto 802
              case('3')
                write(*,*)''
                write(*,*)'You choose (3) tss_position.fdf'
                status=rename('position.fdf','tss_position.fdf')
                status=rename('getdata.lammps','tss_getdata.lammps')
            goto 802
          case default
                write(*,*)''
            write(*,*)'Please choose the correct number below'
            write(*,*)'(1) s_position.fdf ==> For SIESTA (Device)'
            write(*,*)&
              '(2) tse_position.fdf ==> For TranSIESTA (Electrode)'
            write(*,*)&
              '(3) tss_position.fdf ==> For TranSIESTA (Scattering)'
                goto 840
              endselect

              endif
          case('n','N')
            goto 802
          case default
            write(*,*)''
            write(*,*)'Please enter y for yes, n for no'
            goto 830
          endselect
        endif

        !------ change finishied
802     continue

        deallocate(name_ele)       

!------------------------------sort z for transiesta----
        
        write(*,*)''
        write(*,*)'-------------- Information ---------------'
        write(*,*)'Do you want to sort Z axis for Transiesta?'
        write(*,*)'Bubble sort, smallest to biggest!'
        write(*,*)'y for yes, n for no'

702     continue
        
        read(5,*,iostat=check)chs
        if(check/=0)then
        write(*,*)'Please choose y for yes, n for no'
        goto 702
        else
        select case (chs)
          case('y','Y')
        write(*,*)' '
            goto 701
          case('n','N')
        write(*,*)' '
        write(*,*)'Program finished...'
        write(*,*)' '
            goto 500
          case default
        write(*,*)' '
        write(*,*)'Please choose y for yes, n for no'
            goto 702
        endselect
        endif


700     continue
        len_dump=1
        ! don't know how but have to?
        read(50,*)dummy
        do i=1,num
        read(50,'(a40,a)')dummy,len_temp
        if(len_trim(len_temp)>=len_dump)then
          len_dump=len_trim(len_temp)
        else
        endif
        enddo

701     continue

        rewind(50)

        len_dump2=len_dump+4 ! we have 4 variables not calculated
                             ! in the first len_dump
        allocate(pos_x(num),pos_y(num),pos_z(num))
        allocate(character(len=len_dump2)::dump(num))
        allocate(character(len=len_dump2)::temp_dump)

        read(50,*)dummy
        
        do i=1,num
        read(50,'(3(1x,f11.6),a<len_dump2>)')&
          pos_x(i),pos_y(i),pos_z(i),dump(i)
        enddo

        read(50,*)dummy

        !-------- sort z axis with bubble sort
        !----- no module include in this program
      lo=1
      hi=num-1
      do j=lo,hi
      l=0
      do i=lo,hi
      if(pos_z(i)>pos_z(i+1))then
      !------------ swap z
      temp_real=pos_z(i)
      pos_z(i)=pos_z(i+1)
      pos_z(i+1)=temp_real
      !------------ swap x
      temp_real=pos_x(i)
      pos_x(i)=pos_x(i+1)
      pos_x(i+1)=temp_real
      !------------ swap y
      temp_real=pos_y(i)
      pos_y(i)=pos_y(i+1)
      pos_y(i+1)=temp_real
      !------------ swap dump
      temp_dump=dump(i)
      dump(i)=dump(i+1)
      dump(i+1)=temp_dump
      !---------------------------------------
      l=l+1
      else
      endif
      enddo
      if(l.eq.0)exit
      hi=hi-1
      enddo

        !---------------- overwrite position.fdf
        open(60,file='position.fdf', status='replace',&
                form='formatted', access='sequential')

        write(60,*)'%block AtomicCoordinatesAndAtomicSpecies' 
        do i=1,num
        write(60,'(3(1x,f11.6),a<len_dump2>)')&
          pos_x(i),pos_y(i),pos_z(i),dump(i)
        enddo
        write(60,*)'%endblock AtomicCoordinatesAndAtomicSpecies' 

        close(60)
        !--------------------------------------

        write(*,*)'The value of Z axis sorted!'
        write(*,*)'position.fdf file has been overwritten!'
        write(*,*)' '

        deallocate(pos_x, pos_y, pos_z)
        deallocate(dump)
        deallocate(temp_dump)

500     continue
        
        deallocate(id)
        deallocate(num_ele)

        close(10)
        close(30,status='delete')
        close(40)
        close(50)

        goto 99

98      continue
        write(*,*)
        write(*,*)'Error: Can not open a dump.GNR file.'
        write(*,*)
        goto 99

99   stop

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

        end program getdump2fdf
