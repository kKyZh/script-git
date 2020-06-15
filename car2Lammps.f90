        program car2Lammpsf90

        implicit none
        integer :: i, j, k, l, m, n, num, maxa,h,temp1,temp2
        integer :: atom,ele,sort_max
        integer :: num_ele_low,num_ele_hi
        integer, allocatable :: num_ele(:)
        integer, allocatable :: ele_type(:)
        real :: temp_ele
        real, allocatable ::  cell(:), tpos(:,:)
        character(10) :: mass_one,choose_mass
        character(10) :: mass_multi
        character(3) :: aa
        character(3) :: one_ele
        character(3) :: choose_sort
        character(64) :: dummy, filename,dummy2
        character(len=:),allocatable :: chs_el(:)
        character(len=:),allocatable :: ma_mu(:),chs_ma(:)
        character(len=:),allocatable :: element(:),comp_ele(:)
        character(len=:),allocatable :: ele_multi(:)
        character(len=:),allocatable :: check_atom(:)
        logical chs_sort
        !------------------- divide z axis
        integer changetolable
        integer chk_div,chk_div_1,chk_div_2
        integer rename, status! return a output for status
        integer, allocatable :: id(:)
        real range_set
        real, allocatable :: tpos_z(:)
        character(1) chs_div
        character(16) from/'datanew.lammps'/,to/'data.lammps'/
        !rename from terminal function
        !------------------ rename after divide z axis
        character(16)mass_rena,chs_rena,el_rena,rena
        !----------------------------------
        integer lchk ! length of check
        integer nna, narg, lna, larg ! no correct name & number of
        ! argument & length of name & length of argument
        character(32) na ! options variable: name
        character(64) :: chk_filename ! check name is *.car
        !----------------------------------

        write(*,*)
        write(*,'(1x,2a)')'Running... car2Lammps to data.lammps',&
                '(Version --2.1 beta//Apr./17/2019//)'
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
        lchk=len_trim(chk_filename)
        if(chk_filename(lchk-3:lchk)=='.car')then
          filename=chk_filename
          goto 97
        endif
        enddo

        ! ###### check variable has .car finished ######

97      continue
        open(10,file=filename, form='formatted', status='old',& 
                err=98, access='sequential')
        open(20,file='tempforcar2lammps.dat', form='formatted',& 
                status='unknown', access='sequential')
        open(30,file='tempforcar2lammps2.dat', form='formatted', &
                status='unknown',access='sequential')
        open(40,file='data.lammps', form='formatted', &
                status='unknown',access='sequential')

        write(*,*)' '
        write(*,*)'Running... Preparing the file'
        write(*,*)'Running... Quick checking the data'
        write(*,*)'Please wait for a while...'

        do i=1, 4
          read(10,'(A64)') dummy
        enddo

        allocate(cell(6))

        read(10,'(3X,6(F10.4))')(cell(i),i=1,6)
        write(20,'(3X,6(F10.4))')(cell(i),i=1,6)
        
        num=1
     
50      continue
        
         read(10,'(a3,2x,a45,21x,a2)')&
                aa,dummy2,dummy
        if(aa/='end')then

        write(20,'(a2,2x,i6,a45)')&
                dummy,num,dummy2
        num=num+1
        goto 50
        elseif(aa=='end')then
        num=num-1
        goto 51
        else
        endif
      
51      continue        !quick check for lammps or siesta
        
        rewind(20)

        read(20,'(a)')dummy

        allocate(character(2)::check_atom(num))

        do i=1,num
        read(20,'(a2)')check_atom(i)
        if(i>=2.and.check_atom(i)/=check_atom(i-1))then
        goto 52
        else
        endif
        enddo

        write(*,*)'Running... Quick checking completed'
        write(*,*)' '
        write(*,*)'You have only one type of atom!'
        write(*,*)' '
        write(*,'(a7,2x,a2,2x,a9,2x,i6)')&
        'Atom = ',check_atom(1),'Amount = ',num
        one_ele=check_atom(1)

        deallocate(check_atom)

        write(*,*)' '
        write(*,*)'Do you really want to sort the position of data?'
        write(*,*)'y (Yes), n (No), q (Quit the job)'

56      continue

        read(5,*)choose_sort
        write(*,*)''

        select case (choose_sort)
                case ('y','Y')
                  chs_sort=.true.
                goto 53
                case ('n','N')
                  chs_sort=.false.
                goto 53
                case ('q','Q')
        write(*,*)'You Quit the job'
                goto 55
                case ('a':'m','o':'p')
        write(*,*)'Please input y or n for sorting the position or not'
                goto 56
                case ('r':'x','z')
        write(*,*)'Please input y or n for sorting the position or not'
                goto 56
                case default
        write(*,*)'Please input y or n for sorting the position or not'
                goto 56
                end select

52      continue ! for siesta graphene

        rewind(20)

        read(20,'(a)')dummy

        allocate(num_ele(num))
        allocate(tpos(num,3))
        allocate(character(len=2) :: element(num))
        allocate(character(len=2) :: comp_ele(num))

        write(*,*)'Running... Quick checking completed'
        write(*,*)'Running... Reading the file'

        do i=1,num
        read(20,'(a2,2x,a6,3(f15.9))')&
                element(i),dummy,(tpos(i,j),j=1,3)
        enddo

        do i=1,num
                num_ele(i)=0
        enddo

        write(*,*)'Running... Reading completed'
        write(*,*)'Running... Checking the element in file'

                j=1 ! give a comp_ele one label
                k=1 ! comp_ele
                a: do i=1,num
                  if(i==1)then
                        comp_ele(k)=element(i)
                        ! put element(i) into first comp_ele
                        num_ele(k)=num_ele(k)+1
                  else
                    
                    b: do j=1,i-1

                    !after the first one distinguish by itself
                    ! equal or not equal
                      if(element(j)==element(i))then
                        !check m = 1 first then to k for calculating
                        ! num_ele of each element
                        do m=1,k 
                        if(comp_ele(m)==element(i))then
                        num_ele(m)=num_ele(m)+1
                        exit b
                        else
                        endif
                        enddo
                    
                    else
                        l=l+1
                      endif
                    enddo b
              endif
              ! if current one is not equal to last one
              ! maybe a new element
                    if(l==i-1.and.i>1)then
                    k=k+1
                    comp_ele(k)=element(i)
                    num_ele(k)=num_ele(k)+1
                  else
                  endif
                      l=0
              enddo a
            
        maxa=k

        write(*,*)'Running... Checking completed'

        write(*,*)' '
        write(*,*)'Number of atoms ',num
        write(*,*)' '
        write(*,'(a19,1x,i6)')'Total atom type = ',maxa
        write(*,*)' '

        do j=1,maxa
        write(*,'(a15,a2,2x,a9,2x,i6)')'Type of atom = '&
                        ,comp_ele(j),'Amount = ',num_ele(j)        
        enddo
        write(*,*)' '

        write(*,*)'Running... Sorting the element order'

        do i=1,num
        do k=1,maxa
                if(element(i)==comp_ele(k))then
        write(30,'(1x,i6,1x,i2,3(f12.7))')i,k,(tpos(i,j),j=1,3)
        else
        endif
        enddo 
        enddo

        rewind(30)

        allocate(ele_type(num))

        do i=1,num
        read(30,'(1x,i6,1x,i2,3(f12.7))')&
                atom,ele_type(i),(tpos(i,j),j=1,3)
        enddo

        do i=1,num ! Normal sort with element
        do k=1,num
        if(k>=2.and.k<=num.and.ele_type(k-1)>ele_type(k))then
                temp_ele=ele_type(k)
                ele_type(k)=ele_type(k-1)
                ele_type(k-1)=temp_ele
                temp_ele=tpos(k,1)
                tpos(k,1)=tpos(k-1,1)
                tpos(k-1,1)=temp_ele
                temp_ele=tpos(k,2)
                tpos(k,2)=tpos(k-1,2)
                tpos(k-1,2)=temp_ele
                temp_ele=tpos(k,3)
                tpos(k,3)=tpos(k-1,3)
                tpos(k-1,3)=temp_ele
        else
        endif
        enddo
        enddo

        write(*,*)'Running... Sorting completed'
        write(*,*)'Running... Sorting the data (Y position)'

        num_ele_low=1
        num_ele_hi=0
        do k=1,maxa !normal really bad sort with x or y or z
        num_ele_hi=num_ele_hi+num_ele(k)
               do j=num_ele_low,num_ele_hi
               do i=num_ele_low,num_ele_hi
                if(i>=num_ele_low+1&
                .and.i<=num_ele_hi.and.tpos(i-1,2)>tpos(i,2))then
                temp_ele=tpos(i,1)
                tpos(i,1)=tpos(i-1,1)
                tpos(i-1,1)=temp_ele
                temp_ele=tpos(i,2)
                tpos(i,2)=tpos(i-1,2)
                tpos(i-1,2)=temp_ele
                temp_ele=tpos(i,3)
                tpos(i,3)=tpos(i-1,3)
                tpos(i-1,3)=temp_ele
                else
                endif
                enddo      
                enddo
        num_ele_low=num_ele_low+num_ele(k)
        enddo

        write(*,*)'Running... Sorting completed'
        
        rewind(20)

        write(*,*)'Running... Checking the data'

        write(40,*)' Position data'
        write(40,*)' '
        write(40,'(i8,1x,a5)')num,'atoms'
        write(40,*)' '
        write(40,'(i3,1x,a10)')maxa,'atom types'
        write(40,*)' '

        read(20,'(3X,6(F10.4))')(cell(i),i=1,6)
        write(40,'(1x,a3,1x,f11.7,1x,a7)')'0.0',cell(1),'xlo xhi'
        write(40,'(1x,a3,1x,f11.7,1x,a7)')'0.0',cell(2),'ylo yhi'
        write(40,'(1x,a3,1x,f11.7,1x,a7)')'0.0',cell(3),'zlo zhi'

        allocate(character(len=2) :: ele_multi(maxa))
        allocate(character(len=10) :: ma_mu(maxa))
        allocate(character(len=10) :: chs_ma(maxa))
        allocate(character(len=2) :: chs_el(maxa))

        write(40,*)' '
        write(40,*)' Masses'
        write(40,*)' '

        do i=1,maxa
        ele_multi(i)=comp_ele(i)
        enddo

        write(*,*)' '
        do i=1,maxa
        write(*,'(a,1x,a2,a)')'Please input the mass of',&
        ele_multi(i)&
        ,', or (d for "Default" mass 10.0000000, q for "Quit" the job)'

        read(5,*)chs_ma(i)
        write(*,*)' '

        if(chs_ma(i)=='d'.or.chs_ma(i)=='D')then
        ma_mu(i)='10.0000000'
        elseif(chs_ma(i)=='q'.or.chs_ma(i)=='Q')then
        write(*,*)'You Quit the job'
        deallocate(num_ele)
        deallocate(ele_type)
        deallocate(element)
        deallocate(comp_ele)
        deallocate(tpos)
        deallocate(ele_multi)
        deallocate(ma_mu)
        deallocate(chs_ma)
        deallocate(chs_el)
        goto 55
        else
        ma_mu(i)=chs_ma(i)
        endif


        write(*,'(a,1x,a2,2a)')&
        'Please rename the element label of',&
        ele_multi(i),&
        ',(for the input file of lammps calculation,'&
        ' ex. "C" for Carbon, uppercase is needed)'

        read(5,*)chs_el(i)
        write(*,*)' '

        write(40,'(i3,1x,a10,1x,a2)')i,ma_mu(i),chs_el(i)
        enddo

        write(*,*)' '
        do i=1,maxa
        write(*,'(i3,1x,a10,1x,a2)')i,ma_mu(i),chs_el(i)
        enddo
        write(*,*)' '

        deallocate(ele_multi)
        deallocate(ma_mu)
        deallocate(chs_ma)
        deallocate(chs_el)

        write(40,*)' '
        write(40,*)' Atoms'
        write(40,*)' '

        write(*,*)'Running... Checking completed'
        write(*,*)'Outputing the data'

        do i=1,num
        write(40,'(1x,i6,1x,i2,3(f12.7))')&
                        i,ele_type(i),(tpos(i,j),j=1,3)
        enddo

        write(*,*)'Outputing completed'
        write(*,*)'data.lammps file created'
        write(*,*)' '

        deallocate(num_ele)
        deallocate(ele_type)
        deallocate(element)
        deallocate(comp_ele)
        deallocate(tpos)

        goto 55

53      continue        !sort z and not sort z for lammps 
        
        ele=1
        rewind(20)
        
        if(chs_sort)then
        write(*,'(2a)')'Running... Checking the data',&
                ' (Sorting by Z position)'
        else
        write(*,'(2a)')'Running... Checking the data',&
                ' (No sorting by Z position)'
        endif

        write(40,*)' Position data'
        write(40,*)' '
        write(40,'(i8,1x,a5)')num,'atoms'
        write(40,*)' '
        write(40,'(i3,1x,a10)')ele,'atom types'
        write(40,*)' '

        read(20,'(3X,6(F10.4))')(cell(i),i=1,6)
        write(40,'(1x,a3,1x,f11.7,1x,a7)')'0.0',cell(1),'xlo xhi'
        write(40,'(1x,a3,1x,f11.7,1x,a7)')'0.0',cell(2),'ylo yhi'
        write(40,'(1x,a3,1x,f11.7,1x,a7)')'0.0',cell(3),'zlo zhi'

        write(40,*)' '
        write(40,*)' Masses'
        write(40,*)' '

        write(*,*)' '
        write(*,'(a24,1x,a2,1x,a58)')'Please input the mass of',one_ele&
        ,'or (d for "Default" mass 10.0000000, q for "Quit" the job)'

        read(5,*)choose_mass
        write(*,*)' '

        if(choose_mass=='d'.or.choose_mass=='D')then
        mass_one='10.0000000'
        elseif(choose_mass=='q'.or.choose_mass=='Q')then
        write(*,*)'You Quit the job'
        goto 55
        else
        mass_one=choose_mass
        endif

        write(40,'(i3,1x,a10,1x,a2)')ele,mass_one,one_ele

        write(40,*)' '
        write(40,*)' Atoms'
        write(40,*)' '
        write(*,*)'Running... Checking completed'
        write(*,*)'Running... Reading the data'

        allocate(tpos(num,3))
        
        do i=1,num
        read(20,'(4x,i6,3(f15.9))')&
                atom,(tpos(i,j),j=1,3)
        enddo

        write(*,*)'Running... Reading completed'
        
        ! ----- practice a logical control loop for sorting
        if(chs_sort)then
        write(*,*)'Running... Sorting the data (Z position)'

        sort_max=num-1
57      continue

        if(sort_max>1)then
        j=1
        do i=1,sort_max
        if(i>=2.and.tpos(i-1,3)>tpos(i,3))then ! bad Bubble sort
                temp_ele=tpos(i,3)
                tpos(i,3)=tpos(i-1,3)
                tpos(i-1,3)=temp_ele
                temp_ele=tpos(i,1)
                tpos(i,1)=tpos(i-1,1)
                tpos(i-1,1)=temp_ele
                temp_ele=tpos(i,2)
                tpos(i,2)=tpos(i-1,2)
                tpos(i-1,2)=temp_ele
                j=i
        else
        endif
        enddo
        sort_max=j
        goto 57
        else
        endif

        write(*,*)'Running... Sorting completed'
        write(*,*)'Outputing the data (Sorted)'
        else
        write(*,*)'Outputing the data (Not sorted)'
        endif

        do i=1,num
        write(40,'(1x,i6,1x,i2,3(f12.7))')&
                i,ele,(tpos(i,j),j=1,3)
        enddo
        
        write(*,*)'Outputing file completed'
        write(*,*)'data.lammps file created'

        !------------------------- new part divide z value
        if(chs_sort)then
          write(*,*)''
          write(*,*)'-------------- Information ---------------'
          write(*,*)'Do you want to divide the value of Z axis?'
          write(*,*)'y for yes, n for no, q for quit'
          write(*,*)'Warning ==> This will replace the original ',&
            'data.lammps file'
3000    continue
        read(5,*,iostat=chk_div)chs_div
        if(chk_div/=0)then
          write(*,*)''
          write(*,*)'Please choose y for yes, n for no, q for quit.'
          goto 3000
          ! case function is better
        else
          select case(chs_div)
          case('y','Y')
          rewind(40)
        open(50,file='datatemp.lammps', form='formatted', &
                status='unknown',access='sequential')
          goto 3001
        case('n','N')
          write(*,*)''
          write(*,*)'You choose not to divide the value of Z axis. Bye!'
          goto 3002
        case('q','Q')
          write(*,*)''
          write(*,*)'You quit... Bye!!'
          goto 3003
        case default
          write(*,*)''
          write(*,*)'Please choose y for yes, n for no, q for quit.'
          goto 3000
        endselect
        endif

3001    continue

        write(*,*)' '
        write(*,*)'Running...   Changing the label of atoms'
        write(*,*)' '
        write(*,*)'Please choose first range of Z position'

        do i=1,16
        read(40,'(a)')dummy
        enddo

91      continue
        read(5,*,iostat=chk_div_1)range_set
        if(chk_div_1/=0)then
        write(*,*)' ' 
        write(*,*)'Please input the correct range value again'
        goto 91
        else
        endif
        
        write(*,*)' ' 
        write(*,*)'Please input the lable you want to change'
92      continue
        read(5,*,iostat=chk_div_2)changetolable
        if(chk_div_2/=0)then
        write(*,*)' ' 
        write(*,*)'Please input the correct lable again'
        goto 92
        else
        endif

        write(*,*)' '
        write(*,*)'Please wait for changing'
        write(*,*)' '

        j=1
        do i=1,num
        allocate(id(i))
        allocate(tpos_z(i))
        read(40,'(a8,i2,a24,f12.7)')dummy,id(i),dummy2,tpos_z(i)
        if(tpos_z(i)<=range_set)then
                id(i)=changetolable
                write(50,'(i7,1x,i2,a24,f12.7)')&
                i,changetolable,dummy2,tpos_z(i)
        elseif(tpos_z(i)>range_set)then
                write(*,'(a,i0,a)')&
                        'The NO.',j,' change completed'
                write(*,*)' '
                write(*,*)'Please choose the next range'
93      continue
                read(5,*,iostat=chk_div_1)range_set
                if(chk_div_1/=0)then
                write(*,*)' ' 
                write(*,*)'Please input the correct range value again'
                goto 93
                elseif(range_set<tpos_z(i))then
                write(*,*)' ' 
                write(*,*)'Please input a larger value again'
                goto 93
                else 
                endif
                write(*,*)' '
                write(*,*)'Please input the lable you want to change'
94      continue
                read(5,*,iostat=chk_div_2)changetolable
                if(chk_div_2/=0)then
                write(*,*)' ' 
                write(*,*)'Please input the correct lable again'
                goto 94
                else
                endif
                write(50,'(i7,1x,i2,a24,f12.7)')&
                i,changetolable,dummy2,tpos_z(i)
                j=j+1
                write(*,*)' '
                write(*,*)'Please wait for changing'
                write(*,*)' '
        else
        endif
                      
        deallocate(id)
        deallocate(tpos_z)
        enddo

                write(*,'(a,i0,a)')&
                        'The NO.',j,' change completed'
                write(*,*)''
                write(*,*)'Change completed...'
        write(*,*)''
        write(*,*)'-------- Information ---------'
        write(*,*)'Please renew the atom types and masses'

        rewind(40)
        rewind(50)
        open(60,file='datanew.lammps', form='formatted', &
                status='unknown',access='sequential')
        do i=1,4      
        read(40,'(a)')dummy
        write(60,'(a)')dummy
        enddo
        !atom j types
        read(40,'(a)')dummy!useless read
        write(60,'(1x,i0,1x,(a))')j,'atom types'
        do i=1,7
        read(40,'(a)')dummy
        write(60,'(a)')dummy
        enddo

        !read one element
        read(40,'(a15,(a))')dummy,dummy2
        write(*,*)''
        do i=1,j

        write(*,'(a,i0,a)')'Please rename the NO.',i,' element'
        write(*,'(3a)')'(d for "Default" mass ',trim(dummy2),&
          ', q for "Quit" the job)'
        ! later add check error
        read(5,*)el_rena
        write(*,*)''
        if(el_rena=='d'.or.el_rena=='D')then
        rena=dummy2
        elseif(el_rena=='q'.or.el_rena=='Q')then
        write(*,*)'You Quit the job'
        goto 55
        else
        rena=el_rena
        endif

        write(*,'(3a)')'Please input the mass of ',trim(rena)&
        ,', or (d for "Default" mass 10.0000000, q for "Quit" the job)'

        read(5,*)chs_rena
        write(*,*)''

        if(chs_rena=='d'.or.chs_rena=='D')then
        mass_rena='10.0000000'
        elseif(chs_ma(i)=='q'.or.chs_ma(i)=='Q')then
        write(*,*)'You Quit the job'
        goto 55
        else
        mass_rena=chs_rena
        endif

        write(60,'(i3,1x,a10,1x,a2)')i,mass_rena,rena
        enddo

        do i=1,3
        read(40,'(a)')dummy
        write(60,'(a)')dummy
        ! data.lammps read completed
        enddo

        ! start to read datatemp.lammps
        write(*,*)'Writing the file ==> data.lammps'
        write(*,*)''
        do i=1,num
        read(50,'(a)')dummy
        write(60,'(a)')dummy
        enddo
        write(*,*)'a new data.lammps has been created!'

        status=rename('datanew.lammps','data.lammps')
        if (status.ne.0) then
        write(*,*)''
        write(*,*)'Error ==> There is no file to be renamed.'
        write(*,*)'Please check it again, and program stopped'
        goto 3002
        else
        endif

        else
        endif
        
        ! rename from 50 to 40

        close(50,status='delete')
        close(60)

3002    continue

3003    continue

        deallocate(tpos)

55      continue
        deallocate(cell)
        write(*,*)' '

        close(10)
        close(20,status='delete')! status can be removed for checking
        close(30,status='delete')
        close(40)

        goto 99

98      continue
        write(*,*)
        write(*,*)'Error: Can not open a *.car file.'
        write(*,*)
        goto 99

99      stop

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

        end program car2Lammpsf90
