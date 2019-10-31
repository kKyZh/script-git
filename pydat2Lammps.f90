        program car2Lammpsf90

        integer :: i, j, k, l, m, n, num, maxa,h,temp1,temp2
        integer :: atom,ele,sort_max
        integer :: num_ele_low,num_ele_hi,check
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
        character(len=:),allocatable :: element(:),comp_ele(:)
        character(len=:),allocatable :: ele_multi(:)
        character(len=:),allocatable :: check_atom(:)

        call getarg(1,filename)
        open(10,file=filename, form='formatted', status='old',& 
                err=99, access='sequential')
        open(20,file='tempforcar2lammps.dat', form='formatted',& 
                status='unknown', access='sequential')
        open(30,file='tempforcar2lammps2.dat', form='formatted', &
                status='unknown',access='sequential')
        open(40,file='data.lammps', form='formatted', &
                status='unknown',access='sequential')

        write(*,*)' '
        write(*,*)' '
        write(*,*)'Running... car2Lammps to data.lammps',&
                '(Version --1.1beta)'
        write(*,*)' '
        write(*,*)'Running... Preparing the file'
        write(*,*)'Running... Quick checking the data'

                read(10,'(A64)') dummy

        allocate(cell(3))

        !write(*,*)'sizeof',sizeof(cell)
        read(10,*)(cell(i),i=1,3)
        write(20,'(3(F10.4))')(cell(i),i=1,3)
        !write(*,'(3X,6(F10.4))')(cell(i),i=1,6)
        
        read(10,'(a)')dummy

        num=1
        i=1
50      continue
        allocate(tpos(i,j)) 
         !read(10,*,iostat=check)aa,dummy,(tpos(i,j),j=1,3)
         !if(check>0)then
        !            goto 999
        !            else
        !            endif
        !write(*,'(a2,2x,i6,3(f15.9))')&
        !        dummy,num,(tpos(i,j),j=1,3)
        !if(aa/='end')then
!
        !      write(20,'(a2,2x,i6,3(f15.9))')&
        !        dummy,num,(tpos(i,j),j=1,3)
        !num=num+1
        !i=i+1
        !deallocate(tpos)
        !goto 50
        read(10,*,end=999)aa,dummy,(tpos(i,j),j=1,3)
        if(aa/='end')then

        write(20,'(a2,2x,i6,3(f15.9))')&
                dummy,num,(tpos(i,j),j=1,3)
        write(*,'(a2,2x,i6,3(f15.9))')&
                dummy,num,(tpos(i,j),j=1,3)
        num=num+1
        i=i+1
        deallocate(tpos)
        goto 50
        else
                endif
999     continue

        num=num-1
        write(*,*)'num',num
        write(*,*)'hello there'
        deallocate(tpos)
        goto 51
      
51      continue        !quick check for lammps or siesta
        
        close(20)
        open(20,file='tempforcar2lammps.dat', form='formatted',& 
                status='old', access='sequential')

        read(20,'(a)')dummy

        allocate(character(2)::check_atom(num))

        !write(*,*)'sizeof',sizeof(check_atom)

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

        close(20)

56      continue

        read(5,*)choose_sort

        select case (choose_sort)
                case ('y','Y')
                goto 53
                case ('n','N')
                goto 54
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

        close(20)
        open(20,file='tempforcar2lammps.dat', form='formatted',& 
                status='old', access='sequential')

        read(20,'(a)')dummy

        allocate(num_ele(num))
        allocate(tpos(num,3))
        allocate(character(len=2) :: element(num))
        allocate(character(len=2) :: comp_ele(num))

        !write(*,*)'sizeof',sizeof(num_ele)
        !write(*,*)'sizeof',sizeof(tpos)
        !write(*,*)'sizeof',sizeof(element)
        !write(*,*)'sizeof',sizeof(comp_ele)

        write(*,*)'Running... Quick checking completed'
        write(*,*)'Running... Reading the file'

        do i=1,num
        read(20,'(a2,2x,a6,3(f15.9))')&
                element(i),dummy,(tpos(i,j),j=1,3)
        !write(*,'(a15,1x,i6,2x,a2)')'i,element(i) ',i,element(i)
        !write(*,*)'sizeof',sizeof(element)
        enddo

        do i=1,num
                num_ele(i)=0
        enddo

        write(*,*)'Running... Reading completed'
        write(*,*)'Running... Checking the element in file'

                j=0
                k=1
                do i=1,num
                  m=0
                                if(i==1)then
                        comp_ele(1)=element(i)
                        num_ele(1)=num_ele(1)+1
             !    write(*,*)'comp_ele 1,num_ele 1= '&
              !          ,comp_ele(1),num_ele(1)
              elseif(comp_ele(1)==element(i))then
                               num_ele(1)=num_ele(1)+1
               !      write(*,*)'i,num_ele 1= ',i,num_ele(k),element(i)
                elseif(comp_ele(1)/=element(i))then
                        j=j+1
                        if(j==1)then
                        k=k+1
                        comp_ele(k)=element(i)
                        num_ele(k)=num_ele(k)+1
        !write(*,*)'j=1,k=k+1,k,comp(k),num(k)',j,k,&
        !                comp_ele(k),num_ele(k)
                        elseif(j>1.and.j<=(num-1))then
                                do n=1,(j-1)
                         if(element(i)==comp_ele(n+1))then
                                num_ele(n+1)=num_ele(n+1)+1
       ! write(*,*)'i, j ,k,comp(k),num(k),element j='&
         !       ,i,j,k,comp_ele(n+1),num_ele(n+1),element(i)
                elseif(element(i)/=comp_ele(n+1))then
                                             m=m+1
                                 else
                                 endif
                                enddo
                                if((j-1)==m)then
                                        k=k+1
                                        comp_ele(k)=element(i)
                                        num_ele(k)=num_ele(k)+1
       ! write(*,*)'i, j ,k,comp(k),num(k),element j='&
       !         ,i,j,k,comp_ele(k),num_ele(k),element(i)
                                 else
                                endif
                        else
                        endif
                else
                endif
                enddo
        maxa=k

        write(*,*)'Running... Checking completed'

        write(*,*)' '
        write(*,*)'Number of atoms ',num
        write(*,*)' '
        write(*,'(a19,1x,i6)')'Total atom type = ',maxa
        write(*,*)' '

        do k=1,maxa
        !write(30,'(a25,i6,2x,i6,2x,2a)')'k,num_ele k,comp_ele k= '&
               ! ,k,num_ele(k),comp_ele(k)
        write(*,'(a15,a2,2x,a9,2x,i6)')'Type of atom = '&
                        ,comp_ele(k),'Amount = ',num_ele(k)        
        enddo
        write(*,*)' '

        write(*,*)'Running... Sorting the element order'

        do i=1,num
        do k=1,maxa
                if(element(i)==comp_ele(k))then
        !write(*,'(1x,i6,1x,i2,3(f12.7))')i,k,(tpos(i,j),j=1,3)
        write(30,'(1x,i6,1x,i2,3(f12.7))')i,k,(tpos(i,j),j=1,3)
        else
        endif
        enddo 
        enddo

        close(30)
        open(30,file='tempforcar2lammps2.dat', form='formatted', &
                status='old',access='sequential')

        allocate(ele_type(num))

        do i=1,num
        read(30,'(1x,i6,1x,i2,3(f12.7))')&
                atom,ele_type(i),(tpos(i,j),j=1,3)
        !write(*,*)atom,ele_type(i),(tpos(i,j),j=1,3)
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
        do k=1,maxa !normal bad sort with x or y or z
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
        
        close(20)
        open(20,file='tempforcar2lammps.dat', form='formatted',& 
                status='old', access='sequential')


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

        !write(*,*)'sizeof',sizeof(ele_multi)
        write(40,*)' '
        write(40,*)' Masses'
        write(40,*)' '

        do i=1,maxa
        ele_multi(i)=comp_ele(i)
        enddo

        write(*,*)' '
        do i=1,maxa
        write(*,'(a24,1x,a2,1x,a58)')'Please input the mass of',&
        ele_multi(i)&
        ,'or (d for "Default" mass 10.0000000, q for "Quit" the job)'

        read(5,*)choose_mass
        write(*,*)' '

        if(choose_mass=='d'.or.choose_mass=='D')then
        mass_multi='10.0000000'
        elseif(choose_mass=='q'.or.choose_mass=='Q')then
        write(*,*)' '
        write(*,*)'You Quit the job'
        write(*,*)' '
        deallocate(num_ele)
        deallocate(ele_type)
        deallocate(element)
        deallocate(comp_ele)
        deallocate(tpos)
        deallocate(ele_multi)
        goto 55
        else
        mass_multi=choose_mass
        endif

        write(40,'(i3,1x,a10)')i,mass_multi
        !write(40,'(i3,1x,a10,1x,a2)')i,mass_multi,&
        !        ,ele_multi(i)
        !write(40,'(i3,1x,a10)')i,mass_multi
        enddo

        write(*,*)' '
        do i=1,maxa
        write(*,'(i3,1x,a10,1x,a2)')i,mass_multi,ele_multi(i)
        !write(*,'(i3,1x,a10)')i,mass_multi)
        enddo
        write(*,*)' '

        deallocate(ele_multi)

        write(40,*)' '
        write(40,*)' Atoms'
        write(40,*)' '

        write(*,*)'Running... Checking completed'
        write(*,*)'Outputing the data'

        do i=1,num
        write(40,'(1x,i6,1x,i2,3(f12.7))')&
                        i,ele_type(i),(tpos(i,j),j=1,3)
        !write(*,'(1x,i6,1x,i2,3(f12.7))')&
        !                i,ele_type(i),(tpos(i,j),j=1,3)
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

53      continue        !sort z for lammps 
        
        ele=1
        open(20,file='tempforcar2lammps.dat', form='formatted',& 
                status='old', access='sequential')

        write(*,*)'Running... Checking the data'&
                ' (Sorting by Z position)'

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
        write(*,*)' '
        write(*,*)'You Quit the job'
        write(*,*)' '
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
        write(*,*)'Running... Sorting the data (Z position)'

        sort_max=num-1
57      continue

        if(sort_max>1)then
        j=1
        do i=1,sort_max
        if(i>=2.and.tpos(i-1,3)>tpos(i,3))then !Bubble sort
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

        do i=1,num
        write(40,'(1x,i6,1x,i2,3(f12.7))')&
                i,ele,(tpos(i,j),j=1,3)
        enddo

        deallocate(tpos)

        write(*,*)'Outputing file completed'
        write(*,*)'data.lammps file created'
        write(*,*)' '

        goto 55

54      continue        !no sort for lammps

        ele=1
        open(20,file='tempforcar2lammps.dat', form='formatted',& 
                status='old', access='sequential')

        write(*,*)'Running... Outputing the data (No sorting)'
        write(*,*)'Running... Checking the data'

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
        write(*,*)' '
        write(*,*)'You Quit the job'
        write(*,*)' '
        goto 55
        else
        mass_one=choose_mass
        endif

        write(40,'(i3,1x,a10,1x,a2)')ele,mass_one,one_ele

        write(40,*)' '
        write(40,*)' Atoms'
        write(40,*)' '

        write(*,*)'Running... Checking completed'
        write(*,*)'Reading and outputing the data (Not sorted)'

        do i=1,num
        allocate(tpos(num,3))
        read(20,'(4x,i6,3(f15.9))')&
                atom,(tpos(i,j),j=1,3)
        write(40,'(1x,i6,1x,i2,3(f12.7))')&
                atom,ele,(tpos(i,j),j=1,3)
        deallocate(tpos)
        enddo

        write(*,*)'Outputing file completed'
        write(*,*)'data.lammps file created'
        write(*,*)' '

55      continue
        deallocate(cell)
        write(*,*)' '

99      stop
        close(10)
        close(20)
        close(30)
        close(40)

        end program car2Lammpsf90
