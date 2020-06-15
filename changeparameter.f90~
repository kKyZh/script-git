        program changedata

        integer :: i, j, num, maxa,check
        integer :: changetolable
        real :: range_set
        integer, allocatable :: id(:)
        real, allocatable :: tpos(:),cell(:)
        character(3) :: aa
        character(64) :: dummy, filename,dummy2

        call getarg(1,filename)
        open(10,file=filename, form='formatted', status='old',& 
                err=99, access='sequential')
        open(20,file='datanew.lammps', form='formatted', &
                status='unknown',access='sequential')

        write(*,*)' '
        write(*,*)' '
        write(*,*)'Running... Changedata from data.lammps',&
                '(Version --1.0beta)'
        write(*,*)' '
        write(*,*)'Running... Preparing the file'
        write(*,*)'Running... Quick checking the data'

        do i=1, 2
                read(10,'(a)') dummy
                write(20,'(a)')dummy
        enddo
        
        read(10,'(1x,i7,1x,a5)')num,dummy
        write(20,'(1x,i7,1x,a5)')num,dummy
        read(10,'(a)')dummy
        write(20,'(a)')dummy
        read(10,'(1x,i2,1x,a10)')maxa,dummy
        write(20,'(1x,i2,1x,a10)')maxa,dummy
        read(10,'(a)')dummy
        write(20,'(a)')dummy

        do i=1,3
        allocate(cell(3))
        read(10,'(1x,a2,1x,F11.7,1x,a7)')dummy,cell(i),dummy2
        write(20,'(1x,a2,1x,F11.7,1x,a7)')dummy,cell(i),dummy2
        deallocate(cell)
        enddo 

        do i=1,3
        read(10,'(a)')dummy
        write(20,'(a)')dummy
        enddo

        do i=1,maxa
        read(10,'(a)')dummy
        write(20,'(a)')dummy
        enddo

        do i=1,3
        read(10,'(a)')dummy
        write(20,'(a)')dummy
        enddo

        write(*,*)'Running...   Quick checking completed'
        write(*,*)' '
        write(*,'(1x,i7,1x,a5)')num,'atoms'
        write(*,'(1x,i2,1x,a10)')maxa,'type atoms'
        write(*,*)' '
        write(*,*)'Running...   Changing the label of atoms'
        write(*,*)' '
        write(*,*)'Please choose first range of Z position'

91      continue
        read(5,*,iostat=check)range_set
        if(check>0)then
        write(*,*)' ' 
        write(*,*)'Please input the correct range value again'
        goto 91
        else
        endif
        
        write(*,*)' ' 
        write(*,*)'Please input the lable you want to change'
92      continue
        read(5,*,iostat=check)changetolable
        if(check>0)then
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
        allocate(tpos(i))
        read(10,'(a8,i2,a24,f12.7)')dummy,id(i),dummy2,tpos(i)
        if(tpos(i)<=range_set)then
                        id(i)=changetolabel
        !        write(*,'(i7,1x,i2,a24,f12.7)')&
        !        i,changetolable,dummy2,tpos(i)
                write(20,'(i7,1x,i2,a24,f12.7)')&
                i,changetolable,dummy2,tpos(i)
        elseif(tpos(i)>range_set)then
                write(*,'(1x,a7,1x,i2,1x,a16)')&
                        'The NO.',j,'change completed'
                write(*,*)' '
                write(*,*)'Please choose the next range'
93      continue
                read(5,*,iostat=check)range_set
                if(check>0)then
                write(*,*)' ' 
                write(*,*)'Please input the correct range value again'
                goto 93
                elseif(range_set<tpos(i))then
                write(*,*)' ' 
                write(*,*)'Please input a larger value again'
                goto 93
                else 
                endif
                write(*,*)' '
                write(*,*)'Please input the lable you want to change'
94      continue
                read(5,*,iostat=check)changetolable
                if(check>0)then
                write(*,*)' ' 
                write(*,*)'Please input the correct lable again'
                goto 94
                else
                endif
         !       write(*,'(i7,1x,i2,a24,f12.7)')&
         !       i,changetolable,dummy2,tpos(i)
                write(20,'(i7,1x,i2,a24,f12.7)')&
                i,changetolable,dummy2,tpos(i)
                j=j+1
                write(*,*)' '
                write(*,*)'Please wait for changing'
                write(*,*)' '
        else
        endif
        deallocate(id)
        deallocate(tpos)
        enddo

        write(*,*)' '
        write(*,*)'All change completed'
        write(*,*)' '
        write(*,*)'datanew.lammps created'
        write(*,*)' '
        write(*,*)' '

99      stop
        close(10)
        close(20)

        end program changedata
