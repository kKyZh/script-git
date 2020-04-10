      program main
      
      use m_alloc
      use m_random_number
      use m_sort
        ! this program is a practice for learning module procedures
      
      implicit none
        !use m_swap
      !integer i,j,k!,l
      integer nu, mi_nu, ma_nu !no implicit none is okay 
      !integer tmp
        !integer lo_nu,hi_nu
      !integer out_nu !wrong specify in this script
      integer,allocatable :: out_nu(:)
      ! even no implicit none i must declare allocate array

      !real r
      !character(32) i_k
        
        write(*,*)'How many random numbers do you want'
        read(*,*)nu
        write(*,*)'Enter the minimum number of random numbers'
        read(*,*)mi_nu
        write(*,*)'Enter the maximum number of random numbers'
        read(*,*)ma_nu
!------------------allocate then module--------------
        !allocate(out_nu(nu))
      call alloc(nu,out_nu)  
!-------------------------- random number module-----------
      call rd_nu(nu,mi_nu,ma_nu,out_nu)
      !k=0
      !do i=1, nu
!100   continue
      !call random_seed()
      !call random_number(r)
      !out_nu(i) = mi_nu + floor((ma_nu+1-mi_nu)*r) 
      !if (i>1) then
      !do j=1, i-1
      !if (out_nu(i)==out_nu(j))then
      !k=k+1
      !goto 100
      !else
      !endif
      !enddo
      !else
      !endif
      !enddo
      !write(*,*) nu
      !write(i_k,'(i0)')k
      !write(*,'(3a)')'we had ',trim(i_k),' repeated numbers'
      !write(*,'(a,<nu>(i0,1x))')'the random numbers are: ',&
      !    (out_nu(i), i=1, nu)
!----------------------------------sort then module----
      call sort(nu,out_nu)
        !k=0
        !lo_nu=1
        !hi_nu=nu-1
        !do j=lo_nu,hi_nu
        !l=0
        !do i=lo_nu,hi_nu
        !if(out_nu(i)>out_nu(i+1))then
        !call swap(out_nu(i),out_nu(i+1))
        !tmp=out_nu(i)
        !out_nu(i)=out_nu(i+1)
        !out_nu(i+1)=tmp
        !k=k+1
        !l=l+1
        !else
        !endif
        !enddo
        !if(l.eq.0)exit 
        !hi_nu=hi_nu-1
        !write(*,'(a,<nu>(i4,1x))')'the sorted numbers are: ',&
        !  (out_nu(i), i=1, nu)
        !enddo
        
        !lo_nu=lo_nu+1 ! lowest number is useless?
        
        !write(*,*)
        !write(*,*)j-1 ! sorted loops/times
       ! write(*,*)
       ! write(i_k,'(i0)')k
       ! write(*,'(3a)')'we swap ',trim(i_k),' times'
       ! write(*,'(a,<nu>(i4,1x))')'the sorted numbers are: ',&
       !   (out_nu(i), i=1, nu)
!-----------------------deallocate then module-------
       write(*,'(a,i0)')'size of random number array: ',sizeof(out_nu)
       ! we can add any variables for checking in each modules
       ! output k for checking, just add k and intent out k in each
       ! module. then we can write and output k in the main program
       call dealloc(out_nu)
        !deallocate(out_nu)
        endprogram main
