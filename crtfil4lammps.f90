        program crtfil4lammps
        

        integer i,j,k
        integer nutot_ty,tmp_el
        integer,allocatable :: nu_el(:)
        character(len=:),allocatable :: lb_el(:)
        character(len=:),allocatable :: i_tmp_el(:)
        character(64) dummy

        write(*,*)' '
        write(*,*)'Running...   ',&
                'Creating in.min (C&H) file for lammps ',&
                        'Version--1.0 beta'
        write(*,*)' '

        write(*,*)'Running...   Checking data.lammps file'
        open(10,file='data.lammps', form='formatted', status='old',& 
                err=999, access='sequential')
        open(20,file='in.min', form='formatted',& 
                status='unknown', access='sequential')
        write(*,*)'Running...   data.lammps file existed'
        write(*,*)' '

!--------------------------- reading ---------------------------
        
        write(*,*)'Running...   Reading data.lammps file'
        do i=1,4
        read(10,'(a)')dummy
        enddo
        
        read(10,*)nutot_ty!read atom types
        
        do i=6,12
        read(10,'(a)')dummy
        enddo

        allocate(character(2) :: lb_el(nutot_ty))
        allocate(character(4*nutot_ty) :: i_tmp_el(nutot_ty))
        allocate(nu_el(nutot_ty))

        k=0
        do i=1, nutot_ty
        read(10,*)nu_el(i),dummy,lb_el(i)
        if(lb_el(i)=='C')then
        k=k+1
        tmp_el=nu_el(i)
        write(i_tmp_el(k),'(i2)')tmp_el
        else
        endif
        enddo

        write(*,*)'Running...   Reading completed'
        write(*,*)' '

!--------------------------- writing ---------------------------

        write(*,*)'Running...   Writing the in.min file'

        write(20,*)'#input file for (uniaxial strain) lammps'
        write(20,*)'# ----------- Initialized Simulation -----------'
        write(20,*)'clear'
        write(20,*)''
        write(20,*)'variable srate equal -0.0005'
        write(20,*)'variable num_step equal 90000'
        write(20,*)''
        write(20,*)'units       metal'
        write(20,*)'atom_style  atomic'
        write(20,*)'dimension   3'
        write(20,*)'boundary    p p p'
        write(20,*)''
        write(20,*)'atom_modify map array'
        write(20,*)'read_data   data.lammps'
        write(20,*)'group carbon type',&
                        (adjustr(trim(i_tmp_el(i))),i=1,k)

        write(*,*)' '
        write(*,*)'Please confirm the carbon group is correct or not'
        write(*,*)'group carbon type',&
                        (adjustr(trim(i_tmp_el(i))),i=1,k)

        write(20,*)''
        write(20,*)'# ---- Define Interatomic Potential -----'
        write(20,*)'pair_style          airebo 3.0 0 1'
        write(20,*)'pair_coeff          * * CH.airebo',&
                        (adjustr(lb_el(i)),i=1,nutot_ty)

        write(*,*)' '
        write(*,*)'Please confirm the order of C & H is correct or not'
        write(*,*)'pair_coeff          * * CH.airebo',&
                        (adjustr(lb_el(i)),i=1,nutot_ty)

        write(20,*)'neighbor            0.3 bin'
        write(20,*)'neigh_modify        delay 0'
        write(20,*)''
        write(20,*)'# ----------- Define Settings ---------'
        write(20,*)'compute     displace all displace/atom'
        write(20,*)'compute     eng all pe/atom'
        write(20,*)'compute     stress all stress/atom NULL'
        write(20,*)'compute     eatoms all reduce sum c_eng'
        write(20,*)''
        write(20,*)'# ----------- Equilibration ---------'
        write(20,*)'reset_timestep 0'
        write(20,*)''
        write(20,*)'dump        ',&
        '1 all custom 50 dump.GNR id type xs ys zs fx fy fz'
        write(20,*)''
        write(20,*)'#fix 1 all nve'
        write(20,*)'thermo 10'
        write(20,*)'thermo_style custom ',&
        'step pe lx ly lz press pxx pyy pzz c_eatoms'
        write(20,*)'min_style cg'
        write(20,*)'fix 2 carbon setforce 0 0 0'
        write(20,*)'minimize 1e-25 1e-25 5000 10000'
        write(20,*)''
        write(20,*)''
        write(20,*)'print "##### Min done! #####"'
        write(20,*)'fix 3 all nve'
        write(20,*)'run 1'
        write(20,*)''
        write(20,*)'print "All done!"'

        write(*,*)' '
        write(*,*)'Running...   Writing completed'
        write(*,*)' '
        write(*,*)'in.min file created'
        write(*,*)'Please check the in.min file later'
        write(*,*)' '
        write(*,*)' '


        deallocate(lb_el)
        deallocate(nu_el)
        deallocate(i_tmp_el)


        goto 998

999     continue
        write(*,*)' '
        write(*,*)'Error: Cannnot find file "data.lammps".'
        write(*,*)'===> Please run getdumpf90 to get the file'
        write(*,*)' '
        goto 998

998     stop

        endprogram crtfil4lammps
