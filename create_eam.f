      Program GEAM_set
      
      integer*4 type

      write(*,*)'Please select type of the setting'
      write(*,*)'(1) Creating the input file'
      write(*,*)'(2) Creating setf1 file of GEAM potential for LAMMPS'
      write(*,*)'(3) Creating both input and setf1 files'
      read(5,*) type
      
      select case (type)
      case(1)
         call MakeInp
      case(2)
         call MakeSetf1
      case(3)
         call MakeInp
         call MakeSetf1
      end select
      
      stop
      end
      
C-----------------------------------------------------------------C
      subroutine MakeInp
      
      integer*4   i, j, k, num_ele, ielement(17), num_pair
      integer*4   ue, sita, num_sita
      real*8      re(17), fe(17), rhoe(17), alpha(17), beta(17)
      real*8      beta1(17), A(17), B(17), cai(17), ramda(17)
      real*8      ramda1(17), Fn0(17), Fn1(17), Fn2(17), Fn3(17)
      real*8      F0(17), F1(17), F2(17), F3(17), F4(17)
      real*8      eta(17), Fn(17), rhoin(17), rhoout(17), rhol(17)
      real*8      rhoh(17), rhos(17), amass(17)

      character*2  name(20), ele_name
      character*5  name_pair(120)
      character*32 dummy
      
      write(*,*)'How many elements will you simulate'
      read(5,*) num_ele
      do i=1, num_ele
         write(*,'(A34,I2)')'Please input the name of element #',i
         read(5,*) name(i)
      end do
      if(num_ele == 1) then
         num_pair = 0
      else if(num_ele > 1) then
         if(num_ele == 2) then
            num_pair = 1
         else
            ue = 1
            sita = 1
            num_sita = num_ele-2
            do j=1, num_ele
               ue = ue*j
            end do
            do k=1, num_sita
               sita = sita*k
            end do
            num_pair = ue/(sita*2)
         endif
      endif

      open(unit=10,file='EAM_code',form='FORMATTED',status='OLD')
      open(20,FILE='EAMpara.dat', form='formatted', status='unknown',
     $     access='sequential')

      do i=1, 17
         read(10,'(A2)')ele_name
         do j=1, num_ele
            if(ele_name == name(j)) then
               read(10,*) re(j)
               read(10,*) fe(j)
               read(10,*) rhoe(j)
               read(10,*) rhos(j)
               read(10,*) alpha(j)
               read(10,*) beta(j)
               read(10,*) A(j)
               read(10,*) B(j)
               read(10,*) cai(j)
               read(10,*) ramda(j)
               read(10,*) Fn0(j)
               read(10,*) Fn1(j)
               read(10,*) Fn2(j)
               read(10,*) Fn3(j)
               read(10,*) F0(j)
               read(10,*) F1(j)
               read(10,*) F2(j)
               read(10,*) F3(j)
               read(10,*) eta(j)
               read(10,*) Fn(j)
               read(10,*) ielement(j)
               read(10,*) amass(j)
               read(10,*) F4(j)
               read(10,*) beta1(j)
               read(10,*) ramda1(j)
               read(10,*) rhol(j)
               read(10,*) rhoh(j)
               go to 100
            end if
         end do
         do k=1, 27
            read(10,'(A32)') dummy
         end do
 100  end do
      
      write(20,'(I2,1X,I3)') num_ele, num_pair
      write(20,*)' '
      do i=1, num_ele
         write(20,'(A2,1X,I2,1X,F10.6)')name(i),ielement(i), amass(i)
         write(20,*)'===== two-body part ====='
         write(20,*)'re         fe         alpha      beta'
         write(20,'(4(F10.6,1X))')re(i),fe(i),alpha(i),beta(i)
         write(20,*)'A          B          cai        ramda '
         write(20,'(4(F10.6,1X))')A(i),B(i),cai(i),ramda(i)
         write(20,*)'===== many-body part ====='
         write(20,*)'Fn0        Fn1        Fn2        Fn3 '
         write(20,'(4(F10.6,1X))')Fn0(i),Fn1(i),Fn2(i),Fn3(i)
         write(20,*)'F0         F1         F2         F3         F4 '
         write(20,'(5(F10.6,1X))')F0(i),F1(i),F2(i),F3(i),F4(i)
         write(20,*)'eta        Fn         beta1      ramda1'
         write(20,'(4(F10.6,1X))')eta(i),Fn(i),beta1(i),ramda1(i)
         write(20,*)'rhol       rhoh       rhoe       rhos'
         write(20,'(4(F10.6,1X))')rhol(i),rhoh(i),rhoe(i),rhos(i)
         write(20,*)' '
      end do
      
      if(num_pair >= 1) then
         do i=1, num_ele
            do j=1, i-1
               write(20,'(A2,A1,A2)')name(i),'-',name(j)
               write(20,*)'===== two-body part ====='
               write(20,*)'re_i       fe_i       alpha_i    beta_i'
               write(20,'(4(F10.6,1X))')re(i),fe(i),alpha(i),beta(i)
               write(20,*)'A_i        B_i        cai_i      ramda_i'
               write(20,'(4(F10.6,1X))')A(i),B(i),cai(i),ramda(i)
               write(20,*)'re_j       fe_j       alpha_j    beta_j'
               write(20,'(4(F10.6,1X))')re(j),fe(j),alpha(j),beta(j)
               write(20,*)'A_j        B_j        cai_j      ramda_j'
               write(20,'(4(F10.6,1X))')A(j),B(j),cai(j),ramda(j)
               write(20,*)' '
            end do
         end do
      endif

      close(20)
      return
      end

C-----------------------------------------------------------------C
      subroutine MakeSetf1

      integer*4   i, j, k, type, ielement(17)
      integer*4   i1, i2, ct, nr, nrho, length
      real*4      re(17), fe(17), rhoe(17), alpha(17), beta(17)
      real*4      beta1(17), A(17), B(17), cai(17), ramda(17)
      real*4      ramda1(17), Fn0(17), Fn1(17), Fn2(17), Fn3(17)
      real*4      F0(17), F1(17), F2(17), F3(17), F4(17)
      real*4      eta(17), Fn(17), rhoin(17), rhoout(17), rhol(17)
      real*4      rhoh(17), rhos(17), amass(17)
      real*4      blat(17)
      real*4      rei(120), fei(120), alphai(120), betai(120)
      real*4      Ai(120), Bi(120), caii(120), ramdai(120)
      real*4      rej(120), fej(120), alphaj(120), betaj(120)
      real*4      Aj(120), Bj(120), caij(120), ramdaj(120)
      real*4      alatmax, rhoemax, r, rc, rst, dr, fmax
      real*8      f, fvalue
      real*4      rhor(5000,17), psi1, psi2, psi, z2r(5000,17,17)
      real*4      Fr(5000,17), emb, rho, psia, psib, fe1, fe2
            
      character*2  name(20)
      character*80 struc
      character*64 dummy, outelem
      
      open(10,FILE='EAMpara.dat',err=99,form='formatted', status='OLD',
     $     access='sequential')
      

      read(10,'(I2,1X,I3)')num_ele, num_pair
      read(10,'(A64)')dummy

      do i=1, num_ele
         read(10,'(A2,1X,I2,1X,F10.6)')name(i),ielement(i), amass(i)
         read(10,'(A64)')dummy
         read(10,'(A64)')dummy
         read(10,'(4(F10.6,1X))')re(i),fe(i),alpha(i),beta(i)
         read(10,'(A64)')dummy
         read(10,'(4(F10.6,1X))')A(i),B(i),cai(i),ramda(i)
         read(10,'(A64)')dummy
         read(10,'(A64)')dummy
         read(10,'(4(F10.6,1X))')Fn0(i),Fn1(i),Fn2(i),Fn3(i)
         read(10,'(A64)')dummy
         read(10,'(5(F10.6,1X))')F0(i),F1(i),F2(i),F3(i),F4(i)
         read(10,'(A64)')dummy
         read(10,'(4(F10.6,1X))')eta(i),Fn(i),beta1(i),ramda1(i)
         read(10,'(A64)')dummy
         read(10,'(4(F10.6,1X))')rhol(i),rhoh(i),rhoe(i),rhos(i)
         read(10,'(A64)')dummy
         blat(i) = sqrt(2.0)*re(i)
         rhoin(i) = rhol(i)*rhoe(i)
         rhoout(i) = rhoh(i)*rhoe(i)
      end do

      if(num_pair >= 1) then
         do i=1, num_pair
            read(10,'(A64)')dummy
            read(10,'(A64)')dummy
            read(10,'(A64)')dummy
            read(10,'(4(F10.6,1X))')rei(i),fei(i),alphai(i),betai(i)
            read(10,'(A64)')dummy
            read(10,'(4(F10.6,1X))')Ai(i),Bi(i),caii(i),ramdai(i)
            read(10,'(A64)')dummy
            read(10,'(6(F10.6,1X))')rej(i),fej(i),alphaj(i),betaj(i)
            read(10,'(A64)')dummy
            read(10,'(4(F10.6,1X))')Aj(i),Bj(i),caij(i),ramdaj(i)
            read(10,'(A64)')dummy
         end do
      endif
         
      nr=2000
      nrho=2000
      alatmax=blat(1)
      rhoemax=rhoe(1)
      do i=1, num_ele
         if(alatmax < blat(i)) alatmax = blat(i)
         if(rhoemax < rhoe(i)) rhoemax = rhoe(i)
      end do

      rc=sqrt(10.0)/2.0*alatmax
      rst=0.5
      dr=rc/(nr-1.0)
      fmax=-1.0

C----- 2-body part -----C
C      same element     C
C-----------------------C
      do i1=1, num_ele
         do i=1, nr
            r = (i-1.0)*dr
            if (r < rst) r=rst
            f=fe(i1)*exp(-beta1(i1)*(r/re(i1)-1.0))
            fvalue=f/(1.0+(r/re(i1)-ramda1(i1))**20)
            if (fmax < fvalue) fmax = fvalue
            rhor(i,i1) = fvalue
            psi1=A(i1)*exp(-alpha(i1)*(r/re(i1)-1.0))
            psi1=psi1/(1.0+(r/re(i1)-cai(i1))**20)
            psi2=B(i1)*exp(-beta(i1)*(r/re(i1)-1.0))
            psi2=psi2/(1.0+(r/re(i1)-ramda(i1))**20)
            psi=psi1-psi2
            z2r(i,i1,i1)=r*psi
         end do
      end do

C----- 2-body part -----C
C   different element   C
C-----------------------C
      ct = 0
      do i1=2, num_ele
         do i2=1, i1-1
            ct = ct+1
            do i=1, nr
               r = (i-1.0)*dr
               if (r < rst) r=rst
               psi1=A(i1)*exp(-alpha(i1)*(r/re(i1)-1.0))
               psi1=psi1/(1.0+(r/re(i1)-cai(i1))**20)
               psi2=B(i1)*exp(-beta(i1)*(r/re(i1)-1.0))
               psi2=psi2/(1.0+(r/re(i1)-ramda(i1))**20)
               psia=psi1-psi2

               psi1=A(i2)*exp(-alpha(i2)*(r/re(i2)-1.0))
               psi1=psi1/(1.0+(r/re(i2)-cai(i2))**20)
               psi2=B(i2)*exp(-beta(i2)*(r/re(i2)-1.0))
               psi2=psi2/(1.0+(r/re(i2)-ramda(i2))**20)
               psib=psi1-psi2

               f=fei(ct)*exp(-betai(ct)*(r/rei(ct)-1.0))
               fe1=f/(1.0+(r/rei(ct)-ramdai(ct))**20)
               f=fej(ct)*exp(-betaj(ct)*(r/rej(ct)-1.0))
               fe2=f/(1.0+(r/rej(ct)-ramdaj(ct))**20)

               psi=0.5*(fe2/fe1*psia+fe1/fe2*psib)

               z2r(i,i1,i2)=r*psi
               z2r(i,i2,i1)=z2r(i,i1,i2)
            end do
         end do
      end do

C----- Many body part -----C

      rhom=fmax
      if (rhom < 2.0*rhoemax) rhom=2.0*rhoemax
      if (rhom < 100.0) rhom=100.0
      drho=rhom/(nrho-1.0)
      do it=1,num_ele
         do i=1,nrho
            rhoF=(i-1.0)*drho
            if (rhoF < rhoe(it)) then
               Fm33=F3(it)
            else 
               Fm33=F4(it)
            endif
            if (rhoF <  rhoin(it)) then
               emb=Fn0(it)+
     $              Fn1(it)*(rhoF/rhoin(it)-1.0)+
     $              Fn2(it)*(rhoF/rhoin(it)-1.0)**2+
     $              Fn3(it)*(rhoF/rhoin(it)-1.0)**3
            else if (rhoF < rhoout(it)) then
               emb=F0(it)+
     $              F1(it)*(rhoF/rhoe(it)-1.0)+
     $              F2(it)*(rhoF/rhoe(it)-1.0)**2+
     $              Fm33*(rhoF/rhoe(it)-1.0)**3
            else
               emb=Fn(it)*(1.0-eta(it)*log(rhoF/rhos(it)))*
     $              (rhoF/rhos(it))**eta(it)
            endif
            Fr(i,it)=emb
         end do
      end do

C----- making the set file -----C

      open(1,FILE='EAMpara.set',form='formatted',status='unknown',
     $     access='sequential')

      outelem = trim(name(1))
      length = len_trim(outelem)
      if (num_ele > 1) then
         do i=2, num_ele
            outelem = outelem(1:length)//' '//trim(name(i))
            length=len_trim(outelem)
         end do
      endif
      struc='fcc'

      write(1,*)
      write(1,*)
      write(1,*)
      write(1,'(I5,A1,A24)')num_ele,' ',outelem
      write(1,'(I5,E24.16,I5,2(E24.16))')nrho,drho,nr,dr,rc
      do i=1,num_ele
         write(1,'(I5,2(G15.5),A8)')ielement(i),amass(i),blat(i),struc
         write(1,'(5(E24.16))')(Fr(j,i),j=1,nrho)
         write(1,'(5(E24.16))')(rhor(j,i),j=1,nr)
      end do

      do i1=1, num_ele
         do i2=1, i1
            write(*,*)'====',i1,i2,'====='
            write(*,'(5(E24.16))')(z2r(i,i1,i2),i=1,nr)
            write(1,'(5(E24.16))')(z2r(i,i1,i2),i=1,nr)
        end do
      end do

      close(10)
      return

 99   write(*,*)'Can not open the file of EAMparam.dat'

      stop
      end
