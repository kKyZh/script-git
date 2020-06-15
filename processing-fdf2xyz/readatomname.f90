      module m_readatomname

        private
        public readatomname

        contains

          subroutine readatomname(fileindex, tot_at_nu, charlen, & 
              r_at_nam, at_nam, nu_at_nam)

            use m_fdf2xyz_allocatable

            implicit none

            integer i, j, k, l
            integer, intent(in) :: fileindex
            integer, intent(in) :: tot_at_nu
            integer, intent(in) :: charlen
            integer, intent(out) :: nu_at_nam
            character(:), allocatable :: at_nam(:)
            character(:), intent(inout), allocatable :: r_at_nam(:) ! reservoir atom
            character(128) dummy

            read(fileindex, '(a)') dummy

            k = 1 ! at least one species

            do i = 1, tot_at_nu

            read(fileindex, '(a40, (a))') dummy, at_nam(i)

            if( i .eq. 1) then
              r_at_nam(k) = trim(adjustl(at_nam(i)))
            endif

            if( i .gt. 1) then

              ! with / without adjustl(trim()) no significant effort
              l = 1 ! check different species
              do j = 1, k
              if( trim(adjustl(r_at_nam(j))) &
                .ne. trim(adjustl(at_nam(i)))) then
                l = l + 1
              endif
              enddo

              !write(*,*)'i,l,k, r_at, at', i,l,k,'!',&
              !  trim(adjustl(r_at_nam(k))), trim(adjustl(at_nam(i))),'!'

              if(l .gt. k) then
                k = k + 1
                r_at_nam(k) = trim(adjustl(at_nam(i)))
                !write(*,*)k
              endif
              !write(*,*)at_nam(i), r_at_nam(k), l, k, r_at_nam(k), charlen
            endif


            enddo

            nu_at_nam = k

            rewind(fileindex)

          endsubroutine readatomname

      endmodule m_readatomname
