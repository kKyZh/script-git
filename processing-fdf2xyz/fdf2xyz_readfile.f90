      module m_fdf2xyz_readfile

        private
        public fdf2xyz_readfile

        contains

          subroutine fdf2xyz_readfile(filename)

            use m_fdf2xyz_print
            use m_read2end
            use m_fdf2xyz_allocatable
            use m_readcharlen
            use m_readatomname
            use m_userread
            use m_writefilecheck

            implicit none

            integer i, j
            integer ln
            integer fileindex
            integer fileindexout
            integer tot_at_nu
            integer charlen
            integer output_charlen
            integer nu_at_nam
            integer, allocatable :: at_ind(:)
            real, allocatable :: pos_x(:), pos_y(:), pos_z(:)
            character(128) dummy
            character(*), intent(in) :: filename
            character(:), allocatable :: filewrite
            character(:), allocatable :: at_nam(:)
            character(:), allocatable :: r_at_nam(:)
            character(:), allocatable :: output_at_nam(:)

            fileindex = 100

            open(fileindex, file=trim(filename), &
              form='formatted', err=999, &
              status='old', access='sequential')

            write(*,*)
            write(*,*) &
              "File opened : ", trim(filename)

            call read2end(fileindex, ln)

            tot_at_nu = ln - 2

            call readcharlen(fileindex, tot_at_nu, charlen)

            call alloc_int( tot_at_nu, at_ind )
            call alloc_real( tot_at_nu, pos_x , &
              pos_y, pos_z )
            call alloc_char( tot_at_nu, at_nam, charlen)
            call alloc_char( tot_at_nu, r_at_nam, charlen)

            call readatomname(fileindex, tot_at_nu, charlen, &
              r_at_nam, at_nam, nu_at_nam)

            !write(*,'(i0)') nu_at_nam

            !do i = 1, nu_at_nam
            !write(*,'(a)') r_at_nam(i)
            !enddo

            ! chemical element has only 2 maximum digit position
            output_charlen = 2
            call alloc_char( nu_at_nam, output_at_nam, output_charlen )

            call userread( r_at_nam, charlen, output_at_nam, &
              output_charlen, nu_at_nam )

            read(fileindex,'(a)') dummy
            do i = 1, tot_at_nu
            read(fileindex,'(3(1x, f11.6), 1x, i2, 1x, (a))') & 
              pos_x(i), pos_y(i), pos_z(i), at_ind(i), at_nam(i)
            !write(*,'((a), 4x, 3(1x, f11.6))') & 
            !  at_nam(i), pos_x(i), pos_y(i), pos_z(i)
            enddo
            read(fileindex,'(a)') dummy

            rewind(fileindex)

            !do i = 1, nu_at_nam
            !write(*,*) output_at_nam(i)
            !enddo

            call writefilecheck( filename, fileindex, &
              filewrite, fileindexout )

            ! write head in filewrite, and fileindexout
            write(fileindexout,'(2x, i0)') tot_at_nu
            write(fileindexout,*)

            do i = 1, tot_at_nu
            do j = 1, nu_at_nam
            if (trim(adjustl(at_nam(i))) &
              .eq. trim(adjustl(r_at_nam(j)))) then
              at_nam(i) = output_at_nam(j)
              ! trim(adjustl()) can eliminate troubles sometime
            endif
            enddo
            ! origin .xyz has only one digit for element
            ! left 2 digit position as at_nam, -> 4x to 3x
            ! space is not important, due to different name
            write(fileindexout, &
              '(a<output_charlen>, 3x, 3(1x, f11.6))') & 
              adjustl(at_nam(i)), pos_x(i), pos_y(i), pos_z(i)
            enddo

            write(*,*)
            write(*,*)filewrite, ' File created'

            call dealloc_int( at_ind )
            call dealloc_real( pos_x, pos_y, pos_z )
            call dealloc_char( at_nam, charlen)
            call dealloc_char( r_at_nam, charlen)
            call dealloc_char( output_at_nam, output_charlen)

            goto 9999

999   continue
      call fdf2xyz_print("File can not be opened")
9999  continue

          endsubroutine fdf2xyz_readfile

        endmodule
