      module m_userread

        private
        public userread

        contains

        subroutine userread( r_at_nam, charlen, output_at_nam, &
            output_charlen, nu_at_nam )

        use m_fdf2xyz_allocatable
        use jsu_readline

          implicit none

          integer i
          integer, intent(in) :: nu_at_nam
          integer, intent(in) :: charlen
          integer input_charlen
          integer, intent(in) :: output_charlen
          character(:), intent(in), allocatable :: r_at_nam(:)
          character(:), allocatable :: input_at_nam(:)
          character(:), intent(inout), allocatable :: output_at_nam(:)
          character(128) input_dummy

          input_charlen = len(input_dummy)
          ! allocate input and output separately
          ! input for check length
          call alloc_char( nu_at_nam, input_at_nam, input_charlen )

          do i = 1, nu_at_nam
          write(*,*)
          write(*,*) 'Please enter new species name'

100       continue

          ! directly pass array to C and interface then return back
          call userreadline(input_at_nam(i), &
            ''//trim(adjustl(r_at_nam(i)))//' -> ')

          if (len_trim(input_at_nam(i)) .eq. 0 ) then
          write(*,*) 'ERROR: Please enter at least one character'
          goto 100
          elseif(len_trim(input_at_nam(i)) .gt. 2) then
          write(*,*) &
            'ERROR: Please enter chemical element character <= 2'
          goto 100
          endif

          enddo

          do i = 1, nu_at_nam
          output_at_nam(i) = input_at_nam(i)
          !write(*,*)output_at_nam(i)
          enddo

          call dealloc_char( input_at_nam, input_charlen)

        endsubroutine userread

      endmodule m_userread
