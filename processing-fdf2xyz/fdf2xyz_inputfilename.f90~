      module m_fdf2xyz_inputfilename

        private
        public :: fdf2xyz_inputfilename

        contains

          subroutine fdf2xyz_inputfilename(filename, filexist)

          use m_fdf2xyz_print

            implicit none

            character(128) content
            character(len=*), intent(in), optional ::  filename

            logical, intent(out) :: filexist

            if(len_trim(filename) .eq. 0) then
              call fdf2xyz_print("")

            else

            inquire (file=adjustl(trim(filename)),&
              err= 999, exist= filexist)

            if(filexist .eq. .true.)then
              call fdf2xyz_print("File exist")
            elseif(filexist .eq. .false.)then
              call fdf2xyz_print("File not exist")
            endif

            endif

            goto 9999

999     continue

          filexist = .false.
          call fdf2xyz_print("File can not be opened")

9999    continue

          endsubroutine fdf2xyz_inputfilename

      endmodule m_fdf2xyz_inputfilename
