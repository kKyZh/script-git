      module m_readcharlen

        private
        public readcharlen

        contains

          subroutine readcharlen(fileindex, tot_at_nu, charlen)

            implicit none

            integer i
            integer, intent(in) :: fileindex
            integer, intent(in) :: tot_at_nu
            integer, intent(out) :: charlen
            character(128) dummy
            character(32) at_nu_char

            read(fileindex, '(a)') dummy

            do i = 1, tot_at_nu

            read(fileindex, '(a40, (a))') dummy, at_nu_char

            if(i .eq. 1) then
              charlen = len_trim(at_nu_char)
            endif

            if(charlen .lt. len_trim(at_nu_char)) then
              charlen = len_trim(at_nu_char)
            endif

            enddo

            read(fileindex, '(a)') dummy

            rewind(fileindex)

          endsubroutine readcharlen

      endmodule m_readcharlen
