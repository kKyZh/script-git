      module testf

        use iso_c_binding
        implicit none
        private
        public iso_readline
        public Freadline

        interface
          subroutine Freadline(ilen, string, prompt)&
              bind(C, name="FCreadline")
            use iso_c_binding 
            implicit none
            integer(kind=c_int) ,intent(in) :: ilen
            character(kind=c_char), intent(out) :: string(*)
            character(kind=c_char), intent(in) :: prompt(*)
          endsubroutine Freadline
        endinterface

        contains
          subroutine iso_readline(string, prompt)
            use iso_c_binding
            implicit none
            character(kind=c_char, len=*), intent(out) :: string
            character(kind=c_char, len=*), intent(in) :: prompt 

            call Freadline(len(string), string,&
            prompt(:len_trim(prompt))//achar(0))

          endsubroutine iso_readline

      endmodule testf
