      module jsu_readline

        use iso_c_binding
        implicit none
        private
        public iso_readline
        public Freadline
        public userreadline
        public Fuserreadline

        interface
          subroutine Freadline(ilen, string, prompt)&
              bind(C, name="FCreadline")
            use iso_c_binding 
            implicit none
            integer(kind=c_int) ,intent(in) :: ilen
            character(kind=c_char), intent(out) :: string(*)
            character(kind=c_char), intent(in) :: prompt(*)
          endsubroutine Freadline

          subroutine Fuserreadline(ilen, string, prompt)&
              bind(C, name="FCuserreadline")
            use iso_c_binding 
            implicit none
            integer(kind=c_int) ,intent(in) :: ilen
            character(kind=c_char), intent(out) :: string(*)
            character(kind=c_char), intent(in) :: prompt(*)
          endsubroutine Fuserreadline
        endinterface

        contains
          subroutine iso_readline(string, prompt)
            use iso_c_binding
            implicit none
            character(kind=c_char, len=*), intent(out) :: string
            character(kind=c_char, len=*), intent(in) :: prompt 

            call Freadline(len(string), string,&
            prompt(:len_trim(prompt))//achar(0))
          ! // achar(0) for trancate all spaces after prompt last letter

          endsubroutine iso_readline

          subroutine userreadline(string, prompt)
            use iso_c_binding
            implicit none
            character(kind=c_char, len=*), intent(out) :: string
            character(kind=c_char, len=*), intent(in) :: prompt 

            call Fuserreadline(len(string), string,&
            prompt(:len(prompt))//achar(0))
            !prompt(:len_trim(prompt))//achar(0))
          ! // achar(0) for trancate all spaces after prompt last letter

          endsubroutine userreadline
      endmodule jsu_readline
