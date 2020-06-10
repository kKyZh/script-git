      module m_strain_get

      private
      public strain_get

      contains

      subroutine strain_get( strain_chs, num_fil, &
          strain_lo, strain_hi, strain_in, &
          len_strain_nam, len_fra_strain_nam, len_fra_percent)

        use jsu_readline

        !----------------------------------
        character(128) string ! userreadline
        integer check ! check for strain
        integer m
        integer len_int_strain_nam
        integer, intent(out) :: strain_chs, strain_lo, strain_hi
        integer, intent(out) :: num_fil
        integer, intent(out), optional :: len_fra_strain_nam
        integer, intent(out), optional :: len_fra_percent
        integer, intent(out), optional :: len_strain_nam 
        real, intent(out) :: strain_in
        real strain_nam, strain_i, real_check
        character(32) strain_chr_in, strain_chr_nam
        !----------------------------------

        ! ############################## strain or not
91      continue
        write(*,*)
        write(*,'(3a)') 'Do you need strain?', &
          ' (position.fdf -> position_*.fdf) (* strain without %)', &
          ' (POSCAR -> mkdir strain_* -> ./strain_*/POSCAR)'
        call userreadline( string, '(1. yes, 2. no) : ')
        read( string, *, iostat=check) strain_chs
        if( check .ne. 0) then
          goto 91
        elseif( strain_chs .eq. 1) then
          write(*,*)
          write(*,*) '1. yes, with strain'
199     continue
          write(*,*)
          write(*,*) 'Lowest strain (with %)'
          call userreadline( string, '(Integer) : ')
          read( string, *, iostat=check) strain_lo
          if( check .ne. 0) goto 199
          if( strain_lo .lt. 0) goto 199
          !only positive value availiable this time
198     continue
          write(*,*)
          write(*,*) 'Highest strain (with %)'
          call userreadline( string, '(Integer) : ')
          read( string, *, iostat=check) strain_hi
          if( check .ne. 0) goto 198
          if( strain_hi .lt. strain_lo) goto 198
          if( (strain_hi / 100) .ge. 10) then
            ! 100 + strain_hi
            len_int_strain_nam = len_trim(adjustl(string)) - 2 + 1
          elseif((strain_hi/100).ge.9 .and. (strain_hi/100).lt.10) then
            len_int_strain_nam = 3
            ! specific 900 + 100 = 1000 include .
          else
            len_int_strain_nam = 2
          endif
          if( strain_hi .eq. strain_lo) then
            strain_in = 1
            string = '1'
            goto  196
          endif
197     continue
          write(*,*)
          write(*,*) 'Interval (with %)'
          call userreadline( string, '(Decimal or Integer) : ')
          read( string, *, iostat=check) strain_in
          if( check .ne. 0) goto 197
          if( strain_in .gt. (strain_hi - strain_lo)) goto 197
          ! find remainder manually, amod, dmod, mod not work well
           real_check =( int(strain_hi - strain_lo) - &
           (( int((strain_hi - strain_lo) / strain_in)) * strain_in))
          ! find remainder manually, amod, dmod, mod not work well
          !write(*,*) real_check
          if( real_check .ne. 0) goto 197
          ! find length of fraction part, if len < 0 -> = 0
196     continue
          ! strain_lo = strain_hi
          len_strain_nam = len_trim(adjustl(string))
          write( strain_chr_in, '(f0.0)') strain_in
          len_fra_strain_nam = len_strain_nam - &
          len_trim(adjustl(strain_chr_in))
          if( len_fra_strain_nam .lt. 0) then
            len_fra_strain_nam = 0
          endif
          len_fra_percent = len_fra_strain_nam
          len_fra_strain_nam = len_fra_strain_nam + 2
          len_strain_nam = len_int_strain_nam + len_fra_strain_nam
          if( strain_lo .eq. strain_hi) then
            num_fil = 1
          else
            num_fil = ((strain_hi - strain_lo) / strain_in) + 1
          endif

          ! just show new directory created or not

          !############################## open write files

          !############################## open write files

          ! real can not add leading 0 but integer can
          ! do tricks from character for leading 0 real format
          ! real no blank leading spaces 

          ! ### no strain
        elseif( strain_chs .eq. 2) then
          write(*,*)
          write(*,*) '2. No, without strain'
          ! ### no strain -> only one file

        strain_i = 0.0
        strain_lo = 0.0
        strain_hi = 0.0
        strain_in = 0.0
        num_fil = 1
        strain_nam = (100 + strain_i) / 100.0

        ! ### not strain or no strain
        else
          goto 91
        endif
        ! ############################## strain or not finished







      endsubroutine
      endmodule m_strain_get
