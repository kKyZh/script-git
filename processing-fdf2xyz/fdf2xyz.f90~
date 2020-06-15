      program fdf2xyzf90
        
        ! module
        ! choose file
        ! check file
        ! open file
        ! read file
        ! write file
        ! order change
        ! read element
        ! renew element
        ! write element
        ! allocate

        use m_fdf2xyz_inputfilename
        use m_fdf2xyz_print
        use m_fdf2xyz_readfile
        use jsu_readline

        implicit none

        logical chk_fil ! file exit or not feedback
        character(128) content
        character(256) line
        character(256) filename
        
        write(*,*)
        write(*,*) '----------------------------------------'
        content = "fdf2xyz --version 1.0-- //7.Apr.2020//"
        call fdf2xyz_print(content)

        do
        call iso_readline(line, ' (stop the loop : quit) ==>')

        if(line .eq. "quit") stop

        filename = line
        call fdf2xyz_inputfilename(filename, chk_fil)

        if(chk_fil .eq. .true.) then
        call fdf2xyz_readfile(filename)
        endif

        enddo

      endprogram fdf2xyzf90
