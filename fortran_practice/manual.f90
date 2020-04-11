      program manualf90
        implicit none
        integer i,j,nna,narg,lna,larg
        character(32) na

        nna=0
        i=1
        lna=2

        narg=command_argument_count()
        if(narg==0)then
          write(*,*)'call manual: -h'
          goto 999
        else
        do i=1,narg
        call get_command_argument(i,na)
            larg=len_trim(na)
            if(na(1:1)=='-')then
              do lna=2,larg
              if(na(lna:lna)=='h')then
                write(*,*)'manual'
                goto 999
              endif
              enddo
              nna=nna+1
            else
              nna=nna+1
            endif
        enddo
        endif
        if(narg==nna)then
          write(*,*)'------'
          write(*,*)'No available options'
          write(*,*)'call manual: -h'
          goto 999
        else
        endif

999     stop

      endprogram manualf90
