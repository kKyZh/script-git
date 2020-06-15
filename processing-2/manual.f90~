      module m_manual

        implicit none
        private
        public manual

        contains
          subroutine manual

           write(*,*)'#########################################'
           write(*,*)
           write(*,*)'    ########################'
           write(*,*)'    #                      #'
           write(*,*)'    #        Manual        #'
           write(*,*)'    #                      #'
           write(*,*)'    ########################'
           write(*,*)
           write(*,*)'How to run the whole process ==>'
           write(*,*)
           write(*,*)'------------ SIESTA ------------'
           write(*,*)
           write(*,*)'1. car2Lammpsf90'
           write(*,*)'    ==> Need *.car file'
           write(*,*)'    ==> Create data.lammps'
           write(*,*)
           write(*,*)'2. crtfil4lammpsf90'
           write(*,*)'    ==> Need data.lammps'
           write(*,*)'    ==> Create in.min'
           write(*,*)
           write(*,*)'3. copy CH.airebo potential file'
           write(*,*)'    ==> For C-H bonding length'
           write(*,*)
           write(*,*)'4. lammps (crt4run_sh)'
           write(*,*)'    ==> Need in.min'
           write(*,*)'    ==> Create dump.GNR'
           write(*,*)
           write(*,*)'5. getdumpf90 (old) or lmpdum2posf90 (new)'
           write(*,*)'    ==> Need dump.GNR'
           write(*,*)'    ==> Create getdata.lammps'
           write(*,*)'    ==> Create position.fdf'
           write(*,*)
           write(*,*)'6. crtfil4trasief90'
           write(*,*)'    ==> Need getdata.lammps'
           write(*,*)'    ==> Need position.fdf'
           write(*,*)'    ==> Create s_*.fdf input file'
           write(*,*)'        (Need to change name to s_*.fdf/lammps)'
           write(*,*)
           write(*,*)'7. copy and rename C.psf & H.psf'
           write(*,*)
           write(*,*)'--- TranSIESTA for Electrode ---'
           write(*,*)
           write(*,*)'1. car2Lammpsf90'
           write(*,*)'    ==> Need *.car file'
           write(*,*)'    ==> Create data.lammps'
           write(*,*)
           write(*,*)'2. crtfil4lammpsf90'
           write(*,*)'    ==> Need data.lammps'
           write(*,*)'    ==> Create in.min'
           write(*,*)
           write(*,*)'3. copy CH.airebo potential file'
           write(*,*)'    ==> For C-H bonding length'
           write(*,*)
           write(*,*)'4. lammps (crt4run_sh)'
           write(*,*)'    ==> Need in.min'
           write(*,*)'    ==> Create dump.GNR'
           write(*,*)
           write(*,*)'5. getdumpf90 (old) or lmpdum2posf90 (new)'
           write(*,*)'    ==> Need dump.GNR'
           write(*,*)'    ==> Create getdata.lammps'
           write(*,*)'    ==> Create position.fdf'
           write(*,*)
           write(*,*)'6. crtfil4trasief90'
           write(*,*)'    ==> Need getdata.lammps'
           write(*,*)'    ==> Need position.fdf'
           write(*,*)'    ==> Create tse_*.fdf input file'
           write(*,*)'        (Need to change name to tse_*.fdf/lammps)'
           write(*,*)
           write(*,*)'7. copy and rename C.psf & H.psf'
           write(*,*)
           write(*,*)'--- TranSIESTA for Scattering ---'
           write(*,*)
           write(*,*)'1. getdumpf90'
           write(*,*)'    ==> Rename number of atom types respectively' 
           write(*,*)
           write(*,*)'2. copy'
           write(*,'(1x,3a)')&
             '    ==> Copy tse_getdata.lammps, tse_position.fdf,',&
             ' tse_*.TSHS, tse_*.fdf, tse_*.xyz, tse_*.STRUCT_OUT ',&
             ' (After relaxation) to the directory of SIESTA run'
           write(*,*)'    (After rename, copy all files recommanded)'
           write(*,*)
           write(*,*)'3. mer4fdff90'
           write(*,*)'    ==> Need s_getdata.lammps'
           write(*,*)'    ==> Need s_position.fdf'
           write(*,*)'    ==> Need tse_getdata.lammps'
           write(*,*)'    ==> Need tse_position.fdf'
           write(*,*)'    ==> Create getdata.lammps'
           write(*,*)'    ==> Create position.fdf'
           write(*,*)'    (Only merge position coordinates not relaxed)'
           write(*,*)
           write(*,*)'4. crtfil4trasief90'
           write(*,*)'    ==> Need tse_*.fdf input file'
           write(*,*)'    ==> Need getdata.lammps'
           write(*,*)'    ==> Need position.fdf'
           write(*,*)'    ==> Create tss_*.fdf input file'
           write(*,*)'    ==> Create tss_position.fdf'
           write(*,*)'    (After mer4fdff90 run)'
           write(*,*)'    (Need to choose transiesta)'
           write(*,*)'    (Need to correct atom types)'
           write(*,*)'    (Need to change name to tss_*.fdf/lammps)'
           write(*,*)
           write(*,*)'5. xyz2fdff90'
           write(*,*)'    ==> Need s_*.xyz'
           write(*,*)'    ==> Need tse_*.xyz'
           write(*,*)'    ==> Need s_*.STRUCT_OUT'
           write(*,*)'    ==> Need tse_*.STRUCT_OUT'
           write(*,*)'    ==> Need tss_*.fdf input file'
           write(*,*)'    (*.xyz files have an order)'
           write(*,*)'    (First is from SIESTA)'
           write(*,*)'    (Second is from TranSIESTA)'
           write(*,*)'    (Correct coordinates after relaxation)'
           write(*,*)'    (Correct cell vectors after relaxation)'
           write(*,*)
           write(*,*)'------ Copy & Submit job ------'
           write(*,*)
           write(*,*)'1. crt4run_sh'
           write(*,*)
           write(*,*)'2. run'
           write(*,*)
           write(*,*)'------ Support VASP for GPU ------'
           write(*,*)
           write(*,*) 'position file transfer & strain'
           write(*,*)
           write(*,*)'#########################################'

         endsubroutine manual
       endmodule m_manual
