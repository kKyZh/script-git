#!/bin/sh
#PBS -q A_004
#PBS -l walltime=72:00:00
#PBS -l select=1
#PBS -N va-29-360
#PBS -m be
#PBS -M zhang.qinqiang@rift.mech.tohoku.ac.jp
cd ${PBS_O_WORKDIR}
 
ulimit -s unlimited
mpirun -np 10 -ppn 10 -hostfile $PBS_NODEFILE /usr/local/app/VASP5/current/bin/vasp_gpu > vasp.out
