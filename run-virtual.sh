#!/bin/bash
ulimit -s unlimited
nohup mpirun -np 8 ~/siesta-4.0/Obj/transiesta < tss_29112005.fdf > tss_29112005.out &
