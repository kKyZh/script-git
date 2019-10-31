# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# User specific environment and startup programs
 
# PATH=$PATH:/usr/bin:/opt/gcc/7.3.0/bin
PATH=$PATH:$HOME/bin
PATH=$PATH:$HOME/usr/local/bin:$HOME/bin
PATH=$PATH:$HOME/Util/VESTA:$HOME/bin
PATH=$PATH:/usr/local/share:$HOME/usr/local/share
PATH=$PATH:$HOME/usr/local/share/vim
PATH=$PATH:$HOME/Util/gcc/gcc-9.1.0/bin
LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HOME/Util/gcc/gcc-9.1.0/lib64
LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HOME/Util/gcc/gcc-9.1.0/lib
LIBRARY_PATH=$LIBRARY_PATH:$HOME/Util/gcc/gcc-9.1.0/lib
LIBRARY_PATH=$LIBRARY_PATH:$HOME/Util/gcc/gcc-9.1.0/lib64

# export CXX=/opt/intel/compilers_and_libraries_2018.2.199/linux/bin/intel64/icpc
# export CC=/opt/intel/compilers_and_libraries_2018.2.199/linux/bin/intel64/icc
export CC=/opt/gcc/7.3.0/bin/gcc
export CXX=/opt/gcc/7.3.0/bin/g++
# export gcc=/opt/gcc/7.3.0/bin/gcc
# export g++=/opt/gcc/7.3.0/bin/g++
export PATH

# module switch from cray to intel ==> Zhang's
module switch PrgEnv-cray PrgEnv-intel
module unload cray-libsci
echo 
echo '---------------------------- Module switched from Cray to Intel -------------------------'
echo
ftn -V
echo module load gcc
module load gcc
which gcc
echo
echo '-----------------------------------------------------------------------------------------'
echo
jobtime
echo
echo '-----------------------------------------------------------------------------------------'
qstat -u boom9000
echo
echo '-----------------------------------------------------------------------------------------'
echo
