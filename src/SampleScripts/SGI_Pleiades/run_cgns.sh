#PBS -S /bin/csh
#PBS -l select=1024:ncpus=16:mpiprocs=16:model=san
#PBS -l walltime=00:15:00
#PBS -j oe
#PBS -q low
#PBS -W group_list=a1519

module load mpi-sgi/mpt.2.12r26 
module load comp-intel/2016.2.181

# By default, PBS executes your job from your home directory.
# However, you can use the environment variable
# PBS_O_WORKDIR to change to the directory where
# you submitted your job.

cd $PBS_O_WORKDIR

mpiexec_mpt ./benchmark_hdf5

# -end of script-
