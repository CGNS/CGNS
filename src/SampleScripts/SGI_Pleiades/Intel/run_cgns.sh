#PBS -S /bin/csh
#PBS -l select=4:ncpus=16:mpiprocs=16:model=san
#PBS -l walltime=01:00:00
#PBS -j oe
#PBS -q debug
#PBS -W group_list=a1519

module purge
module load comp-intel/2015.0.090
module load mpi-intel/5.0.1.035

# By default, PBS executes your job from your home directory.
# However, you can use the environment variable
# PBS_O_WORKDIR to change to the directory where
# you submitted your job.

cd $PBS_O_WORKDIR

#mpiexec_mpt ./benchmark_hdf5

/nasa/intel/impi/5.0.1.035/bin64/mpiexec.hydra -env I_MPI_EXTRA_FILESYSTEM on -env I_MPI_EXTRA_FILESYSTEM_LIST lustre ./benchmark_hdf5

# -end of script-
