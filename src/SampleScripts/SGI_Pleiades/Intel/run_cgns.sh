#PBS -S /bin/csh
###PBS -N cfd
# This example uses the Sandy Bridge nodes
# User job can access ~31 GB of memory per Sandy Bridge node.
# A memory intensive job that needs more than ~1.9 GB
# per process should use less than 16 cores per node
# to allow more memory per MPI process. This example
# asks for 32 nodes and 8 MPI processes per node.
# This request implies 32x8 = 256 MPI processes for the job.
#PBS -l select=4:ncpus=16:mpiprocs=16:model=san
#PBS -l walltime=00:15:00
#PBS -j oe
#PBS -q devel
#PBS -W group_list=a1521
##PBS -W group_list=a0801
##PBS -m e

# Currently, there is no default compiler and MPI library set.
# You should load in the version you want.
# Currently, MVAPICH or SGI's MPT are available in 64-bit only,
# you should use a 64-bit version of the compiler.

#module load mpi-intel/5.0.1.035
#module load comp-intel/2013.5.192
#module load comp-intel/2013.5.192
#module load mpi-sgi/mpt.2.11r13
module load comp-intel/2015.0.090
module load mpi-sgi/mpt.2.11r13

# By default, PBS executes your job from your home directory.
# However, you can use the environment variable
# PBS_O_WORKDIR to change to the directory where
# you submitted your job.

cd $PBS_O_WORKDIR

# use of dplace to pin processes to processors may improve performance
# Here you request to pin processes to processors 4-11 of each Sandy Bridge node.
# For other processor types, you may have to pin to different processors.

# The resource request of select=32 and mpiprocs=8 implies
# that you want to have 256 MPI processes in total.
# If this is correct, you can omit the -np 256 for mpiexec
# that you might have used before.
#setenv MPICH_MPIIO_HINTS "*:romio_cb_write=disable:romio_ds_write=disable"
#setenv MPICH_MPIIO_HINTS_DISPLAY 1

#mpiexec_mpt ./ptests/benchmark_hdf5

/nasa/intel/impi/5.0.1.035/bin64/mpiexec.hydra -env I_MPI_EXTRA_FILESYSTEM on -env I_MPI_EXTRA_FILESYSTEM_LIST lustre ./ptests/benchmark_hdf5 

# It is a good practice to write stderr and stdout to a file (ex: output)
# Otherwise, they will be written to the PBS stderr and stdout in /PBS/spool,
# which has limited amount  of space. When /PBS/spool is filled up, any job
# that tries to write to /PBS/spool will die.

# -end of script-

