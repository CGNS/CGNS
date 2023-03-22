/* 
 * A set of parallel timer convenience functions
 *
 * Copyright (c) DoD HPCMP PETTT.  All rights reserved.
 * Modified https://github.com/PETTT/miniIO
 * See LICENSE file for details.
 *
 */

#include <math.h>
#include <mpi.h>

#define t_barrier 1
#define t_nobarrier 0

/* Statistics structure: contains stats that aggregate timer
 * values from all ranks */

struct timer_statinfo {
  double min;
  double max;
  double mean;
  double std;
};

/* Start timer
 *    timer   - pointer to timer to initialize
 *    comm    - communicator for barrier
 *    barrier - flag, t_barrier or t_nobarrier
 */

void timer_start(double *timer, MPI_Comm comm, int barrier)
{
  if(barrier)
    MPI_Barrier(comm);

  *timer = MPI_Wtime();
}

/* Stop timer and return elapsed time
 *    timer   - pointer to timer; returns elapsed time of rank
 *    comm    - communicator for barrier
 *    barrier - flag, t_barrier or t_nobarrier
 */
void timer_end(double *timer, MPI_Comm comm, int barrier)
{
  if(barrier)
    MPI_Barrier(comm);

  double t2 = MPI_Wtime();
  *timer = t2 - *timer;
}

/* Collect statistics of timers on all ranks
 *    timer    - elapsed time for rank
 *    comm     - communicator for collecting stats
 *    destrank - the rank to which to collect stats
 *    stats    - pointer to timer stats
*/

void timer_stats(double timer, MPI_Comm comm, int destrank, struct timer_statinfo *stats)
{
    int rank, nprocs, i;
    double *rtimers=NULL;    /* All timers from ranks */

    MPI_Comm_rank(comm, &rank);
    MPI_Comm_size(comm, &nprocs);
    if(rank == destrank)  {
      rtimers = (double *) malloc(nprocs*sizeof(double));
      stats->mean = 0.;
      stats->min = timer;
      stats->max = timer;
      stats->std = 0.f;
    }
    MPI_Gather(&timer, 1, MPI_DOUBLE, rtimers, 1, MPI_DOUBLE, destrank, comm);
    if(rank == destrank) {
      for(i = 0; i < nprocs; i++) {
        if(rtimers[i] > stats->max)  stats->max = rtimers[i];
        if(rtimers[i] < stats->min)  stats->min = rtimers[i];
        stats->mean += rtimers[i];
      }
      stats->mean /= nprocs;
      for(i = 0; i < nprocs; i++)
        stats->std += (rtimers[i]-stats->mean)*(rtimers[i]-stats->mean);
      stats->std = sqrt(stats->std / nprocs);
      free(rtimers);
    }
}
