/* 
 * A set of parallel timer convenience functions
 *
 * Copyright (c) DoD HPCMP PETTT.  All rights reserved.  
 * See LICENSE file for details.
 */

#include <math.h>
#include <mpi.h>

/* Initialize a timer
 *    timer: pointer to timer to initialize
 *    comm: communicator for barrier
 *    barrier: flag, 0=no barrier, 1=perform barrier */

void timer_tick(double *timer, MPI_Comm comm, int barrier)
{
    if(barrier)
        MPI_Barrier(comm);
    *timer = MPI_Wtime();
}

/* Statistics structure: contains stats that aggregate timer
 *    values from all ranks */

struct timer_statinfo {
    double min;
    double max;
    double mean;
    double std;
};

/* Stop a timer and record elapsed time
 *    timer: pointer to timer; will contain elapsed time of rank when done */

void timer_tock(double *timer)
{
    double t2 = MPI_Wtime();
    *timer = t2 - *timer;
}

/* Collect statistics of timers on all ranks
 *    timer: elapsed time for rank
 *    comm: communicator for collecting stats
 *    destrank: the rank to which to collect stats
 *    stats: pointer to timerstats */

void timer_collectstats(double timer, MPI_Comm comm, int destrank, struct timer_statinfo *stats)
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

/* Print time statistics
 *    prefix: string to print in front of stats, use "" for no string
 *    stats: pointer to timerstats */

void timer_printstats(char *prefix, struct timer_statinfo *stats)
{
    printf("%s timer seconds mean = %.2f, min = %.2f, max = %.2f, std = %.3f\n",
           prefix, stats->mean, stats->min, stats->max, stats->std);
}

/* Collect statistics of timers on all ranks to one rank and print from that rank
 *    timer: elapsed time for rank
 *    comm: communicator for collecting stats
 *    destrank: the rank to which to collect stats
 *    prefix: string to print in front of stats, use "" for no string */

void timer_collectprintstats(double timer, MPI_Comm comm, int destrank, char *prefix)
{
    int rank;
    struct timer_statinfo stats;

    MPI_Comm_rank(comm, &rank);
    timer_collectstats(timer, comm, destrank, &stats);
    if(rank == destrank)
        timer_printstats(prefix, &stats);
}

