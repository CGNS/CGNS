      program ftest

#ifdef WINNT
      include 'cgnswin_f.h'
#endif
      include 'cgnslib_f.h'
      include 'mpif.h'
#include "cgnstypes_f.h"

      integer totcnt
      parameter (totcnt = 40320 * 10)

      integer npp, commsize, commrank
      integer i, j, n, nb, nz, nerrs
      integer ierr, F, B, Z, E, S
      integer Cx, Cy, Cz, Fx, Fy, Fz, Ax, Ay, Az
      integer sizes(3), start, end
      real*8 ts, te, tt, dsize
      real*8 dx(totcnt), dy(totcnt), dz(totcnt)
      integer ie(4*totcnt)
      character*32 name
      character*11 piomode(2)
      character*6 outmode(2)
      
      data piomode /'independent','collective'/
      data outmode /'direct','queued'/

#if CG_BUILD_64BIT
      PRINT *,'PROGRAM NOT SET UP FOR 64-BIT'
      STOP
#endif
      
      call MPI_INIT(ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD,commsize,ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD,commrank,ierr)

      if (commsize .gt. 8) then
        if (commrank .eq. 0)
     .    print *, 'number of processes must be 8 or less'
        stop
      endif

      npp = totcnt / commsize;
      start = commrank * npp + 1;
      end   = start + npp - 1;

      j = 0;
      do n=1,npp
        dx(n) = start + n - 1
        dy(n) = commrank + 1
        dz(n) = n
        do i=1,4
          j = j + 1
          ie(j) = start + n - 1
        enddo
      enddo
 
      sizes(1) = totcnt
      sizes(2) = totcnt
      sizes(3) = 0

      dsize = (9.0 * totcnt * 8.0 + 4.0 * totcnt * 4.0) /
     &        (1024.0 * 1024.0);
     
      if (commrank .eq. 0) then
        print *,'number processes       =', commsize
        print *,'array size per process =', npp
        print *,'total array size       =', totcnt
        print *,'total Mb for all data  =', dsize
      endif

c default is MPI_COMM_WORLD, but can set another communicator with this
c     call cgp_mpi_comm_f(MPI_COMM_WORLD,ierr)

      call cgp_open_f('ftest.cgns',CG_MODE_WRITE,F,ierr)
      if (ierr .ne. CG_OK) call cgp_error_exit_f

      do nb=1,2
        write(name,'(a4,i2)') 'Base',nb
        call cg_base_write_f(F,name,3,3,B,ierr)
        if (ierr .ne. CG_OK) call cgp_error_exit_f
        call cgp_pio_mode_f(nb-1,ierr)
        if (ierr .ne. CG_OK) call cgp_error_exit_f

        do nz=1,2
          write(name,'(a4,i2)') 'Zone',nz
          call cg_zone_write_f(F,B,name,sizes,Unstructured,Z,ierr)
          if (ierr .ne. CG_OK) call cgp_error_exit_f
          call cgp_coord_write_f(F,B,Z,RealDouble,'CoordinateX',
     &                           Cx,ierr)
          if (ierr .ne. CG_OK) call cgp_error_exit_f
          call cgp_coord_write_f(F,B,Z,RealDouble,'CoordinateY',
     &                           Cy,ierr)
          if (ierr .ne. CG_OK) call cgp_error_exit_f
          call cgp_coord_write_f(F,B,Z,RealDouble,'CoordinateZ',
     &                           Cz,ierr)
          if (ierr .ne. CG_OK) call cgp_error_exit_f
          call cgp_section_write_f(F,B,Z,'Tets',TETRA_4,1,totcnt,
     &                             0,E,ierr)
          if (ierr .ne. CG_OK) call cgp_error_exit_f
          call cg_sol_write_f(F,B,Z,'Solution',Vertex,S,ierr)
          if (ierr .ne. CG_OK) call cgp_error_exit_f
          call cgp_field_write_f(F,B,Z,S,RealDouble,'MomentumX',
     &                           Fx,ierr)
          if (ierr .ne. CG_OK) call cgp_error_exit_f
          call cgp_field_write_f(F,B,Z,S,RealDouble,'MomentumY',
     &                           Fy,ierr)
          if (ierr .ne. CG_OK) call cgp_error_exit_f
          call cgp_field_write_f(F,B,Z,S,RealDouble,'MomentumZ',
     &                           Fz,ierr)
          if (ierr .ne. CG_OK) call cgp_error_exit_f
          call cg_goto_f(F,B,ierr,name,0,'end')
          if (ierr .ne. CG_OK) call cgp_error_exit_f
          call cg_user_data_write_f('User Data',ierr)
          if (ierr .ne. CG_OK) call cgp_error_exit_f
          call cg_gorel_f(F,ierr,'User Data',0,'end')
          if (ierr .ne. CG_OK) call cgp_error_exit_f
          call cgp_array_write_f('ArrayX',RealDouble,1,totcnt,
     &                           Ax,ierr)
          if (ierr .ne. CG_OK) call cgp_error_exit_f
          call cgp_array_write_f('ArrayY',RealDouble,1,totcnt,
     &                           Ay,ierr)
          if (ierr .ne. CG_OK) call cgp_error_exit_f
          call cgp_array_write_f('ArrayZ',RealDouble,1,totcnt,
     &                           Az,ierr)
          if (ierr .ne. CG_OK) call cgp_error_exit_f

          call cgp_queue_set_f(nz-1,ierr)
          if (ierr .ne. CG_OK) call cgp_error_exit_f
          call MPI_BARRIER(MPI_COMM_WORLD, ierr)
          ts = MPI_WTIME()

          call cgp_coord_write_data_f(F,B,Z,Cx,start,end,dx,ierr)
          if (ierr .ne. CG_OK) call cgp_error_exit_f
          call cgp_coord_write_data_f(F,B,Z,Cy,start,end,dy,ierr)
          if (ierr .ne. CG_OK) call cgp_error_exit_f
          call cgp_coord_write_data_f(F,B,Z,Cz,start,end,dz,ierr)
          if (ierr .ne. CG_OK) call cgp_error_exit_f
          call cgp_elements_write_data_f(F,B,Z,E,start,end,ie,ierr)
          if (ierr .ne. CG_OK) call cgp_error_exit_f
          call cgp_field_write_data_f(F,B,Z,S,Fx,start,end,dx,ierr)
          if (ierr .ne. CG_OK) call cgp_error_exit_f
          call cgp_field_write_data_f(F,B,Z,S,Fy,start,end,dy,ierr)
          if (ierr .ne. CG_OK) call cgp_error_exit_f
          call cgp_field_write_data_f(F,B,Z,S,Fz,start,end,dz,ierr)
          if (ierr .ne. CG_OK) call cgp_error_exit_f
          call cg_goto_f(F,B,ierr,'Zone_t',Z,'UserDefinedData_t',
     &                   1,'end')
          if (ierr .ne. CG_OK) call cgp_error_exit_f
          call cgp_array_write_data_f(Ax,start,end,dx,ierr)
          if (ierr .ne. CG_OK) call cgp_error_exit_f
          call cgp_array_write_data_f(Ay,start,end,dy,ierr)
          if (ierr .ne. CG_OK) call cgp_error_exit_f
          call cgp_array_write_data_f(Az,start,end,dz,ierr)
          if (ierr .ne. CG_OK) call cgp_error_exit_f

          if (nz .eq. 2) then
            call cgp_queue_flush_f(ierr)
            if (ierr .ne. CG_OK) call cgp_error_exit_f
          endif

          call MPI_BARRIER(MPI_COMM_WORLD, ierr)
          te = MPI_WTIME()
          tt = te - ts;
          if (commrank .eq. 0) then
            print *,'write:',tt,' secs,', dsize/tt, ' Mb/sec (',
     &               piomode(nb),',',outmode(nz),')'
          endif
        enddo
      enddo

      call cgp_queue_set_f(0,ierr);
      call cgp_close_f(F,ierr)
      if (ierr .ne. CG_OK) call cgp_error_exit_f

      call cgp_open_f('ftest.cgns',CG_MODE_READ,F,ierr)
      if (ierr .ne. CG_OK) call cgp_error_exit_f
    
      Z = 1
      S = 1
      E = 1;
      do B=1,2
        call cgp_pio_mode_f(B-1,ierr)
        if (ierr .ne. CG_OK) call cgp_error_exit_f

        call MPI_BARRIER(MPI_COMM_WORLD, ierr)
        ts = MPI_WTIME()

        call cgp_coord_read_data_f(F,B,Z,1,start,end,dx,ierr)
        if (ierr .ne. CG_OK) call cgp_error_exit_f
        call cgp_coord_read_data_f(F,B,Z,2,start,end,dy,ierr)
        if (ierr .ne. CG_OK) call cgp_error_exit_f
        call cgp_coord_read_data_f(F,B,Z,3,start,end,dz,ierr)
        if (ierr .ne. CG_OK) call cgp_error_exit_f
        call cgp_elements_read_data_f(F,B,Z,E,start,end,ie,ierr)
        if (ierr .ne. CG_OK) call cgp_error_exit_f

        call MPI_BARRIER(MPI_COMM_WORLD, ierr)
        te = MPI_WTIME()
        tt = te - ts

        nerrs = 0;
        j = 0;
        do n=1,npp
          if (dx(n) .ne. (start+n-1)) nerrs = nerrs + 1
          if (dy(n) .ne. (commrank+1)) nerrs = nerrs + 1
          if (dz(n) .ne. n) nerrs = nerrs + 1
          do i=1,4
            j = j + 1
            if (ie(j) .ne. (start+n-1)) nerrs = nerrs + 1
          enddo
        enddo

        call MPI_BARRIER(MPI_COMM_WORLD, ierr)
        ts = MPI_WTIME()

        call cgp_field_read_data_f(F,B,Z,S,1,start,end,dx,ierr)
        if (ierr .ne. CG_OK) call cgp_error_exit_f
        call cgp_field_read_data_f(F,B,Z,S,2,start,end,dy,ierr)
        if (ierr .ne. CG_OK) call cgp_error_exit_f
        call cgp_field_read_data_f(F,B,Z,S,3,start,end,dz,ierr)
        if (ierr .ne. CG_OK) call cgp_error_exit_f

        call MPI_BARRIER(MPI_COMM_WORLD, ierr)
        te = MPI_WTIME()
        tt = tt + te - ts

        do n=1,npp
          if (dx(n) .ne. (start+n-1)) nerrs = nerrs + 1
          if (dy(n) .ne. (commrank+1)) nerrs = nerrs + 1
          if (dz(n) .ne. n) nerrs = nerrs + 1
        enddo

        call MPI_BARRIER(MPI_COMM_WORLD, ierr)
        ts = MPI_WTIME()

        call cg_goto_f(F,B,ierr,'Zone_t',Z,'UserDefinedData_t',
     &                 1,'end')
        if (ierr .ne. CG_OK) call cgp_error_exit_f
        call cgp_array_read_data_f(1,start,end,dx,ierr)
        if (ierr .ne. CG_OK) call cgp_error_exit_f
        call cgp_array_read_data_f(2,start,end,dy,ierr)
        if (ierr .ne. CG_OK) call cgp_error_exit_f
        call cgp_array_read_data_f(3,start,end,dz,ierr)
        if (ierr .ne. CG_OK) call cgp_error_exit_f

        call MPI_BARRIER(MPI_COMM_WORLD, ierr)
        te = MPI_WTIME()
        tt = tt + te - ts

        do n=1,npp
          if (dx(n) .ne. (start+n-1)) nerrs = nerrs + 1
          if (dy(n) .ne. (commrank+1)) nerrs = nerrs + 1
          if (dz(n) .ne. n) nerrs = nerrs + 1
        enddo

        if (commrank .eq. 0) then
          print *,'read :',tt,' secs,',dsize/tt,' Mb/sec (',
     &      piomode(B),') errors =',nerrs
        endif
      enddo

      call cgp_close_f(F,ierr)
      if (ierr .ne. CG_OK) call cgp_error_exit_f

      call MPI_FINALIZE(ierr)
      end

