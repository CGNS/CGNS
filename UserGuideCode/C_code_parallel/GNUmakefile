CC       := mpicc
CFLAGS   := -I/usr/local/cgns/dev/include
LIBFLAGS := -L/usr/local/cgns/dev/lib -L/usr/local/parallel/lib -Wl,-rpath,/usr/local/cgns/dev/lib -Wl,-rpath,/usr/local/parallel/lib -lcgns -lhdf5 -lsz -lz

execs    := write_grid_str2d \
            write_flowcentrind_shape_str2d

all: $(execs)

$(execs): %: %.c
	$(CC) $(CFLAGS) $(LIBFLAGS) $< -o $@

clean:
	rm -f $(execs) *~
