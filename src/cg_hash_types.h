#ifndef CGNS_HASH_TYPES_H
#define CGNS_HASH_TYPES_H
#include <stdint.h>

/* Typedef used by hashmap */
/* Not stored in cgnstypes.h to not leak them */

#define SIZEOF_LONG 8
#define SIZEOF_VOID_P 8
#if 1
typedef int64_t map_ssize_t;
typedef uint64_t map_usize_t;
#define SIZEOF_MAP_USIZE_T 8
#else
typedef int32_t map_ssize_t;
typedef uint32_t map_usize_t;
#define SIZEOF_MAP_USIZE_T 4
#endif
typedef char char_name[33];

#endif
