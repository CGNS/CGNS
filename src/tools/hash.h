/*
 * hash.h - include file for hash routines
 */

#ifndef _HASH_H_
#define _HASH_H_

#include <stdio.h>  /* for NULL */

typedef void *HASH;

#ifdef __cplusplus
extern "C" {
#endif

HASH HashCreate (
#ifdef PROTOTYPE
    int size,
    int (*compare) (void *entry1, void *entry2),
    unsigned (*hashfunc) (void *entry)
#endif
);

void HashDestroy (
#ifdef PROTOTYPE
    HASH hash,
    void (*freeentry) (void *entry)
#endif
);

void *HashFind (
#ifdef PROTOTYPE
    HASH hash,
    void *entry
#endif
);

void *HashAdd (
#ifdef PROTOTYPE
    HASH hash,
    void *entry
#endif
);

void *HashDelete (
#ifdef PROTOTYPE
    HASH hash,
    void *entry
#endif
);

#define HashSize(H)    HashList(H,NULL,NULL)

int HashList (
#ifdef PROTOTYPE
    HASH hash,
    int (*listentry)(
        void *entry,
        void *userdata
    ),
    void *userdata
#endif
);

void HashStats (
#ifdef PROTOTYPE
    HASH hash
#endif
);

#ifdef __cplusplus
}
#endif

#endif  /* _HASH_H_ */

