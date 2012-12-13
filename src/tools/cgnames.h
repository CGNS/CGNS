#ifndef _CGNAMES_H_
#define _CGNAMES_H_
int cg_get_identifier (const char *name, int *nexps, float *exps);
int cg_find_identifier (const char *pattern, int maxnames, char **names);
int cg_enum_identifier (int (*callback)(char *name,
    int nexps, float *exps, void *user), void *user);
#endif

