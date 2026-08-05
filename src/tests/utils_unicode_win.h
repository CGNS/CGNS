/*
 * Windows-only UTF-8 filesystem helpers for CGNS test programs.
 *
 * Include this header inside a  #if defined(_WIN32) ... #endif  guard
 * after including <windows.h>.  Each function is defined static so it
 * can be included by multiple translation units without ODR conflicts.
 */
#ifndef UTILS_UNICODE_WIN_H
#define UTILS_UNICODE_WIN_H

#if defined(_WIN32)
#include <stdlib.h>
#include <errno.h>
#include <limits.h>

/* Convert a UTF-8 string to a newly-allocated UTF-16 (wide) string.
 * Returns NULL on failure (bad input, OOM, or overflow).
 * Caller must free() the returned pointer. */
static wchar_t *utf8_to_wide(const char *utf8)
{
    int len = MultiByteToWideChar(CP_UTF8, 0, utf8, -1, NULL, 0);
    if (len <= 0) return NULL;
    /* Guard against len * sizeof(wchar_t) wrapping size_t on 32-bit hosts. */
    if ((size_t)len > (size_t)(INT_MAX / (int)sizeof(wchar_t))) {
        errno = ENOMEM;
        return NULL;
    }
    wchar_t *wide = (wchar_t *)malloc((size_t)len * sizeof(wchar_t));
    if (wide == NULL) { errno = ENOMEM; return NULL; }
    if (MultiByteToWideChar(CP_UTF8, 0, utf8, -1, wide, len) == 0) {
        free(wide);
        return NULL;
    }
    return wide;
}

static int utf8_mkdir(const char *path)
{
    wchar_t *wpath = utf8_to_wide(path);
    int ret;
    if (wpath == NULL) { errno = ENOMEM; return -1; }
    ret = _wmkdir(wpath);
    free(wpath);
    return ret;
}

static int utf8_unlink(const char *path)
{
    wchar_t *wpath = utf8_to_wide(path);
    int ret;
    if (wpath == NULL) { errno = ENOMEM; return -1; }
    ret = _wunlink(wpath);
    free(wpath);
    return ret;
}

static int utf8_rmdir(const char *path)
{
    wchar_t *wpath = utf8_to_wide(path);
    int ret;
    if (wpath == NULL) { errno = ENOMEM; return -1; }
    ret = _wrmdir(wpath);
    free(wpath);
    return ret;
}

#endif /* _WIN32 */
#endif /* UTILS_UNICODE_WIN_H */
