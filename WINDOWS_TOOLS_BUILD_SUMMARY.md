# Windows Tools Build - Implementation Summary

## Overview
This branch (689) implements fixes to enable CGNS tools to build on Windows and adds Windows CI testing. The work complements PR #868 which attempted to remove private Tk header dependencies.

## Problem Statement
PR #868 (https://github.com/CGNS/CGNS/pull/868) attempted to eliminate private Tk header dependencies by using forward declarations. However, this approach fails on Windows because:
1. `tkogl.c` directly accesses `TkWindow` struct members (`dispPtr`, `dirtyAtts`, `dirtyChanges`) in `WinMakeWindowExist()`
2. Windows code uses `TkWinDCState` type defined only in `tkWinInt.h`
3. Modern Tcl/Tk uses standard C variadic functions instead of deprecated `TCL_VARARGS` macros
4. OpenGL GLU headers use inconsistent function pointer types across platforms

## Solution Components

### 1. Platform-Specific Header Strategy (tkogl.c, tkogl.h)

**src/cgnstools/tkogl/tkogl.c:**
```c
#if defined(__WIN32__) || defined(_WIN32)
/* Windows: Need full TkWindow definition for internal member access */
#include "tkInt.h"
#include "tkWinInt.h"
#else
/* Unix/Linux: Forward declaration only */
typedef struct TkWindow TkWindow;
extern void TkWmAddToColormapWindows(TkWindow *winPtr);
#endif
```

**Rationale:**
- Windows requires full struct definitions due to direct member access at lines 408, 413-417
- Linux only needs `TkWmAddToColormapWindows()` function, avoids complex header dependencies
- Removed hardcoded `<tk-private/generic/tkInt.h>` path that fails on standard distributions

**src/cgnstools/tkogl/tkogl.h:**
- Removed `#include <tkWinInt.h>` (line 13)
- Moved to tkogl.c where it's actually needed
- Added documentation explaining the strategy

### 2. Modernize Tcl Compatibility (winmain.c files)

**Files Modified:**
- src/cgnstools/cgnscalc/winmain.c
- src/cgnstools/cgnsview/winmain.c
- src/cgnstools/cgnsplot/winmain.c

**Changes:**
```c
// OLD (Tcl 8.3 era, removed in 8.4+)
static void WishPanic TCL_VARARGS(char *,format);
WishPanic TCL_VARARGS_DEF(char *,arg1) {
    format = TCL_VARARGS_START(char *,arg1,argList);
    vsprintf(buf, format, argList);
}

// NEW (Standard C99, compatible with Tcl 8.4+)
static void WishPanic (const char *format, ...);
WishPanic (const char *format, ...) {
    va_start(argList, format);
    vsprintf(buf, format, argList);
    va_end(argList);
}
```

**Rationale:**
- `TCL_VARARGS` macros removed from Tcl 8.4+ (2002)
- Modern `Tcl_PanicProc` typedef: `(const char *format, ...)`
- Compatible with all Tcl versions 8.4-8.6+ (20+ years of releases)

### 3. Fix OpenGL Compatibility (gencyl.c)

**src/cgnstools/tkogl/gencyl.c:**
```c
// OLD (Windows-specific, not defined in all GLU headers)
gluTessCallback(obj, GLU_BEGIN, (_GLUfuncptr)glBegin);

// NEW (Portable, works across all GLU implementations)
gluTessCallback(obj, GLU_BEGIN, (void (*)())glBegin);
```

**Rationale:**
- `_GLUfuncptr` not consistently defined across Windows GLU implementations
- `void(*)()` is standard portable approach recommended by OpenGL community
- Works with all GLU versions

### 4. Windows CI Workflow

**File:** `.github/workflows/windows-tools.yml` (new, 414 lines)

**Workflow Steps:**
1. **Build Tcl/Tk 8.6.15 from Source**
   - Downloads official source from SourceForge (tcl8.6.15-src.tar.gz, tk8.6.15-src.tar.gz)
   - Uses `curl -L` to properly follow redirects
   - Builds with MSVC (Visual Studio 2022)
   - Installs to `C:\tcltk`

2. **Manually Copy Private Headers**
   - Tcl: `tclInt.h`, `tclIntDecls.h`, `tclIntPlatDecls.h`, `tclPort.h` from generic/
   - Tcl Windows: `tclWinInt.h`, `tclWinPort.h` from win/
   - Tk: `tkInt.h`, `tkIntDecls.h`, `tkIntPlatDecls.h`, `tkPort.h` from generic/
   - Tk Windows: `tkWin.h`, `tkWinInt.h`, `tkWinPort.h` from win/
   - Reason: `install-private-headers` target doesn't work reliably on Windows

3. **Download HDF5 Windows Binaries**
   - HDF5 1.14.6 with Visual Studio 2022 (vs2022_64)
   - Handles double-zip structure (zip containing zip)
   - Extracts to `C:\hdf5`

4. **Build CGNS with Tools**
   - Configures CMake with cgnstools enabled
   - Sets Tcl/Tk paths to `C:\tcltk`
   - Builds all cgnstools including cgnsview (cgiowish.exe)
   - Verifies tkogl.c compiles successfully on Windows

## Files Changed

### Source Code (6 files)
- `src/cgnstools/tkogl/tkogl.c` - Platform-specific header strategy
- `src/cgnstools/tkogl/tkogl.h` - Remove tkWinInt.h include
- `src/cgnstools/tkogl/gencyl.c` - _GLUfuncptr → void(*)()
- `src/cgnstools/cgnscalc/winmain.c` - TCL_VARARGS → C99 variadic
- `src/cgnstools/cgnsview/winmain.c` - TCL_VARARGS → C99 variadic
- `src/cgnstools/cgnsplot/winmain.c` - TCL_VARARGS → C99 variadic

### CI/CD (1 file)
- `.github/workflows/windows-tools.yml` - Windows build workflow (new)

## Testing & Validation

### Linux ✅
- Forward declaration approach works
- No private header dependencies
- Existing CI passes

### Windows ✅
- Builds Tcl/Tk from source
- Private headers available
- tkogl.c compiles successfully
- All cgnstools build

### Compatibility ✅
- Tcl/Tk 8.4+ (2002-present)
- Modern OpenGL/GLU implementations
- Visual Studio 2022 (Windows)
- GCC/Clang (Linux)

## Key Design Decisions

1. **Platform-specific approach**: Different strategies for Windows (private headers) vs Linux (forward declarations) based on actual code requirements

2. **No source code changes for standard distributions**: Linux users don't need tk-private packages, only Windows build-from-source scenario needs private headers

3. **Modernization**: Updated deprecated Tcl 8.3 constructs to standard C99, improving long-term maintainability

4. **Windows CI**: Builds dependencies from source to ensure private headers available, comprehensive test of the fix

## Future Considerations

- Could explore removing Windows dependency on TkWindow internals (major refactoring)
- Modern GUI alternatives (Qt, Dear ImGui, wxWidgets) discussed but out of scope
- Tcl/Tk 8.6.17 available (currently using 8.6.15 in CI)

## References

- Issue: https://github.com/CGNS/CGNS/issues/689
- PR #868: https://github.com/CGNS/CGNS/pull/868
- Branch: 689
