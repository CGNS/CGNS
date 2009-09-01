#!/bin/sh

# sh script to launch CGNS plot

dir=`dirname $0`

# source the setup script

if test -f $dir/cgconfig ; then
  . $dir/cgconfig
elif test -f $dir/../cgconfig ; then
  . $dir/../cgconfig
else
  if test -z "$CG_SYSTEM" && test -x $dir/cgsystem ; then
    CG_SYSTEM=`$dir/cgsystem`; export CG_SYSTEM
  fi
  if test -f $dir/$CG_SYSTEM/cgconfig ; then
    . $dir/$CG_SYSTEM/cgconfig
  fi
fi

# get the plotwish executable

plotwish=""
for d in $dir $dir/$CG_SYSTEM $dir/cgnsplot \
  $CG_BIN_DIR $CG_BIN_DIR/$CG_SYSTEM ; do
  if test -x $d/plotwish ; then
    plotwish=$d/plotwish
    break
  fi
  if test -x $d/cgnswish/plotwish ; then
    plotwish=$d/cgnswish/plotwish
    break
  fi
done
if test -z "$plotwish" ; then
  echo "Error: plotwish executable not found"
  exit 1
fi

# find the cgnsplot tcl script

cgnsplot=""
for d in $dir $dir/$CG_SYSTEM $dir/cgnsplot \
  $CG_LIB_DIR $CG_LIB_DIR/$CG_SYSTEM ; do
  if test -f $d/cgnsplot.tcl ; then
    cgnsplot=$d/cgnsplot.tcl
    break
  fi
  if test -f $d/cgnstools/cgnsplot.tcl ; then
    cgnsplot=$d/cgnstools/cgnsplot.tcl
    break
  fi
done
if test -z "$cgnsplot" ; then
  cgnsplot="/usr/local/share/cgnstools/cgnsplot.tcl"
fi
if test -z "$cgnsplot" ; then
  echo "Error: cgnsplot.tcl script not found"
  exit 1
fi

# check that display is set

#if test -z "$DISPLAY" ; then
#  echo "Error: DISPLAY environment variable not set"
#  exit 1
#fi

# execute

if test "$#" = 0 ; then
  exec $plotwish $cgnsplot
else
  exec $plotwish $cgnsplot "$@"
fi
