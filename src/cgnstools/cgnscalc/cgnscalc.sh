#!/bin/sh

# sh script to launch CGNS calculator

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

# get the calcwish executable

calcwish=""
for d in $dir $dir/$CG_SYSTEM $dir/cgnscalc \
  $CG_BIN_DIR $CG_BIN_DIR/$CG_SYSTEM ; do
  if test -x $d/calcwish ; then
    calcwish=$d/calcwish
    break
  fi
  if test -x $d/cgnswish/calcwish ; then
    calcwish=$d/cgnswish/calcwish
    break
  fi
done
if test -z "$calcwish" ; then
  echo "Error: calcwish executable not found"
  exit 1
fi

# find the cgnscalc tcl script

cgnscalc=""
for d in $dir $dir/$CG_SYSTEM $dir/cgnscalc \
  $CG_LIB_DIR $CG_LIB_DIR/$CG_SYSTEM ; do
  if test -f $d/cgnscalc.tcl ; then
    cgnscalc=$d/cgnscalc.tcl
    break
  fi
  if test -f $d/cgnstools/cgnscalc.tcl ; then
    cgnscalc=$d/cgnstools/cgnscalc.tcl
    break
  fi
done
if test -z "$cgnscalc" ; then
  echo "Error: cgnscalc.tcl script not found"
  exit 1
fi

# check that display is set

if test -z "$DISPLAY" ; then
  echo "Error: DISPLAY environment variable not set"
  exit 1
fi

# execute

if test "$#" = 0 ; then
  exec $calcwish $cgnscalc
else
  exec $calcwish $cgnscalc "$@"
fi

