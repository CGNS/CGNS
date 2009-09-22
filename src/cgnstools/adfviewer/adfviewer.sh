#!/bin/sh

# sh script to launch ADF File viewer/editor

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

# get the adfwish executable

adfwish=""
for d in $dir $dir/$CG_SYSTEM $dir/adfviewer \
  $CG_BIN_DIR $CG_BIN_DIR/$CG_SYSTEM ; do
  if test -x $d/adfwish ; then
    adfwish=$d/adfwish
    break
  fi
  if test -x $d/cgnswish/adfwish ; then
    adfwish=$d/cgnswish/adfwish
    break
  fi
done
if test -z "$adfwish" ; then
  echo "Error: adfwish executable not found"
  exit 1
fi

# find the adfviewer tcl script

adfviewer=""
for d in $dir $dir/$CG_SYSTEM $dir/adfviewer \
  $CG_LIB_DIR $CG_LIB_DIR/$CG_SYSTEM ; do
  if test -f $d/adfviewer.tcl ; then
    adfviewer=$d/adfviewer.tcl
    break
  fi
  if test -f $d/cgnstools/adfviewer.tcl ; then
    adfviewer=$d/cgnstools/adfviewer.tcl
    break
  fi
done
if test -z "$adfviewer" ; then
  echo "Error: adfviewer.tcl script not found"
  exit 1
fi

# check that display is set

if test -z "$DISPLAY" ; then
  echo "Error: DISPLAY environment variable not set"
  exit 1
fi

# execute

if test "$#" = 0 ; then
  exec $adfwish $adfviewer
else
  exec $adfwish $adfviewer "$@"
fi
