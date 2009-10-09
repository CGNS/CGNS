#!/bin/sh

# sh script to launch CGNS shell

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

# get the cgnssh executable

cgnsshell=""
for d in $dir $dir/$CG_SYSTEM $dir/cgnssh \
  $CG_BIN_DIR $CG_BIN_DIR/$CG_SYSTEM ; do
  if test -x $d/cgnsshell ; then
    cgnsshell=$d/cgnsshell
    break
  fi
  if test -x $d/cgnswish/cgnsshell ; then
    cgnsshell=$d/cgnswish/cgnsshell
    break
  fi
done
if test -z "$cgnsshell" ; then
  echo "Error: cgnsshell executable not found"
  exit 1
fi

# execute

exec $cgnsshell

