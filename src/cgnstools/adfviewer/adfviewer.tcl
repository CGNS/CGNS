#!/bin/sh
# the next line restarts using wish \
exec adfwish -f "$0" "$@"

proc error_exit {msg} {
  wm withdraw .
  tk_dialog .error Error $msg error 0 Exit
  exit 1
}

if {[catch {package require Tk 8.0} msg]} {
  error_exit $msg
}

#---------- get platform

set platform $tcl_platform(platform)
if {$platform == "windows" &&
    [info exists env(TERM)] &&
    $env(TERM) == "cygwin"} {
  set platform cygwin
}

#---------- setup paths to tcl files

set cmd_name [file tail $argv0]
set cmd_dir  [file dirname $argv0]
if {![file exists $argv0] || [file isdirectory $argv0]} {
  if {$platform == "windows"} {
    set sep ";"
  } else {
    set sep ":"
  }
  foreach i [split $env(PATH) $sep] {
    if {$sep == ";"} {
      set i [join [split $i \\] /]
    }
    if {[file exists $i/$cmd_name] && ![file isdirectory $i/$cmd_name]} {
      set cmd_dir $i
      break;
    }
  }
}
set curdir [pwd]
if ![catch {cd $cmd_dir}] {
  set cmd_dir [pwd]
  cd $curdir
}
if {$tcl_platform(platform) == "windows"} {
  set cmd_dir [file attributes $cmd_dir -shortname]
}

set auto_path "$cmd_dir $cmd_dir/../common $auto_path"
if {[info exists env(TCL_PROC_DIR)]} {
  set auto_path "$env(TCL_PROC_DIR) $auto_path"
}

#---------- initialize windows

if {$platform == "windows"} {
  set vers [join [split $tcl_version .] {}]
  if {[info commands ADFopen] == {}} {
    if {[catch {load adftcl$vers} msg]} {
      error_exit $msg
    }
  }
  catch {load tclreg$vers registry}
  if ![info exists env(CG_SYSTEM)] {
    set env(CG_SYSTEM) Win32
  }
} else {
  if {[info commands ADFopen] == {}} {
    error_exit "need to run script with adfwish"
  }
}

if [catch {config_defaults 1} msg] {error_exit $msg}


#----- node tree icons

image create photo dirimg -data {
R0lGODlhFAAQALMAAAAAAIAAAACAAICAAAAAgIAAgACAgICAgMDAwP8AAAD/AP//AAAA//8A\
/wD//////yH5BAEAAA4ALAAAAAAUABAAAARF0MlJq5Un53sPWh9ycJQHnqK2VcfjvrB7AOyD\
3t9chziolzaecPGbtIbCIibYy9GASJTS0Wqepqps9kkBeL9gMGlMLksiADs=}

image create photo fileimg -data {
R0lGODlhFAAQALMAAAAAAIAAAACAAICAAAAAgIAAgACAgICAgMDAwP8AAAD/AP//AAAA//8A\
/wD//////yH5BAEAAAgALAAAAAAUABAAAAQ5EMlJq5Ug63wreGDIdRhInA+AqOQXgltroqL8\
vuzlnjxs377OjkcI6oA14ayX+gFzlo10RKpar5IIADs=}

image create photo badimg -data {
R0lGODlhFAAQALMAAAAAAIAAAACAAICAAAAAgIAAgACAgICAgMDAwP8AAAD/AP//AAAA//8A\
/wD//////yH5BAEAAAgALAAAAAAUABAAAAQkEMlJq704S8K5pl34Id1UaifpoatKjGALp7Nc\
v3Ce0bfu/4gIADs=}

image create photo dirlink -data {
R0lGODlhFAAQALMAAAAAAIAAAACAAICAAAAAgIAAgACAgICAgMDAwP8AAAD/AP//AAAA//8A\
/wD//////yH5BAEAAA4ALAAAAAAUABAAAARR0MlJq5Un53sPWh9ycJQHnqK2VcfjvrB7AOyD\
3t9chziolzHX7TdpPQBIwIMnogFdSRuPiIFGmVRHK6ocOotH5UuV+U6OsaTaAoWRKMHH2xEB\
ADs=}

image create photo filelink -data {
R0lGODlhFAAQALMAAAAAAIAAAACAAICAAAAAgIAAgACAgICAgMDAwP8AAAD/AP//AAAA//8A\
/wD//////yH5BAEAAAgALAAAAAAUABAAAAQ/EMlJq5Ug63wreGDIdRhInA+AqOQXgltroqL8\
vqx1z3AHa7dcRZR50ISUXxHnA36OTdcOOUkFYxfYi5TccRERADs=}

image create photo badlink -data {
R0lGODlhFAAQALMAAAAAAIAAAACAAICAAAAAgIAAgACAgICAgMDAwP8AAAD/AP//AAAA//8A\
/wD//////yH5BAEAAAgALAAAAAAUABAAAAQ5EMlJq704S8K5pl34Id1UaifpjeBqPXD8qEQG\
AzjwpFec7zXN7efCPH461lEXK/aYzaDtKFPKYJ8IADs=}

image create photo invalid -data {
R0lGODlhFAAQALMAAAAAAIAAAACAAICAAAAAgIAAgACAgICAgMDAwP8AAAD/AP//AAAA//8A\
/wD//////yH5BAEAAAgALAAAAAAUABAAAARLEMmZap04A2B7/lzyjE/4YdVDrI93pmRbmR+8\
WuOVzSQ+0pQEIFdhlXTBoY8EDFpWRiRqGZNOVVCC7CQJxWQALmLTsWzETit6PYkAADs=}

#----- initialize program data

array set ProgData {
  version "ADF Version unknown"
  libvers ""
  filevers ""
  format NATIVE
  newfile NewFile.adf
  file ""
  tmpdir /tmp
  tmpfile ""
  readonly 1
  follow 0
  verify 1
  backup 1
  bakfile ""
  autoload 1
  defsize 1024
  maxsize 1024
  defcnt 10
  maxcnt 10
  showlines 1
  linenum ""
  linecnt ""
  lineval ""
  toolbar 1
  tree ""
  buttons ""
  find ""
  case 0
  fromtop 0
  start /
  seppos 0.4
  sepwd 7
  reg,file ".cgnstools"
  reg,base "HKEY_CURRENT_USER/Software/CGNS"
  reg,key "ADFviewer"
  reg,vals {file follow verify backup autoload maxsize maxcnt \
            toolbar find case fromtop showlines}
  cgnsversion ""
  cgnscheck ""
  cgnsplot ""
  cgnscalc ""
  menucfg ""
  ADFHDF5 ""
  HDF5ADF ""

  file,name ""
  file,dir ""
  file,size ""
  file,fmt ""
  file,cgns ""
  file,type ""
  file,mode ""
  file,mtime ""
  file,atime ""

  units,mass Kilogram
  units,length Meter
  units,time Second
  units,temp Kelvin
  units,angle Radian
  units,current Ampere
  units,amount Mole
  units,intensity Candela
}

array set Node {
  parent ""
  name ""
  label ""
  type ""
  dim ""
  size ""
  lnode ""
  lfile ""
  trace 0
}

foreach i [array names Node] {
  set NodeSave($i) $Node($i)
}

if {![catch ADFversion version]} {
  set ProgData(version) $version
}
set ProgData(libvers) [CGNSversion]

cgns_init $ProgData(libvers)

array set Import {
  inputfile ""
  cgnsfile ""
  exefile ""
  options ""
  basename ""
  basename,flag 1
  dupcheck,flag 0
  dupcheck d
  duptol ""
}

array set Export {
  cgnsfile ""
  outputfile ""
  exefile ""
  options ""
  basenum ""
  solnum ""
  basenum,flag 1
  solnum,flag 1
  ascii 0
  solution 1
  weight 0
}

array set Tools {
  cgnsinput ""
  cgnsoutput ""
  verbose ""
  warnings 2
  errors ""
  exefile ""
  options ""
  weight ""
  basenum ""
  basename ""
  zonenum ""
  solnum ""
  solname ""
  basenum,flag 1
  basename,flag 1
  zonenum,flag 1
  solnum,flag  1
  solname,flag 1
  background 0
}

set DataTypes {
  "MT (empty)"
  "C1 (character data)"
  "I4 (32-bit integer)"
  "R4 (32-bit real)"
  "R8 (64-bit real)"
  "B1 (byte data)"
  "U4 (unsigned 32-bit integer)"
  "I8 (64-bit integer)"
  "U8 (unsigned 64-bit integer)"
  "X4 (32-bit complex real)"
  "X8 (64-bit complex real)"
}

#----- read registry

if {[tclreg_init -base $ProgData(reg,base) -fname $ProgData(reg,file)]} {
  foreach i $ProgData(reg,vals) {
    if {![catch {tclreg_get $ProgData(reg,key) $i} val] && $val != ""} {
      set ProgData($i) $val
    }
  }
  catch units_read
}

#----- setup search paths for executable files and script files

if [info exists env(CG_SYSTEM)] {
  set system $env(CG_SYSTEM)
} else {
  set system ""
}

set ProgData(exepath) [list $cmd_dir $cmd_dir/cgnswish]
if {$system != ""} {
  lappend ProgData(exepath) $cmd_dir/$system $cmd_dir/$system/cgnswish
}
if {[info exists env(CG_BIN_DIR)] && $env(CG_BIN_DIR) != ""} {
  lappend ProgData(exepath) $env(CG_BIN_DIR) $env(CG_BIN_DIR)/cgnswish
  if {$system != ""} {
    lappend ProgData(exepath) $env(CG_BIN_DIR)/$system \
      $env(CG_BIN_DIR)/$system/cgnswish
  }
}

set ProgData(libpath) [list $cmd_dir $cmd_dir/cgnstools]
if {$system != ""} {
  lappend ProgData(exepath) $cmd_dir/$system $cmd_dir/$system/cgnstools
}
if {[info exists env(CG_LIB_DIR)] && $env(CG_LIB_DIR) != "" &&
    $env(CG_LIB_DIR) != $cmd_dir} {
  lappend ProgData(libpath) $env(CG_LIB_DIR) $env(CG_LIB_DIR)/cgnstools
  if {$system != ""} {
    lappend ProgData(libpath) $env(CG_LIB_DIR)/$system \
      $env(CG_LIB_DIR)/$system/cgnstools
  }
}

set root_dir [file dirname $cmd_dir]
foreach d {cgnscalc cgnsplot tools utilities} {
  if [file isdirectory $root_dir/$d] {
    lappend ProgData(exepath) $root_dir/$d
    lappend ProgData(libpath) $root_dir/$d
  }
}

proc get_executable {name {showerr 0}} {
  global ProgData
  set exe [find_file executable $name $ProgData(exepath) \$PATH]
  if {$exe == "" && $showerr} {
    set msg "$name executable not found in:"
    foreach p $ProgData(exepath) {
      append msg "\n  $p"
    }
    append msg "\n  \$PATH"
    errormsg $msg
  }
  return $exe
}

proc get_file {name {showerr 0}} {
  global ProgData
  set fname [find_file exists $name $ProgData(libpath) \$PATH]
  if {$fname == "" && $showerr} {
    set msg "$name file not found in:"
    foreach p $ProgData(libpath) {
      append msg "\n  $p"
    }
    append msg "\n  \$PATH"
    errormsg $msg
  }
  return $fname
}

#----- find cgnscheck, cgnsversion, cgnsplot, cgnscalc

set ProgData(cgnscheck) [get_executable cgnscheck]
set ProgData(cgnsversion) [get_executable cgnsversion]

set cgnsplot [get_executable cgnsplot]
if {$cgnsplot != ""} {
  set ProgData(cgnsplot) $cgnsplot
} else {
  set plotwish [get_executable plotwish]
  set cgnsplot [get_file cgnsplot.tcl]
  if {$plotwish != "" && $cgnsplot != ""} {
    set ProgData(cgnsplot) [list $plotwish $cgnsplot]
  }
}

set cgnscalc [get_executable cgnscalc]
if {$cgnscalc != ""} {
  set ProgData(cgnscalc) $cgnscalc
} else {
  set calcwish [get_executable calcwish]
  set cgnscalc [get_file cgnscalc.tcl]
  if {$calcwish != "" && $cgnscalc != ""} {
    set ProgData(cgnscalc) [list $calcwish $cgnscalc]
  }
}

#----- file conversions

set ProgData(ADFHDF5) [get_executable adf2hdf]
set ProgData(HDF5ADF) [get_executable hdf2adf]

if {$platform == "windows"} {
  if {[info exists env(TEMP)]} {
    set tmpdir [join [split $env(TEMP) \\] /]
  } elseif {[info exists env(TMP)]} {
    set tmpdir [join [split $env(TMP) \\] /]
  } else {
    set tmpdir .
  }
  set ProgData(tmpdir) [file attributes $tmpdir -shortname]
}

#---------- main window

wm title . ADFviewer
wm protocol . WM_DELETE_WINDOW do_quit
bind . <Key-F1> cgns_info

#---------- menu

menubar_create {File Config Tree Tools Help}

#--- file menu

set m [menubar_get File]
$m add command -label "New..." -command file_new
$m add command -label "Open..." -command file_load
$m add command -label "Save..." -command file_save -state disabled
$m add separator
$m add command -label "Reload" -command file_reload -state disabled
$m add command -label "File Info..." -command file_info -state disabled
$m add command -label "Restore" -command restore_backup -state disabled

#--- config menu

set m [menubar_get Config]
$m add checkbutton -label " Show Lines" -variable ProgData(showlines) \
  -onvalue 1 -offvalue 0 -command {
    TreeConfig $ProgData(tree) -lines $ProgData(showlines)
  }
$m add checkbutton -label " Show Toolbar" -variable ProgData(toolbar) \
  -onvalue 1 -offvalue 0 -command {
    if {$ProgData(toolbar)} {
      pack .toolbar.but -side left
    } else {
      pack forget .toolbar.but
      .toolbar configure -height 1
    }
  }
$m add checkbutton -label " Verify Delete" -variable ProgData(verify) \
  -onvalue 1 -offvalue 0
$m add checkbutton -label " Auto Backup" -variable ProgData(backup) \
  -onvalue 1 -offvalue 0
$m add checkbutton -label " Auto Load Data" -variable ProgData(autoload) \
  -onvalue 1 -offvalue 0
$m add command -label " Auto Data Size..." -command auto_size

#--- tree menu

set m [menubar_get Tree]
$m add checkbutton -label " Follow Links" -variable ProgData(follow) \
  -onvalue 1 -offvalue 0 -command {follow_links $ProgData(follow)}
$m add command -label " Expand All" -command {tree_expand /} \
  -state disabled
$m add command -label " Collapse All" -command {tree_collapse /} \
  -state disabled
$m add separator
$m add command -label " Find Node..." -command find_node \
  -state disabled
$m add command -label " Find Again" -command find_again \
  -state disabled

#--- tools menu

set m [menubar_get Tools]
$m add command -label "CGNS Version..." -state disabled -command update_cgns
$m add command -label "Check CGNS..." -state disabled -command check_cgns
$m add command -label "Plot CGNS..."  -state disabled -command plot_cgns
$m add command -label "Calculate CGNS..."  -state disabled -command calc_cgns
$m add separator
$m add command -label "Unit Conversions..." -command units_convert

#--- help menu

set m [menubar_get Help]
$m add command -label "ADFviewer..." -command {help_show adfviewer}
$m add command -label "Utilities..." -command {help_show utilities}
$m add command -label "CGNS/ADF..." -command {help_show cgns}
$m add separator
$m add command -label "Configure..." -command help_setup
$m add separator
$m add command -label "CGNS Nodes..." -command cgns_tree
$m add command -label "About..." -underline 0 -command do_about

#----- add menu options

proc next_line {fp} {
  set input ""
  while {[gets $fp line] >= 0} {
    set n [string first "\#" $line]
    if {$n == 0} {
      set line ""
    } elseif {$n > 0} {
      set line [string trim [string range $line 0 [expr $n - 1]]]
    } else {
      set line [string trim $line]
    }
    if {$line == ""} continue
    set end [expr [string length $line] - 1]
    if {[string index $line $end] == "\\"} {
      incr end -1
      append input [string range $line 0 $end]
      continue
    }
    append input $line
    break
  }
  return $input
}

set menufiles [glob -nocomplain $cmd_dir/*.mnu]
if {$menufiles == ""} {
  set menufiles [get_file utilities.mnu]
}

if {$menufiles != ""} {
  set cnt 0
  foreach f $menufiles {
    if {![catch {open $f r} fp]} {
      while (1) {
        set line [next_line $fp]
        if {$line == ""} break
        set mnu [split [lindex $line 0] :]
        set len [expr [llength $mnu] - 1]
        if {$len < 1} {
          error_exit "invalid menu specification - [lindex $line 0]"
        }
        set m [menubar_add [lindex $mnu 0] Help]
        for {set i 1} {$i < $len} {incr i} {
          set entry [lindex $mnu $i]
          if {[catch {$m index $entry} n]} {
            $m add cascade -label $entry -menu $m.m$cnt
            set m $m.m$cnt
            incr cnt
            menu $m -tearoff 0
          } else {
            set m [$m entrycget $n -menu]
            if {$m == ""} {
              error_exit "menu item is not a submenu - [lindex $line 0]"
            }
          }
        }
        set entry [lindex $mnu $len]
        if {[string match "sep*" $entry]} {
          $m add separator
        } else {
          set opts [list $entry]
          eval lappend opts [lrange $line 1 end]
          $m add command -label $entry -command "run_menu $opts"
          set class [string tolower [lindex $line 1]]
          if {[string match "ex*" $class] ||
              [string match "con*" $class] ||
              [string match "ut*" $class]} {
            set n [$m index end]
            $m entryconfigure $n -state disabled
            lappend ProgData(menucfg) [list $m $n]
          }
        }
      }
      close $fp
    }
  }
}

proc run_menu {args} {
  set n 0
  foreach i {name class exe func script} {
    set $i [lindex $args $n]
    incr n
  }
  if {$func == ""} {
    switch -glob [string tolower $class] {
      im* {set func import_default}
      ex* {set func export_default}
      con* {set func tools_convert}
      ut* {set func tools_utility}
      default {set func tools_default}
    }
  } else {
    if {![get_external_proc $func $script]} return
  }
  if {[catch {$func .tools $name $exe} msg]} {
    errormsg $msg
  }
  catch {destroy .tools}
}

proc get_external_proc {func script} {
  if {[info commands $func] == "" && $script != ""} {
    set scr [get_file $script 1]
    if {$scr == ""} {return 0}
    if {[catch {uplevel \#0 source $scr} msg]} {
      errormsg $msg
      return 0
    }
    if {[info commands $func] == ""} {
      errormsg "procedure $func not found in script $script"
      return 0
    }
  }
  return 1
}

#----- add Quit to file menu

set m [menubar_get File]
$m add separator
$m add command -label "Quit" -command do_quit

#----- toolbar

frame .toolbar -height 1
pack .toolbar -side top -pady 3 -fill x

set f [frame .toolbar.but]
if {$ProgData(toolbar)} {
  pack $f -side left
}

#--- file

image create photo img_new -data {\
R0lGODlhEAAQALMAAAAAAIAAAACAAICAAAAAgIAAgACAgICAgMDAwP8AAAD/AP//AAAA//8A\
/wD//////yH5BAEAAAgALAAAAAAQABAAQwQvEMlJ6wQvaw0q3g8gdtQHZuR1ihZirt7JtbQ1\
3mkMYrn07jpZ77cZylAeHK7GjAAAOw==}

image create photo img_open -data {\
R0lGODlhEAAQALMAAAAAAIAAAACAAICAAAAAgIAAgACAgICAgMDAwP8AAAD/AP//AAAA//8A\
/wD//////yH5BAEAAAgALAAAAAAQABAAQwQ4EMlJKwJvZcC7BxdnSV04nCgKjtR6vZgmZ49L\
bmlus7xV9j4QQPYRtWbI3RCXU10WgKaTVPQAexEAOw==}

image create photo img_save -data {\
R0lGODlhEAAQALMAAAAAAIAAAACAAICAAAAAgIAAgACAgICAgMDAwP8AAAD/AP//AAAA//8A\
/wD//////yH5BAEAAA4ALAAAAAAQABAAQwQ90MlJqwRjgM13BpeGjOSIgQ6mdYCphW1Jtugp\
z2/6sVye8rwLMKiL3Tiwm6smUp5Cmaj0A+Utq6yrZTuJAAA7}

image create photo img_reload -data {\
R0lGODlhEAAQALMAAAAAAIAAAACAAICAAAAAgIAAgACAgMDAwICAgP8AAAD/AP//AAAA//8A\
/wD//////yH5BAEAAAcALAAAAAAQABAAAAQ98IBJ67kYvM33xJfWcRoAiqRHZePUmSFZkewD\
IDZq16K+x7aWp5YK/iQvDaJHrKBgSFmL6JkCR6+McwuLAAA7}

image create photo img_info -data {\
R0lGODlhEAAQALMAAAAAAIAAAACAAICAAAAAgIAAgACAgICAgMDAwP8AAAD/AP//AAAA//8A\
/wD//////yH5BAEAAA4ALAAAAAAQABAAQwRG0MlJqwMv64c5BSCIACNimgSBgVy4Wt95Auk6\
cmXo1G0behOXUAcLCneECokkQ/Bwy5KJpupllkBJZ7MBfLhcbxDcLZonEQA7}

set b [frame $f.file]
pack $b -side left -padx 5

button $b.new -image img_new -takefocus 0 -command file_new
set_balloon $b.new "New File..."

button $b.open -image img_open -takefocus 0 -command file_load
set_balloon $b.open "Open File..."

button $b.save -image img_save -takefocus 0 \
  -command file_save -state disabled
set_balloon $b.save "Save As..."

button $b.reload -image img_reload -takefocus 0 \
  -command file_reload -state disabled
set_balloon $b.reload "Reload File"

button $b.info -image img_info -takefocus 0 \
  -command "file_info $b.info" -state disabled
set_balloon $b.info "File Information..."

pack $b.new $b.open $b.save $b.reload $b.info -side left

#--- config

image create photo img_size -data {\
R0lGODlhEAAQALMAAAAAAIAAAACAAICAAAAAgIAAgACAgICAgMDAwP8AAAD/AP//AAAA//8A\
/wD//////yH5BAEAAAgALAAAAAAQABAAAAQ8EMlJq72V6M05/khHWNoHnEC2ScAhHSnWOs4b\
h2vqHC5yVz/WxNNCHGowUw9267BQgEHQJMWJVr4BCBgBADs=}

set b [frame $f.config]
pack $b -side left -padx 5

button $b.size -image img_size -takefocus 0 \
  -command "auto_size $b.size"
set_balloon $b.size "Auto Data Size..."

pack $b.size -side left

#--- tree

image create photo img_follow -data {\
R0lGODlhEAAQALMAAAAAAIAAAACAAICAAAAAgIAAgACAgICAgMDAwP8AAAD/AP//AAAA//8A\
/wD//////yH5BAEAAAgALAAAAAAQABAAAAQwEMlJq714jpHrWFwnbaFoVkAGrCnSIgSBrhIL\
P3JtV3FuvxgaJjYBWgi4YqZ3aloiADs=}

image create photo img_expand -data {\
R0lGODlhEAAQALMAAAAAAIAAAACAAICAAAAAgIAAgACAgICAgMDAwP8AAAD/AP//AAAA//8A\
/wD//////yH5BAEAAAgALAAAAAAQABAAAAQvEMlJq712DJzX5pP2VQBIWuWVUitBTCsMlMTz\
IrHl3jnYUy7J7Cep8UA7k3IpiQAAOw==}

image create photo img_collapse -data {\
R0lGODlhEAAQALMAAAAAAIAAAACAAICAAAAAgIAAgACAgICAgMDAwP8AAAD/AP//AAAA//8A\
/wD//////yH5BAEAAAgALAAAAAAQABAAAAQkEMlJq7046wrwGBQgXsMCTh35hRvbSqm1urJJ\
V98Jvzvv/5UIADs=}

set b [frame $f.tree]
pack $b -side left -padx 5

button $b.follow -image img_follow -takefocus 0 \
  -command {follow_links ""}
set_balloon $b.follow "Follow Links..."
if {$ProgData(follow)} {
  $b.follow configure -relief sunken
}

button $b.expand -image img_expand -takefocus 0 \
  -command {tree_open /} -state disabled
set_balloon $b.expand "Open One Level"

button $b.collapse -image img_collapse -takefocus 0 \
  -command {tree_close /} -state disabled
set_balloon $b.collapse "Close One Level"

pack $b.follow $b.expand $b.collapse -side left

image create photo img_find -data {\
R0lGODlhEAAQALMAAAAAAIAAAACAAICAAAAAgIAAgACAgMDAwICAgP8AAAD/AP//AAAA//8A\
/wD//////yH5BAEAAA4ALAAAAAAQABAAAARC0MnpELiIagfQO4+1Sd1nihriPeyZUYDJgscB\
aADYmjbO8z2Yh/Y5qHAlmvEGe2BqS99lipFeRpPYFAu7cmHfMDYCADs=}

image create photo img_again -data {\
R0lGODlhEAAQALMAAAAAAIAAAACAAICAAAAAgIAAgACAgMDAwICAgP8AAAD/AP//AAAA//8A\
/wD//////yH5BAEAAA4ALAAAAAAQABAAAARH0MnpELiIagfQO4+1Sd1nihriPeyZUYDJgscB\
aADYmjbO8z2Yh/Y5qHAlmvEGe2BqS99lipFeRpPYFAu7km5XL6fLATO5mwgAOw==}

set b [frame $f.find]
pack $b -side left -padx 5

button $b.first -image img_find -takefocus 0 \
  -command find_node -state disabled
set_balloon $b.first "Find Node..."

button $b.again -image img_again -takefocus 0 \
  -command find_again -state disabled
set_balloon $b.again "Find Again"

pack $b.first $b.again -side left

#--- tools

image create photo img_version -data {\
R0lGODlhEAAQALMAAAAAAIAAAACAAICAAAAAgIAAgACAgMDAwICAgP8AAAD/AP//AAAA//8A\
/wD//////yH5BAEAAAcALAAAAAAQABAAQwQ6EEh5qr1n6gqep930ANeEnZ33hSNVWqE0njSW\
qmR9r3iuITDVxxYU5mBFHGdT44xaRxR0pptGbRpJBAA7}

image create photo img_check -data {\
R0lGODlhEAAQALMAAAAAAIAAAACAAICAAAAAgIAAgACAgICAgMDAwP8AAAD/AP//AAAA//8A\
/wD//////yH5BAEAAAgALAAAAAAQABAAAAQ3EMlJq70V6A0wAk8YapkEig/YfeuHplzrvqPs\
niKZjblOnRpYyoLjDHc1IBFVXNKOvxjHQ5VEAAA7}

image create photo img_plot -data {\
R0lGODlhEAAQALMAAAAAAIAAAACAAICAAAAAgIAAgACAgICAgMDAwP8AAAD/AP//AAAA//8A\
/wD//////yH5BAEAAAUALAAAAAAQABAAQwRKEDEkq7UsaxaSD+Amakk4ip05TRU7BZdbdSVI\
MU6Og14N5jqgAwckYo43ytEFCrCWlJRTKXt9bIhhkHb9EbVDWhNM1oaEwaKQGAEAOw==}

image create photo img_calc -data {\
R0lGODlhEAAQALMAAAAAAIAAAACAAICAAAAAgIAAgACAgICAgMDAwP8AAAD/AP//AAAA//8A\
/wD//////yH5BAEAAAgALAAAAAAQABAAQwQ8EIFJ60RYvg14fx2AfSH4ceMGqmqYrmEJvuXE\
zRqro/nOir2d67X6UYg+3unUwpGSTk/MFDVNYSOL9hIBADs=}

set b [frame $f.tools]
pack $b -side left -padx 5

button $b.version -image img_version -takefocus 0 \
  -command update_cgns -state disabled
set_balloon $b.version "Change CGNS Version..."

button $b.check -image img_check -takefocus 0 \
  -command check_cgns -state disabled
set_balloon $b.check "Check CGNS File..."

button $b.plot -image img_plot -takefocus 0 \
  -command plot_cgns -state disabled
set_balloon $b.plot "Plot CGNS File..."

button $b.calc -image img_calc -takefocus 0 \
  -command calc_cgns -state disabled
set_balloon $b.calc "Calculate CGNS..."

pack $b.version $b.check $b.plot $b.calc -side left

image create photo img_convert -data {\
R0lGODlhEAAQALMAAAAAAIAAAACAAICAAAAAgIAAgACAgICAgMDAwP8AAAD/AP//AAAA//8A\
/wD//////yH5BAEAAA4ALAAAAAAQABAAQwQt0MlJq70VSO349B0ghhwYntuIXZ7YUmaWxeu3\
3eTb4Sld8yngztYDuYbC3yQCADs=}

button $f.convert -image img_convert -takefocus 0 \
  -command "units_convert $f.convert"
set_balloon $f.convert "Unit Conversions..."

pack $f.convert -side left -padx 5

#--- help

image create photo img_help -data {\
R0lGODlhEAAQALMAAAAAAIAAAACAAICAAAAAgIAAgACAgICAgMDAwP8AAAD/AP//AAAA//8A\
/wD//////yH5BAEAAAgALAAAAAAQABAAQwQiEMlJq50kX5kJ1hvShd+4mSJ4qmTrXl28ehw7\
t+j75joVAQA7}

button $f.help -image img_help -takefocus 0 -command {help_show adfviewer}
pack $f.help -side left -padx 5
set_balloon $f.help Help

proc help_menu {} {
  global HelpData
  set n 0
  foreach i {adfviewer utilities cgns} {
    if {[help_valid $i]} {
      menubar_state Help normal $n
      if {$i == "adfviewer"} {
        .toolbar.but.help configure -state normal
      }
    } else {
      menubar_state Help disabled $n
      if {$i == "adfviewer"} {
        .toolbar.but.help configure -state disabled
      }
    }
    incr n
  }
}

help_init {adfviewer ADFviewer} {utilities Utilities} {cgns CGNS/ADF}

#---------- main window

frame .main -width 640 -height 500
pack .main -side top -fill both -expand 1 -padx 5 -pady 5

#--- window seperator

frame .main.sep -width 6 -bd 2 -relief raised -cursor sb_h_double_arrow
place .main.sep -relx $ProgData(seppos) -x -3 -rely 0 -relheight 1

bind .main.sep <ButtonPress-1> sep_begin_move

proc sep_begin_move {} {
  set width [winfo width .main]
  set x [winfo rootx .main.sep]
  set y [winfo rooty .main.sep]
  set h [winfo height .main.sep]
  set xmin [expr [winfo rootx .main] + 100]
  set xmax [expr [winfo rootx .main] + $width - 100]

  set top [toplevel .main.move -borderwidth 1 \
    -relief raised -cursor sb_h_double_arrow]
  wm overrideredirect $top 1
  wm geom $top "4x$h+$x+$y"

  update idletasks
  grab set $top
  bind $top <ButtonRelease-1> "sep_end_move $top $xmin $xmax %X"
  bind $top <Motion> "sep_move $top $xmin $xmax %X $y"
}

proc sep_move {top xmin xmax x y} {
  if {$x < $xmin} {
    set x $xmin
  } elseif {$x > $xmax} {
    set x $xmax
  }
  wm geom $top "+$x+$y"
}

proc sep_end_move {top xmin xmax x} {
  global ProgData
  destroy $top
  if {$x < $xmin} {
    set x $xmin
  } elseif {$x > $xmax} {
    set x $xmax
  }
  set s [expr double($x - [winfo rootx .main]) / \
    double([winfo width .main])]
  place .main.tree -relx 0 -relwidth $s -width -$ProgData(sepwd) \
    -rely 0 -relheight 1
  place .main.node -relx $s -x $ProgData(sepwd) \
    -relwidth [expr 1.0 - $s] -width -$ProgData(sepwd) \
    -rely 0 -relheight 1
  place .main.sep -relx $s -x -3 -rely 0 -relheight 1
  set ProgData(seppos) $s
}

#---------- node tree

FrameCreate .main.tree -text "Node Tree" -font $Font(bold)
place .main.tree -relx 0 -relwidth $ProgData(seppos) \
  -width -$ProgData(sepwd) -rely 0 -relheight 1
set f [FrameGet .main.tree]

set ProgData(tree) $f.tree

scrollbar $f.ys -orient vertical -command "$ProgData(tree) yview" \
  -takefocus 0 -highlightthickness 0
pack $f.ys -side right -fill y

scrollbar $f.xs -orient horizontal -command "$ProgData(tree) xview" \
  -takefocus 0 -highlightthickness 0
pack $f.xs -side bottom -fill x

TreeCreate $ProgData(tree) -width 200 -height 400 -relief sunken \
  -bd 2 -highlightthickness 1 -yscrollcommand "$f.ys set" -takefocus 1 \
  -xscrollcommand "$f.xs set" -font $Font(normal) \
  -lines $ProgData(showlines)
pack $ProgData(tree) -side left -fill both -expand 1

bind $ProgData(tree) <1> {tree_show %W %x %y}
bind $ProgData(tree) <2> {tree_info %W %x %y}
bind $ProgData(tree) <3> {tree_menu %W %x %y}
bind $ProgData(tree) <Double-1> {TreeToggle %W [TreeAt %W %x %y]}
bind $ProgData(tree) <Shift-1> {tree_at %W %x %y open}
bind $ProgData(tree) <Control-1> {tree_at %W %x %y close}
bind $ProgData(tree) <Shift-3> {tree_at %W %x %y expand}
bind $ProgData(tree) <Control-3> {tree_at %W %x %y collapse}

bind $ProgData(tree) <Down> tree_next
bind $ProgData(tree) <Up> tree_prev
bind $ProgData(tree) <Left> tree_collapse
bind $ProgData(tree) <Right> tree_expand
bind $ProgData(tree) <Key-space> tree_toggle
bind $ProgData(tree) <Insert> tree_insert
bind $ProgData(tree) <Return> tree_rename
bind $ProgData(tree) <Delete> tree_delete

set ProgData(menu) [menu .nodemenu -tearoff 0]
$ProgData(menu) add command -label "Expand" -command tree_expand
$ProgData(menu) add command -label "Collapse" -command tree_collapse
$ProgData(menu) add command -label "Info..." -command cgns_info
$ProgData(menu) add separator
set ProgData(insert) [menu .nodemenu.insert -tearoff 0]
$ProgData(menu) add cascade -label "Insert" -menu $ProgData(insert)
$ProgData(menu) add command -label "Rename" -command tree_rename
$ProgData(menu) add command -label "Delete" -command tree_delete

#---------- node data

set wl 12
set we 30

frame .main.node
place .main.node -relx $ProgData(seppos) -x $ProgData(sepwd) \
  -relwidth [expr 1.0 - $ProgData(seppos)] -width -$ProgData(sepwd) \
  -rely 0 -relheight 1

#----- node description

FrameCreate .main.node.node -text "Node Description" -font $Font(bold)
pack .main.node.node -side top -pady 2 -fill x
set node [FrameGet .main.node.node]

set f [frame $node.parent]
pack $f -side top -fill x
label $f.lab -text "Parent Node" -width $wl -anchor w
pack $f.lab -side left
entry $f.ent -textvariable Node(parent) -width $we -highlightthickness 0
pack $f.ent -side left -fill x -expand 1
entry_balloon $f.ent

set f [frame $node.name]
pack $f -side top -fill x
label $f.lab -text "Node Name" -width $wl -anchor w
pack $f.lab -side left
ComboboxCreate $f.cb -width 5 -variable Node(name) \
  -command set_node_name
pack $f.cb -side top -fill x -expand 1
entry_balloon $f.cb.ent

proc set_node_name {w n} {
  global Node NodeSave CGNSnodes _Combobox
  set name [ComboboxValue $w $n]
  if {$NodeSave(lnode) == ""} {
    set Node(label) [lindex $CGNSnodes($name) 1]
    set Node(type)  [lindex $CGNSnodes($name) 2]
    set Node(dim)   [lindex $CGNSnodes($name) 3]
  }
  if {[lindex $CGNSnodes($name) 0]} {
    $w.ent selection clear
  } else {
    set _Combobox($w,focus) $w.ent
    $w.ent delete 0 end
    $w.ent insert 0 $name
    $w.ent selection range 0 end
  }
  return $name
}

set f [frame $node.label]
pack $f -side top -fill x
label $f.lab -text "Node Label" -width $wl -anchor w
pack $f.lab -side left
ComboboxCreate $f.cb -width 5 -variable Node(label)
pack $f.cb -side top -fill x -expand 1
entry_balloon $f.cb.ent

#----- link description

FrameCreate .main.node.link -text "Link Description" -font $Font(bold)
pack .main.node.link -side top -pady 2 -fill x
set link [FrameGet .main.node.link]

foreach i {\
  {lfile "Link File"} \
  {lnode "Link Node"}} {
  set j [lindex $i 0]
  set f [frame $link.$j]
  pack $f -side top -fill x
  label $f.lab -text [lindex $i 1] -width $wl -anchor w
  pack $f.lab -side left
  entry $f.ent -textvariable Node($j) -width 5 -highlightthickness 0
  pack $f.ent -side left -fill x -expand 1
  button $f.but -text Browse -padx 0 -pady 0 -command "select_$j $f.but"
  pack $f.but -side right -fill y
  entry_balloon $f.ent
}

#----- data description

FrameCreate .main.node.fmt -text "Data Description" -font $Font(bold)
pack .main.node.fmt -side top -pady 2 -fill x
set fmt [FrameGet .main.node.fmt]

set f [frame $fmt.type]
pack $f -side top -fill x
label $f.lab -text "Data Type" -width $wl -anchor w
pack $f.lab -side left
ComboboxCreate $f.cb -values $DataTypes -variable Node(type) \
  -edit 0 -width 5 -height 5 -command set_data_type -post get_data_index
pack $f.cb -side top -fill x -expand 1

proc set_data_type {w n} {
  global ProgData DataTypes Node
  set oldtype [ComboboxEntry $w]
  set newtype [lindex [lindex $DataTypes $n] 0]
  if {$oldtype == $newtype} {return $newtype}
  node_clear
  if {![string match {?[148]} $newtype] ||
      ![string match {?[148]} $oldtype]} {
    set Node(dim) ""
    set Node(size) 0
    $ProgData(buttons).read configure -state disabled
    return $newtype
  }
  $ProgData(buttons).read configure -state normal
  if {![catch {expr int($Node(size))} size]} {
    if {[string match "X?" $oldtype]} {
      set oldsize [expr 2 * [string index $oldtype 1]]
    } else {
      set oldsize [string index $oldtype 1]
    }
    if {[string match "X?" $newtype]} {
      set newsize [expr 2 * [string index $newtype 1]]
    } else {
      set newsize [string index $newtype 1]
    }
    set Node(size) [expr ($size * $newsize) / $oldsize]
  }
  return $newtype
}

proc get_data_index {w s} {
  global DataTypes
  return [lsearch -glob $DataTypes "$s*"]
}

set f [frame $fmt.dim]
pack $f -side top -fill x
label $f.lab -text Dimensions -width $wl -anchor w
pack $f.lab -side left
entry $f.ent -textvariable Node(dim) -width $we -highlightthickness 0
pack $f.ent -side left -fill x -expand 1

set f [frame $fmt.size]
pack $f -side top -fill x
label $f.lab -text Bytes -width $wl -anchor w
pack $f.lab -side left
entry $f.ent -textvariable Node(size) -width $we \
  -state disabled -cursor {} -highlightthickness 0
pack $f.ent -side left -fill x -expand 1

#---------- buttons

set ProgData(buttons) [frame .main.node.buttons]
pack $ProgData(buttons) -side top -fill x -pady 3
foreach i {create modify read clear delete} {
  button $ProgData(buttons).$i -text $i -width 6 -state disabled \
    -command node_$i
  pack $ProgData(buttons).$i -side left -expand 1
}

#---------- node data

FrameCreate .main.node.data -text "Node Data" -font $Font(bold) -pady 0
pack .main.node.data -side top -pady 2 -fill both -expand 1
set data [FrameGet .main.node.data]

set f [frame $data.list]
pack $f -side top -fill both -expand 1 -pady 2

scrollbar $f.ys -orient vertical -command "$f.text yview" \
  -takefocus 0 -highlightthickness 0
pack $f.ys -side right -fill y

scrollbar $f.xs -orient horizontal -command "$f.text xview" \
  -takefocus 0 -highlightthickness 0
pack $f.xs -side bottom -fill x

set ProgData(text) [text $f.text -width 40 -height 5 \
  -wrap none -xscrollcommand "$f.xs set" \
  -yscrollcommand "$f.ys set" -highlightthickness 0]
pack $f.text -side top -fill both -expand 1

bind $ProgData(text) <ButtonRelease-3> cgns_data
bind $ProgData(text) <Tab> {tkTabToWindow [tk_focusNext %W];break}
bind $ProgData(text) <Shift-Tab> {tkTabToWindow [tk_focusPrev %W];break}

bind $ProgData(text) <KeyRelease> get_location
bind $ProgData(text) <ButtonRelease-1> get_location

set f [frame $data.loc]
pack $f -side top -fill x -pady 2

label $f.lab -text Line
entry $f.ent -width 10 -textvariable ProgData(linenum)
pack $f.lab $f.ent -side left
bind $f.ent <Return> {set_location %W}

label $f.val -textvariable ProgData(lineval)
pack $f.val -side left -padx 5

frame $f.cnt
pack $f.cnt -side right
label $f.cnt.lab -text "Values/Line"
entry $f.cnt.ent -width 8 -textvariable ProgData(linecnt)
pack $f.cnt.lab $f.cnt.ent -side left
bind $f.cnt.ent <Return> {set_perline %W}

proc get_location {} {
  global ProgData
  set linenum [lindex [split [$ProgData(text) index insert] .] 0]
  set ProgData(linenum) $linenum
  if {$ProgData(linecnt) == ""} {
    set ProgData(lineval) ""
  } else {
    set ProgData(lineval) "([expr $ProgData(linecnt) * ($linenum - 1) + 1])"
  }
}

proc set_location {w} {
  global ProgData
  if {![catch {expr int([$w get])} linenum]} {
    catch {
      $ProgData(text) mark set insert $linenum.0
      $ProgData(text) see $linenum.0
    }
  }
  get_location
  focus $ProgData(text)
}

proc set_perline {w} {
  if {[catch {expr int([$w get])} cnt] || $cnt < 1} {
    set cnt 0
  }
  node_read $cnt
}

#---------- update buttons when node changes

trace variable Node(parent) w check_node
trace variable Node(name) w check_node

#========== procedures ===============================================

proc do_quit {} {
  global ProgData
  if {![remove_backup]} return
  catch ADFclose
  if {$ProgData(tmpfile) != ""} {
    catch {file delete $ProgData(tmpfile)}
  }
  foreach i $ProgData(reg,vals) {
    catch {tclreg_set $ProgData(reg,key) $i $ProgData($i)}
  }
  catch units_write
  catch tclreg_close
  catch {WinHtml close}
  destroy .
  exit 0
}

image create photo img_about -data {\
R0lGODlhIAAgALMAAAAAAIAAAACAAICAAAAAgIAAgACAgICAgMDAwP8AAAD/AP//AAAA//8A\
/wD//////yH5BAEAAAgALAAAAAAgACAAAATPEMkpmb3Y0s13/kwnTtZjnqipjZwFvHD8rmzF\
yLhMj2Xqo7vW7UcEhjquotIlswGURWbMCSX2jI/XKZHYcrumL/j6KGkf4nAa/VUxsFqumj2X\
n8jmp12+79rdMXl0dH1sdnhvcWJ+YHxtZW93iXqLg45YklmUdY2MmG6aloyPh5GBk6KGYINk\
JlRVPq0Pr7CfJzaySwwEBCZTCBi1brw+QlUWvMm9tx6RusrEzBQzIMEP0MmuG9TVPdjRHGe1\
3wThT8Lk5r45MSjY6sIm2BEAADs=}

proc do_about {} {
  global ProgData
  dialog .about -1 -1 "About ADFviewer" \
"ADFviewer Version 2.5
$ProgData(version)
CGNS Library Version $ProgData(libvers)

Bruce Wedan
brucewedan@hotmail.com" img_about 0 Close
}

proc do_backup {} {
  global ProgData
  if {!$ProgData(backup) || $ProgData(bakfile) != ""} return
  set name "$ProgData(file,name).bak"
  if {[file exists $name]} {
    for {set n 1} {$n < 100} {incr n} {
      if {![file exists $name$n]} {
        append name $n
        break
      }
    }
  }
  dialog .backup -1 -1 "Back Up" "Backing up file to \"$name\"" \
    hourglass 0
  update
  if {![catch {file copy $ProgData(file,name) $name} msg]} {
    set msg ""
  }
  destroy .backup
  if {$msg == ""} {
    set ProgData(bakfile) $name
    menubar_state File normal 6
  } else {
    errormsg $msg
    menubar_state File disabled 6
  }
}

proc remove_backup {} {
  global ProgData
  if {$ProgData(bakfile) != "" &&
    [file exists $ProgData(bakfile)]} {
    set del [dialog .delbak -1 -1 Delete \
      "delete the backup file ?" question 0 Yes No Cancel]
    if {$del == 2} {return 0}
    if {$del == 0} {catch {file delete $ProgData(bakfile)}}
  }
  set ProgData(bakfile) ""
  menubar_state File disabled 6
  return 1
}

proc restore_backup {} {
  global ProgData
  if {$ProgData(bakfile) == "" ||![file exists $ProgData(bakfile)]} return
  catch ADFclose
  if {[catch {file rename -force $ProgData(bakfile) \
      $ProgData(file,name)} msg]} {
    errormsg $msg
    return
  }
  set ProgData(bakfile) ""
  menubar_state File disabled 6
  file_reload
}

#----- configuration

proc follow_links {state} {
  global ProgData
  if {$state == ""} {
    if {$ProgData(follow)} {
      set ProgData(follow) 0
    } else {
      set ProgData(follow) 1
    }
  }
  if {$ProgData(follow)} {
    .toolbar.but.tree.follow configure -relief sunken
  } else {
    .toolbar.but.tree.follow configure -relief raised
  }
  file_reload
}

proc auto_size {{loc .}} {
  global ProgData NewSize
  catch {destroy .size}
  toplevel .size
  wm title .size "Node Data Loading"
  wm transient .size .

  foreach i {maxsize maxcnt} {
    set NewSize($i) $ProgData($i)
  }

  FrameCreate .size.top
  pack .size.top -side top -padx 5 -pady 2 -fill x
  set top [FrameGet .size.top]

  set f [frame $top.size]
  pack $f -side top -fill x -padx 5 -pady 5
  label $f.lab -width 24 -text "Auto Load Data Size (bytes)" -anchor w
  entry $f.ent -width 15 -textvariable NewSize(maxsize)
  pack $f.lab $f.ent -side left

  set f [frame $top.cnt]
  pack $f -side top -fill x -padx 5 -pady 5
  label $f.lab -width 24 -text "Max Data Values per Line" -anchor w
  entry $f.ent -width 15 -textvariable NewSize(maxcnt)
  pack $f.lab $f.ent -side left

  set f [frame .size.but]
  pack $f -side top -fill x -expand 1 -padx 5 -pady 5
  button $f.accept -text Accept -width 8 -default active -command {
    foreach i {maxsize maxcnt} {
      if {![catch {expr int($NewSize($i))} size]} {
        set ProgData($i) $size
      }
    }
    destroy .size
  }
  button $f.default -text Default -width 8 -command {
    foreach i {size cnt} {
      set NewSize(max$i) $ProgData(def$i)
    }
  }
  button $f.cancel -text Cancel -width 8 -command {destroy .size}
  pack $f.accept $f.default $f.cancel -side left -expand 1

  bind .size <Return> "$f.accept invoke"

  center_window .size $loc
  set oldFocus [focus]
  set oldGrab [grab current .size]
  if {$oldGrab != ""} {
    set grabStatus [grab status $oldGrab]
  }
  catch {grab .size}
  tkwait visibility .size
  focus .size
  tkwait window .size
  catch {focus $oldFocus}
  if {$oldGrab != ""} {
    if {$grabStatus == "global"} {
      grab -global $oldGrab
    } else {
      grab $oldGrab
    }
  }
}

#----- node operations

proc check_links {node} {
  set path {}
  foreach p [split [file dirname $node] /] {
    if {$p != ""} {
      append path "/$p"
      if {[ADFlink $path] != {}} {return 1}
    }
  }
  return 0
}

proc recursive_link {node link} {
  set path {}
  foreach p [split $node /] {
    if {$p != ""} {
      append path "/$p"
      if {$link == $path} {return 1}
    }
  }
  return 0
}

proc get_node {parent name} {
  if {$name == ""} {return ""}
  if {$parent == ""} {
    if {$name == "/"} {return "/"}
    return ""
  }
  if {$parent == "/"} {return "/$name"}
  return "$parent/$name"
}

proc delete_node {node} {
  global ProgData
  if {$node == ""} return
  if {$node == "/"} {
    errormsg "can't delete root node of ADF file"
    return
  }
  if {[check_links $node]} {
    errormsg "can't delete child of a linked parent node"
    return
  }
  if {$ProgData(verify) &&
    [dialog .delete -1 -1 "Delete Node" \
      "Delete the node \"$node\" ?" warning 0 Yes No Cancel]} {
    return
  }
  do_backup
  if {[catch {ADFdelete $node} msg]} {
    errormsg $msg
    file_reload
  } else {
    TreeDelete $ProgData(tree) $node
    update_node [TreeSelectionGet $ProgData(tree)]
  }
}

proc update_node {node} {
  global ProgData Node NodeSave CGNSnodes CGNSnodeChildren
  set Node(trace) 0
  node_clear
  if {$node == {}} {
    array set Node {
      parent ""
      name ""
      node ""
      label ""
      type ""
      dim ""
      size ""
      lnode ""
      lfile ""
      link 0
    }
    set size 0
  } else {
    set ProgData(start) $node
    set Node(node) $node
    if {$node == "/"} {
      set Node(parent) {}
      set Node(name) $node
    } else {
      set Node(parent) [file dirname $node]
      set Node(name) [file tail $node]
    }
    catch {ADFlabel $node} Node(label)
    if {![catch {ADFtype $node} type]} {
      set Node(type) [string toupper $type]
    } else {
      set Node(type) $type
    }
    catch {ADFdimensions $node} Node(dim)
    if {[catch {ADFsize $node} Node(size)]} {
      set size 0
    } else {
      set size $Node(size)
    }
    if {[catch {ADFlink $node} link]} {
      set Node(lnode) $link
    } else {
      set Node(lnode) [lindex $link 0]
      set Node(lfile) [lindex $link 1]
    }
    set Node(link) [check_links $node]
  }

  foreach i [array names Node] {
    set NodeSave($i) $Node($i)
  }

  set node [FrameGet .main.node.node]
  set link [FrameGet .main.node.link]
  set fmt [FrameGet .main.node.fmt]
  if {$ProgData(readonly) || $Node(node) == "/" || $Node(link)} {
    $node.parent.ent configure -state disabled -cursor {}
    ComboboxConfig $node.name.cb -state disabled
    ComboboxConfig $node.label.cb -state disabled
    foreach i {lfile lnode} {
      $link.$i.ent configure -state disabled -cursor {}
      $link.$i.but configure -state disabled
    }
    ComboboxConfig $fmt.type.cb -state disabled
    $fmt.dim.ent configure -state disabled -cursor {}
  } else {
    $node.parent.ent configure -state normal -cursor xterm
    if {$Node(parent) == "/"} {
      set names $CGNSnodeChildren(/)
    } elseif {[catch {ADFlabel $Node(parent)} label] ||
      ![info exists CGNSnodeChildren($label)]} {
      set names ""
    } else {
      set names $CGNSnodeChildren($label)
    }
    ComboboxConfig $node.name.cb -values $names -state normal
    foreach i {lfile lnode} {
      $link.$i.ent configure -state normal -cursor xterm
      $link.$i.but configure -state normal
    }
    if {$Node(lnode) == ""} {
      set vallist {}
      foreach n $names {
        set val [lindex $CGNSnodes($n) 1]
        if {[lsearch $vallist $val] < 0} {
          lappend vallist $val
        }
      }
      ComboboxConfig $node.label.cb -values $vallist -state normal
      ComboboxConfig $fmt.type.cb -state normal
      $fmt.dim.ent configure -state normal -cursor xterm
    } else {
      ComboboxConfig $node.label.cb -state disabled
      ComboboxConfig $fmt.type.cb -state disabled
      $fmt.dim.ent configure -state disabled -cursor {}
    }
  }

  update idletasks
  set Node(trace) 1
  check_node
  if {!$size} {
    $ProgData(buttons).read configure -state disabled
  } else {
    $ProgData(buttons).read configure -state normal
    if {$ProgData(autoload) && [expr $size <= $ProgData(maxsize)]} {
      node_read
    }
  }
}

proc show_node {node} {
  global ProgData
  TreeSelectionSet $ProgData(tree) $node
  update_node $node
}

proc check_node {args} {
  global ProgData Node NodeSave
  if {!$Node(trace)} return
  set parent [string trim $Node(parent)]
  set name   [string trim $Node(name)]
  set node   [get_node $parent $name]

  if {$ProgData(readonly)} {
    foreach b {create modify delete} {
      $ProgData(buttons).$b configure -state disabled
    }
    if {$NodeSave(node) == ""} {
      $ProgData(buttons).read configure -state disabled
    } else {
      $ProgData(buttons).read configure -state normal
    }
    $ProgData(text) configure -state disabled -cursor {} -takefocus 0
    return
  }

  if {$NodeSave(node) == "" || $NodeSave(link)} {
    foreach b {modify delete} {
      $ProgData(buttons).$b configure -state disabled
    }
    if {$NodeSave(node) == ""} {
      $ProgData(buttons).read configure -state disabled
    } else {
      $ProgData(buttons).read configure -state normal
    }
    if {$node != "" && [ADFnode $node] == ""} {
      $ProgData(buttons).create configure -state normal
      $ProgData(text) configure -state normal -cursor xterm -takefocus 1
    } else {
      $ProgData(buttons).create configure -state disabled
      $ProgData(text) configure -state disabled -cursor {} -takefocus 0
    }
    return
  }

  if {$node == $NodeSave(node)} {
    $ProgData(buttons).read configure -state normal
    if {$node == "/"} {
      foreach b {create modify delete} {
        $ProgData(buttons).$b configure -state disabled
      }
      $ProgData(text) configure -state disabled -cursor {} -takefocus 0
    } else {
      $ProgData(buttons).create configure -state disabled
      foreach b {modify delete} {
        $ProgData(buttons).$b configure -state normal
      }
      if {[string trim $Node(lnode)] == ""} {
        $ProgData(text) configure -state normal -cursor xterm -takefocus 1
      } else {
        $ProgData(text) configure -state disabled -cursor {} -takefocus 0
      }
    }
    return
  }

  foreach b {create modify read delete} {
    $ProgData(buttons).$b configure -state disabled
  }
  $ProgData(text) configure -state disabled -cursor {} -takefocus 0
  if {$node == "" || [ADFnode $node] != ""} return

  if {$parent == "/" || [ADFnode $parent] == "node"} {
    foreach b {create modify} {
      $ProgData(buttons).$b configure -state normal
    }
    $ProgData(text) configure -state normal -cursor xterm -takefocus 1
  }
}

proc add_node {parent child args} {
  global ProgData Font
  set tree $ProgData(tree)
  set node $parent/$child
  if {[string first / $child] >= 0 || [catch {ADFlink $node} link]} {
    if {$parent == ""} {set parent /}
    TreeInsert $tree $child -dir $parent -icon invalid -fill red
    return
  }
  if {$link == {}} {
    set img img
    set opts $args
  } else {
    if {[lindex $link 1] == "" &&
      [recursive_link $node [lindex $link 0]]} {
      TreeInsert $tree $node -icon badlink -fill red
      return
    }
    set img link
    set opts "-fill blue -tag link"
  }
  if {[catch {ADFnumchild $node} numchild]} {
    TreeInsert $tree $node -icon bad$img -fill red
    return
  }
  if {$numchild < 1} {
    eval TreeInsert $tree {$node} -icon file$img $args
    return
  }
  eval TreeInsert $tree {$node} -icon dir$img $args
  if {$link != {} && !$ProgData(follow)} return
  if {[catch {ADFchildren $node} children] || $children == {}} {
    for {set n 1} {$n <= $numchild} {incr n} {
      if {[catch {ADFchildname $node $n} name]} {
        TreeInsert $tree invalid$n -dir $node -icon invalid -fill red
      } else {
        eval add_node {$node} {$name} $opts
      }
    }
  } else {
    foreach n $children {
      eval add_node {$node} {$n} $opts
    }
  }
}

proc build_tree {} {
  global ProgData
  if {[catch {ADFchildren /} children]} {
    TreeInsert $ProgData(tree) / -icon badimg
    return
  }
  dialog .build -1 -1 "Reading..." \
    "Reading nodes and building node tree" hourglass 0 {}
  .build configure -cursor watch
  . configure -cursor watch
  update
  TreeInsert $ProgData(tree) / -icon dirimg
  foreach n $children {
    add_node "" $n
  }
  destroy .build
  . configure -cursor {}
}

#----- tree

proc tree_show {w x y} {
  set node [TreeAt $w $x $y]
  if {$node != ""} {
    show_node $node
  }
}

proc tree_info {w x y} {
  set node [TreeAt $w $x $y]
  if {$node == ""} return
  if {$node != [TreeSelectionGet $w]} {
    show_node $node
  }
  cgns_info
}

proc insert_menu {w node} {
  global ProgData CGNSnodes CGNSnodeChildren
  set allowed {}
  if {$node == "/"} {
    set parent ""
    set type /
  } else {
    set parent $node
    if [catch {ADFlabel $node} type] {set type ""}
  }
  if {$type != "" && [info exists CGNSnodeChildren($type)]} {
    set existing {}
    foreach child [TreeGet $w $node -children] {
      if [info exists CGNSnodes($child)] {
        if [lindex $CGNSnodes($child) 0] {
            lappend existing $child
        } else {
            lappend existing [lindex $CGNSnodes($child) 1]
        }
      } else {
        if {![catch {ADFlabel $parent/$child} lab]} {
          lappend existing $lab
        }
      }
    }
    if {[lsearch $CGNSnodeChildren($type) PointList] >= 0} {
      foreach n {PointList PointRange ElementList ElementRange} {
        if {[lsearch $existing $n] >= 0} {
          lappend existing PointList PointRange ElementList ElementRange
          break
        }
      }
      if {$type == "GridConnectivity_t"} {
        if {[lsearch $existing PointListDonor] >= 0 ||
            [lsearch $existing CellListDonor] >= 0} {
          lappend existing PointListDonor CellListDonor
        }
      }
    }
    foreach n $CGNSnodeChildren($type) {
      if [lindex $CGNSnodes($n) 0] {
        set i $n
      } else {
        set i [lindex $CGNSnodes($n) 1]
      }
      if {[lindex $CGNSnodes($n) 4] || [lsearch $existing $i] < 0} {
        lappend allowed $n
      }
    }
  }
  destroy $ProgData(insert)
  set m [menu $ProgData(insert) -tearoff 0]
  $m add command -label "New Node" -command tree_insert
  if {$allowed != {}} {
    $m add separator
    foreach n $allowed {
      $m add command -label $n -command "tree_insert $n"
    }
  }
}

proc tree_menu {w x y} {
  global ProgData
  set node [TreeAt $w $x $y]
  if {$node == ""} return
  if {$node != [TreeSelectionGet $w]} {
    show_node $node
  }
  if {$ProgData(readonly) || [TreeGet $w $node -tag] == "link"} {
    if {[TreeGet $w $node -children] == {}} return
    foreach n {0 1} {
      $ProgData(menu) entryconfigure $n -state normal
    }
    foreach n {4 5 6} {
      $ProgData(menu) entryconfigure $n -state disabled
    }
  } else {
    if {[TreeGet $w $node -children] == {}} {
      $ProgData(menu) entryconfigure 0 -state disabled
      $ProgData(menu) entryconfigure 1 -state disabled
    } else {
      $ProgData(menu) entryconfigure 0 -state normal
      $ProgData(menu) entryconfigure 1 -state normal
    }
    if {[string match "*link" [TreeGet $w $node -icon]]} {
      $ProgData(menu) entryconfigure 4 -state disabled
    } else {
      $ProgData(menu) entryconfigure 4 -state normal
      insert_menu $w $node
    }
    if {$node == "/"} {
      $ProgData(menu) entryconfigure 2 -state disabled
      $ProgData(menu) entryconfigure 5 -state disabled
      $ProgData(menu) entryconfigure 6 -state disabled
    } else {
      $ProgData(menu) entryconfigure 2 -state normal
      $ProgData(menu) entryconfigure 5 -state normal
      $ProgData(menu) entryconfigure 6 -state normal
    }
  }
  $ProgData(menu) post [expr [winfo rootx $ProgData(tree)] + $x] \
    [expr [winfo rooty $ProgData(tree)] + $y]
}

proc tree_toggle {{node ""}} {
  global ProgData
  if {$node == ""} {
    set node [TreeSelectionGet $ProgData(tree)]
    if {$node == ""} return
  }
  TreeToggle $ProgData(tree) $node
}

proc tree_expand {{node ""}} {
  global ProgData
  if {$node == ""} {
    set node [TreeSelectionGet $ProgData(tree)]
    if {$node == ""} return
  }
  TreeExpand $ProgData(tree) $node
}

proc tree_open {{node ""}} {
  global ProgData
  if {$node == ""} {
    set node [TreeSelectionGet $ProgData(tree)]
    if {$node == ""} return
  }
  TreeOpenLevel $ProgData(tree) $node
}

proc tree_collapse {{node ""}} {
  global ProgData
  if {$node == ""} {
    set node [TreeSelectionGet $ProgData(tree)]
    if {$node == ""} return
  }
  TreeCollapse $ProgData(tree) $node
}

proc tree_close {{node ""}} {
  global ProgData
  if {$node == ""} {
    set node [TreeSelectionGet $ProgData(tree)]
    if {$node == ""} return
  }
  TreeCloseLevel $ProgData(tree) $node
}

proc tree_at {w x y mode} {
  set node [TreeAt $w $x $y]
  if {$node == ""} return
  if {$node != [TreeSelectionGet $w]} {
    show_node $node
  }
  switch $mode {
    expand {TreeExpand $w $node}
    collapse {TreeCollapse $w $node}
    open {tree_open $node}
    close {tree_close $node}
  }
}

proc tree_insert {{name "New Node"}} {
  global ProgData CGNSnodes
  if {$ProgData(readonly)} return
  set w $ProgData(tree)
  set parent [TreeSelectionGet $w]
  if {$parent == ""} return
  set icon [TreeGet $w $parent -icon]
  set open [TreeGet $w $parent -open]
  TreeSet $w $parent -icon dirimg -open 1
  if {$parent == "/"} {
    set root ""
  } else {
    set root $parent
  }
  set node "$root/$name"
  if {[ADFnode $node] != ""} {
    for {set n 1} {$n < 100} {incr n} {
      if {[ADFnode "$node$n"] == ""} {
        append node "$n"
        break
      }
    }
  }
  if [info exists CGNSnodes($name)] {
    set cgnsnode $CGNSnodes($name)
    set fixed [lindex $cgnsnode 0]
  } else {
    set cgnsnode {}
    set fixed 0
  }
  TreeInsert $w $node -icon fileimg
  if {!$fixed} {
    set name [TreeEdit $w $node]
    if {$name == ""} {
      TreeSet $w $parent -icon $icon -open $open
      TreeDelete $w $node
      return
    }
    TreeMove $w $node $name
  }
  do_backup
  if {[catch {ADFcreate "$root/$name"} msg]} {
    errormsg $msg
    file_reload
    return
  }
  TreeSelectionSet $w "$root/$name"
  if {$cgnsnode != {}} {
    set labl [lindex $cgnsnode 1]
    set type [lindex $cgnsnode 2]
    set dims [lindex $cgnsnode 3]
    catch {ADFlabel "$root/$name" $labl}
    if {$type == "C1"} {
      if {$labl == "DimensionalUnits_t"} {
        set data [list Kilogram Meter Second Kelvin Radian]
      } elseif {$labl == "AdditionalUnits_t"} {
        set data [list Ampere Mole Candela]
      } else {
        set data Null
      }
      if {$dims == {}} {set dims [string length $data]}
      catch {ADFwrite "$root/$name" $type $dims $data}
    } elseif {$type != {} && $dims != {}} {
      catch {ADFwrite "$root/$name" $type $dims 0}
    } else {
      if {$type != {}} {
        catch {ADFtype "$root/$name" $type}
      }
      if {$dims != {}} {
        catch {ADFdimensions "$root/$name" $dims}
      }
    }
  }
  update_node "$root/$name"
}

proc tree_delete {} {
  global ProgData
  if {$ProgData(readonly)} return
  delete_node [TreeSelectionGet $ProgData(tree)]
}

proc tree_rename {} {
  global ProgData
  if {$ProgData(readonly)} return
  set w $ProgData(tree)
  set node [TreeSelectionGet $w]
  if {$node == ""} return
  set name [TreeEdit $w $node]
  if {$name == "" || $name == [file tail $node]} return
  do_backup
  if {[catch {ADFname $node $name} msg]} {
    errormsg $msg
  } else {
    TreeMove $w $node $name
    set node [get_node [file dirname $node] $name]
  }
  file_reload
  if {[ADFnode $node] != ""} {
    TreeSelectionSet $w $node
    update_node $node
  }
}

proc tree_prev {{update 1}} {
  global ProgData
  set tree $ProgData(tree)
  set cur [TreeSelectionGet $tree]
  if {$cur == ""} {
    set node /
  } else {
    set node [TreePrev $tree $cur]
  }
  if {$node != ""} {
    TreeSelectionSet $tree $node
    TreeSee $tree $node
    if {$update} {
      update_node $node
    }
  }
}

proc tree_next {{update 1}} {
  global ProgData
  set tree $ProgData(tree)
  set cur [TreeSelectionGet $tree]
  if {$cur == ""} {
    set node /
  } else {
    set node [TreeNext $tree $cur]
  }
  if {$node != ""} {
    TreeSelectionSet $tree $node
    TreeSee $tree $node
    if {$update} {
      update_node $node
    }
  }
}

#----- node search

proc find_node {} {
  global ProgData
  set w .find
  catch {destroy $w}
  toplevel $w
  wm title $w "Find Node..."
  wm transient $w .
  wm protocol $w WM_DELETE_WINDOW {set ProgData(done) 0}

  set f [frame $w.f1]
  pack $f -side top -fill x -padx 5 -pady 5 -fill x
  label $f.lab -text "Node:"
  pack $f.lab -side left
  entry $f.ent -relief sunken -width 20 -textvariable ProgData(find) \
    -highlightthickness 0
  pack $f.ent -side left -fill x -expand 1

  set f [frame $w.f2]
  pack $f -side left -fill y -padx 5 -pady 5
  checkbutton $f.case -text "Match Case" -variable ProgData(case) \
    -onvalue 1 -offvalue 0
  checkbutton $f.top -text "Start from Root" \
    -variable ProgData(fromtop) -onvalue 1 -offvalue 0
  pack $f.case $f.top -side top -anchor w -expand 1

  set sel [TreeSelectionGet $ProgData(tree)]
  if {$sel == "" || ![TreeExists $ProgData(tree) $sel]} {
    set ProgData(start) /
    $f.top configure -state disabled
  } else {
    set ProgData(start) $sel
  }

  set f [frame $w.f3]
  pack $f -side right -fill y -padx 5 -pady 5
  button $f.find -text Find -width 6 -default active \
    -command {set ProgData(done) 1}
  button $f.cancel -text Cancel -width 6 -command {set ProgData(done) 0}
  pack $f.find $f.cancel -side top -expand 1

  bind $w <Return> {set ProgData(done) 1}

  center_window $w .
  set oldFocus [focus]
  set oldGrab [grab current $w]
  if {$oldGrab != ""} {
    set grabStatus [grab status $oldGrab]
  }
  catch {grab $w}
  tkwait visibility $w
  focus $w.f1.ent
  tkwait variable ProgData(done)
  destroy $w
  catch {focus $oldFocus}
  if {$oldGrab != ""} {
    if {$grabStatus == "global"} {
      grab -global $oldGrab
    } else {
      grab $oldGrab
    }
  }

  if {$ProgData(done) && $ProgData(find) != ""} {
    if {$ProgData(fromtop)} {
      set ProgData(start) /
    }
    find_again
  }
}

proc find_again {} {
  global ProgData
  if {$ProgData(find) == ""} {
    find_node
    return
  }
  if {$ProgData(start) == ""} {set ProgData(start) /}
  set node [TreeFind $ProgData(tree) $ProgData(start) \
    $ProgData(find) $ProgData(case)]
  if {$node == ""} {
    errormsg "node not found"
    set ProgData(start) /
    return
  }
  TreeSee $ProgData(tree) $node
  TreeSelectionSet $ProgData(tree) $node
  update_node $node
  set ProgData(start) $node
}

#----- file operations

proc file_stats {} {
  global ProgData
  set ProgData(file,size) "[file size $ProgData(file,name)] bytes"
  if {[catch ADFformat format]} {
    set ProgData(file,fmt) unknown
  } else {
    set ProgData(file,fmt) $format
  }
  set ProgData(file,type) [CGNSfile $ProgData(file)]
  if {[file writable $ProgData(file,name)] && $ProgData(tmpfile) == ""} {
    set ProgData(file,mode) "read/write"
  } else {
    set ProgData(file,mode) "read-only"
  }
  foreach t {mtime atime} {
    if {[catch {clock format [file $t $ProgData(file,name)] \
      -format "%a %b %d %Y %T"} time]} {
      set ProgData(file,$t) unknown
    } else {
      set ProgData(file,$t) $time
    }
  }
  if {[catch {ADFread /CGNSLibraryVersion} cgns]} {
    set ProgData(filevers) ""
    set ProgData(file,cgns) unknown
  } else {
    set ProgData(filevers) $cgns
    set ProgData(file,cgns) "CGNS $cgns"
  }
}

proc file_new {} {
  global ProgData tcl_platform Font
  set w .newfile
  catch {destroy $w}
  toplevel $w
  wm title $w "New File..."
  wm transient $w .
  wm protocol $w WM_DELETE_WINDOW {set ProgData(done) 0}

  FrameCreate $w.file -text "File Name" -font $Font(bold)
  pack $w.file -side top -pady 2 -fill x
  set file [FrameGet $w.file]

  entry $file.ent -textvariable ProgData(newfile) -width 30
  pack $file.ent -side left -fill x -expand 1
  button $file.but -text Browse -pady 0 -command {
    set fname [FileSave "New File" $ProgData(newfile) . \
      {{{CGNS Files} {.adf .cgns .cgn}} {{All Files} {*}}} adf]
    if {$fname != ""} {
      set ProgData(newfile) $fname
    }
  }
  pack $file.but -side right -fill y

  FrameCreate $w.format -text "File Format" -font $Font(bold)
  pack $w.format -side top -pady 2 -fill x
  set fmt [FrameGet $w.format]

  set n 1
  foreach type {\
    {NATIVE CRAY} \
    {IEEE_BIG_32 IEEE_BIG_64} \
    {IEEE_LITTLE_32 IEEE_LITTLE_64}} {
    set f [frame $fmt.f$n]
    pack $f -side left -expand 1
    foreach i $type {
      set j [string tolower $i]
      radiobutton $f.$j -text $i -variable ProgData(format) -value $i
      pack $f.$j -side top -anchor w
    }
    incr n
  }

  set f [frame $w.but]
  pack $f -side top -pady 5
  button $f.accept -text Accept -width 6 -default active \
    -command {set ProgData(done) 1}
  button $f.cancel -text Cancel -width 6 -command {set ProgData(done) 0}
  pack $f.accept $f.cancel -side left -padx 5

  bind $w <Return> {set ProgData(done) 1}

  center_window $w .
  set oldFocus [focus]
  set oldGrab [grab current $w]
  if {$oldGrab != ""} {
    set grabStatus [grab status $oldGrab]
  }
  catch {grab $w}
  tkwait visibility $w
  focus $file.ent
  $file.ent selection range 0 end
  tkwait variable ProgData(done)
  catch {focus $oldFocus}
  destroy $w
  if {$oldGrab != ""} {
    if {$grabStatus == "global"} {
      grab -global $oldGrab
    } else {
      grab $oldGrab
    }
  }

  if {$ProgData(done)} {
    catch ADFclose
    if {[file exists $ProgData(newfile)] &&
        [catch {file delete $ProgData(newfile)} msg]} {
      errormsg $msg
      return
    }
    catch {ADFopen $ProgData(newfile) new $ProgData(format)}
    file_load $ProgData(newfile)
  }
}

proc file_load {{inpfile ""}} {
  global ProgData tcl_platform
  if {$inpfile == ""} {
    set inpfile [FileOpen "Open File" $ProgData(file) . \
      {{{CGNS Files} {.adf .cgns .cgn}} {{All Files} {*}}}]
    if {$inpfile == ""} return
  }
  if [catch {CGNSfile $inpfile} type] {
    errormsg $type
    return
  }
  set dir [file dirname $inpfile]
  set fname [file tail $inpfile]
  set build [lindex $ProgData(version) 0]
  if {$type == $build} {
    set convert ""
  } else {
    set convert $type$build
    if {![info exists ProgData($convert)] || $ProgData($convert) == ""} {
      errormsg "$fname is $type\nand $type to $build convertor not found"
      return
    }
    set dir $ProgData(tmpdir)
    set cmd "$ProgData($convert) $inpfile $dir/$fname"
    if {![run_command "Converting $type to $build" $cmd]} return
  }
  catch ADFclose
  remove_backup
  if {$ProgData(tmpfile) != ""} {
    catch {file delete $ProgData(tmpfile)}
    set ProgData(tmpfile) ""
  }
  if {[catch {cd $dir} msg]} {
    errormsg $msg
    if {$convert != ""} {
      catch {file delete $dir/$fname}
    }
    return
  }
  set dir [pwd]
  if {[file writable $fname] && $convert == ""} {
    set status old
    set ProgData(readonly) 0
  } else {
    set status read_only
    set ProgData(readonly) 1
  }

  dialog .open -1 -1 "Opening..." \
    "Opening and reading $fname" hourglass 0 {}
  .open configure -cursor watch
  . configure -cursor watch
  update
  if {![catch {ADFopen $fname $status} msg]} {
    set msg ""
  }
  destroy .open
  . configure -cursor {}
  if {$msg != ""} {
    errormsg $msg
    if {$convert != ""} {
      catch {file delete $dir/$fname}
    }
    return
  }

  set ProgData(file,name) $fname
  if {$tcl_platform(platform) == "windows"} {
    set ProgData(file,dir) [join [split $dir /] \\]
  } else {
    set ProgData(file,dir) $dir
  }
  set ProgData(file) $inpfile
  if {$convert != ""} {
    set ProgData(tmpfile) $dir/$fname
  }
  file_stats
  TreeDelete $ProgData(tree) /
  build_tree

  if {$ProgData(readonly)} {
    append fname " (read only)"
  }
  wm title . "ADFviewer : $fname"
  foreach i {reload save info} {
    .toolbar.but.file.$i configure -state normal
  }
  foreach i {2 4 5} {
    menubar_state File normal $i
  }
  foreach i {tree.expand tree.collapse find.first find.again} {
    .toolbar.but.$i configure -state normal
  }
  foreach i {1 2 4 5} {
    menubar_state Tree normal $i
  }

  if {$ProgData(filevers) == ""} {
    set state disabled
  } else {
    set state normal
  }

  if {$ProgData(cgnsversion) == ""} {
    .toolbar.but.tools.version configure -state disabled
    menubar_state Tools disabled 0
  } else {
    .toolbar.but.tools.version configure -state $state
    menubar_state Tools $state 0
  }

  if {$ProgData(cgnscheck) == ""} {
    .toolbar.but.tools.check configure -state disabled
    menubar_state Tools disabled 1
  } else {
    .toolbar.but.tools.check configure -state $state
    menubar_state Tools  $state 1
  }

  if {$ProgData(cgnsplot) == ""} {
    .toolbar.but.tools.plot configure -state disabled
    menubar_state Tools disabled 2
  } else {
    .toolbar.but.tools.plot configure -state $state
    menubar_state Tools $state 2
  }

  if {$ProgData(cgnscalc) == ""} {
    .toolbar.but.tools.calc configure -state disabled
    menubar_state Tools disabled 3
  } else {
    .toolbar.but.tools.calc configure -state $state
    menubar_state Tools $state 3
  }

  foreach i $ProgData(menucfg) {
    catch {[lindex $i 0] entryconfigure [lindex $i 1] -state $state}
  }

  $ProgData(buttons).clear configure -state normal
  update_node ""
}

proc file_reload {} {
  global ProgData
  if {$ProgData(file,name) == ""} return
  catch {cd $ProgData(file,dir)}
  if {[file writable $ProgData(file,name)] && $ProgData(tmpfile) == ""} {
    set status old
    set ProgData(readonly) 0
    wm title . "ADFviewer : $ProgData(file,name)"
  } else {
    set status read_only
    set ProgData(readonly) 1
    wm title . "ADFviewer : $ProgData(file,name) (read only)"
  }
  if {[catch {ADFopen $ProgData(file,name) $status} msg]} {
    errormsg $msg
  } else {
    build_tree
  }
  set node [TreeSelectionGet $ProgData(tree)]
  if {$node != "" && ![TreeVisible $ProgData(tree) $node]} {
    set node ""
  }
  update_node $node
}

proc file_save {} {
  global ProgData tcl_platform
  if {$ProgData(file,name) == ""} return
  set fname [FileSave "Save File" $ProgData(file) . \
      {{{CGNS Files} {.adf .cgns .cgn}} {{All Files} {*}}} adf]
  if {$fname == ""} return
  if {[same_file $fname $ProgData(file)]} {
    errormsg "current file and save file are the same"
    return
  }
  if {[catch {file copy -force $ProgData(file) $fname} msg]} {
    errormsg $msg
  } else {
    catch {file attributes $fname -readonly 0}
    file_load $fname
  }
}

proc file_info {{loc .}} {
  global ProgData tcl_platform
  if {$ProgData(file,name) == ""} return
  file_stats
  if {[winfo exists .info]} {
    wm deiconify .info
    raise .info
    return
  }
  toplevel .info
  wm title .info "File Information"
  wm resizable .info 0 0
  set wl 10

  frame .info.main
  pack .info.main -side top -padx 5 -pady 5

  set base [frame .info.main.base -relief groove -bd 2]
  pack $base -side top -padx 5 -pady 5

  foreach i {\
    {name Name:} \
    {dir Location:} \
    {size Size:} \
    {sep1 {}} \
    {type Type:} \
    {fmt Format:} \
    {cgns Version:} \
    {sep2 {}} \
    {mode Mode:} \
    {mtime Modified:} \
    {atime Accessed:}} {
    set j [lindex $i 0]
    if {[lindex $i 1] == {}} {
      set f [frame $base.$j -relief groove -bd 1 -height 2]
      pack $f -side top -fill x -pady 2
    } else {
      set f [frame $base.$j]
      pack $f -side top -anchor w -padx 5
      label $f.lab -text [lindex $i 1] -width $wl -anchor w
      label $f.dat -textvariable ProgData(file,$j)
      pack $f.lab $f.dat -side left
    }
  }

  set f [frame .info.main.but]
  pack $f -side top

  button $f.close -text Close -default active -command {destroy .info}
  button $f.stats -text Update -command file_stats
  pack $f.close $f.stats -side left -padx 5

  bind .info <Return> {destroy .info}

  center_window .info $loc
}

#----- link selection

proc select_lfile {{loc .}} {
  global ProgData Node tcl_platform
  set lfile [FileOpen "Link File" $Node(lfile) $loc \
      {{{CGNS Files} {.cgn .cgns .adf}} {{ADF Files} {.adf}} \
      {{All Files} {*}}}]
  if {$lfile == ""} return
  if {[same_file $lfile $ProgData(file)]} {
    set Node(lfile) ""
  } elseif {[same_file [file dirname $lfile] \
    [file dirname $ProgData(file)]]} {
    set Node(lfile) [file tail $lfile]
  } elseif {$tcl_platform(platform) == "windows"} {
    set Node(lfile) [join [split $lfile /] \\]
  } else {
    set Node(lfile) $lfile
  }
}

proc select_lnode {{loc .}} {
  global Node ProgData Font
  if {$Node(lfile) == ""} {
    set lfile $ProgData(file)
  } else {
    set lfile $Node(lfile)
  }
  if {![file exists $lfile]} {
    errormsg "can't browse - link file does not exist"
    return
  }
  if {[catch {cd [file dirname $lfile]} msg] ||
      [catch {ADFopen [file tail $lfile] read_only} msg]} {
    errormsg $msg
    file_reload
    return
  }
  set tree $ProgData(tree)

  set w .lnode
  catch {destroy $w}
  toplevel $w
  wm title $w "Select Link Node"
  wm transient $w .
  wm withdraw $w

  label $w.file -text [file tail $lfile]
  pack $w.file -side top -padx 5 -pady 5

  set f [frame $w.top]
  pack $f -side top -fill both -expand 1 -padx 5

  set ProgData(tree) $f.tree
  scrollbar $f.ys -orient vertical -command "$ProgData(tree) yview"
  pack $f.ys -side right -fill y

  scrollbar $f.xs -orient horizontal -command "$ProgData(tree) xview"
  pack $f.xs -side bottom -fill x

  TreeCreate $ProgData(tree) -width 200 -height 300 -relief sunken \
    -bd 2 -yscrollcommand "$f.ys set" -xscrollcommand "$f.xs set" \
    -font $Font(normal) -lines $ProgData(showlines)
  pack $ProgData(tree) -side left -fill both -expand 1

  bind $ProgData(tree) <1> {
    set node [TreeAt %W %x %y]
    if {$node != ""} {
      TreeSelectionSet %W $node
    }
  }
  bind $ProgData(tree) <Double-1> {TreeToggle %W [TreeAt %W %x %y]}

  bind $w <Down> {tree_next 0}
  bind $w <Up> {tree_prev 0}
  bind $w <Left> tree_collapse
  bind $w <Right> tree_expand
  bind $w <Key-space> tree_toggle

  set f [frame $w.but]
  pack $f -side top -fill x -padx 5 -pady 5
  button $f.accept -text Accept -width 6 -default active -command {
    if {[TreeSelectionGet $ProgData(tree)] != ""} {
      set Node(done) 1
    }
  }
  button $f.cancel -text Cancel -width 6 -command {set Node(done) 0}
  pack $f.accept $f.cancel -side left -expand 1

  bind $w <Return> "$f.accept invoke"

  build_tree
  TreeSee $ProgData(tree) $Node(lnode)
  TreeSelectionSet $ProgData(tree) $Node(lnode)

  center_window $w $loc
  set oldFocus [focus]
  set oldGrab [grab current $w]
  if {$oldGrab != ""} {
    set grabStatus [grab status $oldGrab]
  }
  catch {grab $w}
  tkwait visibility $w
  focus $w
  tkwait variable Node(done)

  if {$Node(done)} {
    set Node(lnode) [TreeSelectionGet $ProgData(tree)]
  }
  set ProgData(tree) $tree

  destroy $w
  catch {focus $oldFocus}
  if {$oldGrab != ""} {
    if {$grabStatus == "global"} {
      grab -global $oldGrab
    } else {
      grab $oldGrab
    }
  }

  catch {cd $ProgData(file,dir)}
  if {$ProgData(readonly)} {
    set status read_only
  } else {
    set status old
  }
  if {[catch {ADFopen $ProgData(file,name) $status} msg]} {
    errormsg $msg
  }
}

#----- operation buttons

proc node_create {} {
  global ProgData Node
  set parent [string trim $Node(parent)]
  set name   [string trim $Node(name)]
  if {$parent == "" || $name == ""} {
    errormsg "need to specify parent node and node name"
    return
  }
  set node [get_node $parent $name]
  if {[ADFnode $node] != ""} {
    errormsg "node \"$node\" already exists"
    return
  }
  do_backup
  foreach i {label lfile lnode type dim} {
    set $i [string trim $Node($i)]
  }
  if {$lnode == ""} {
    if {[catch {ADFcreate $node} msg]} {
      errormsg msg
    }
  } else {
    if {[catch {ADFlink $node $lnode $lfile} msg]} {
      errormsg $msg
    }
  }
  if {$label != "" && [catch {ADFlabel $node $label} msg]} {
    errormsg $msg
  }

  if {$lnode == "" && [string match {?[148]} $type]} {
    set data [string trim [$ProgData(text) get 1.0 end]]
    if {$data != ""} {
      if {$dim == ""} {
        if {$type == "C1"} {
          set dim [string length $data]
        } else {
          set dim [llength $data]
        }
      }
      if {$type == "C1" && [llength $dim] > 1} {
        set data [split $data "\n"]
      }
      if {[catch {ADFwrite $node $type $dim $data} msg]} {
        errormsg $msg
      }
    }
  }

  file_reload
  if {[ADFnode $node] != ""} {
    TreeSee $ProgData(tree) $node
    TreeSelectionSet $ProgData(tree) $node
    update_node $node
  }
}

proc node_modify {} {
  global ProgData Node NodeSave
  if {$NodeSave(node) == "" || $NodeSave(node) == "/" ||
    [ADFnode $NodeSave(node)] == ""} return
  foreach i {parent name label lfile lnode type dim} {
    set $i [string trim $Node($i)]
  }
  if {$parent == "" || $name == ""} return
  set node [get_node $parent $name]

  if {$node != $NodeSave(node)} {
    if {[ADFnode $node] != ""} {
      errormsg "node \"$node\" already exists"
      return
    }
  }
  if {$lfile == ""} {
    if {$lnode != ""} {
      if {[recursive_link $node $lnode]} {
        errormsg "link is recursive"
        return
      }
    }
  } else {
    if {$lnode == ""} {
      errormsg "must specify the link node for a link"
      return
    }
  }

  if {$lnode != $NodeSave(lnode) ||
      $lfile != $NodeSave(lfile)} {
    do_backup
    catch {ADFdelete $NodeSave(node)}
    if {$lnode == ""} {
      if {[catch {ADFcreate $node} msg]} {
        errormsg $msg
      }
      set NodeSave(label) ""
    } else {
      if {[catch {ADFlink $node $lnode $lfile} msg]} {
        errormsg $msg
      }
      set label $NodeSave(label)
    }
  } else {
    if {$node != $NodeSave(node)} {
      do_backup
      if {$parent != $NodeSave(parent)} {
        if {[catch {ADFmove $NodeSave(node) $parent} msg]} {
          errormsg $msg
          file_reload
          return
        }
      }
      if {$name != $NodeSave(name)} {
        set tmpnode [get_node $parent $NodeSave(name)]
        if {[catch {ADFname $tmpnode $name} msg]} {
          errormsg msg
        }
      }
    }
  }

  if {$label != $NodeSave(label)} {
    do_backup
    if {[catch {ADFlabel $node $label} msg]} {
      errormsg $msg
    }
  }

  if {$node != $NodeSave(node)} {
    TreeMove $ProgData(tree) $NodeSave(node) $node
    TreeSee $ProgData(tree) $node
    TreeSelectionSet $ProgData(tree) $node
  }

  if {$lnode != ""} {
    file_reload
    return
  }
  if {$type == "MT"} {
    if {$NodeSave(type) != "MT"} {
      do_backup
      if {[catch {ADFtype $node MT} msg] ||
          [catch {ADFdimensions $node {}} msg]} {
        errormsg $msg
      }
    }
    file_reload
    return
  }

  set data [string trim [$ProgData(text) get 1.0 end]]
  if {$data == ""} {
    if {$dim != $NodeSave(dim)} {
      do_backup
      if {[catch {ADFdimensions $node $dim} msg]} {
        errormsg $msg
      }
    }
    if {$type != $NodeSave(type)} {
      do_backup
      if {[catch {ADFtype $node $type} msg]} {
        errormsg $msg
      }
    }
  } else {
    if {$dim == ""} {
      if {$type == "C1"} {
        set dim [string length $data]
      } else {
        set dim [llength $data]
      }
    }
    if {$type == "C1" && [llength $dim] > 1} {
      set data [split $data "\n"]
    }
    do_backup
    if {[catch {ADFwrite $node $type $dim $data} msg]} {
      errormsg $msg
    }
  }
  file_reload
}

proc values_perline {} {
  global ProgData NodeSave
  if {$NodeSave(name) == "ElementConnectivity" &&
      $NodeSave(label) == "DataArray_t" &&
      ![catch {ADFlabel $NodeSave(parent)} label] &&
      $label == "Elements_t" &&
      ![catch {ADFdimensions $NodeSave(parent)} dim] &&
      $dim == 2 && ![catch {ADFread $NodeSave(parent)} data]} {
    switch [lindex $data 0] {
      2 {return 1}
      3 {return 2}
      4 - 5  {return 3}
      6 - 14 {return 6}
      7 - 10 {return 4}
      8 - 17 {return 8}
      9  {return 9}
      11 {return 10}
      12 {return 5}
      13 {return 14}
      15 {return 15}
      16 {return 18}
      18 {return 20}
      19 {return 27}
    }
  }
  set len [lindex $NodeSave(dim) 0]
  if {$ProgData(maxcnt) && $len > $ProgData(maxcnt)} {
    return $ProgData(maxcnt)
  }
  return $len
}

proc node_read {{perline 0}} {
  global ProgData NodeSave
  set state [$ProgData(text) cget -state]
  $ProgData(text) configure -state normal
  $ProgData(text) delete 1.0 end
  set ProgData(linecnt) ""
  set node $NodeSave(node)
  if {$node != "" && [ADFnode $node] != ""} {
    . configure -cursor watch
    update
    if {[catch {ADFread $node} data]} {
      $ProgData(text) insert end $data
    } else {
      set dim [llength $NodeSave(dim)]
      set cnt 1
      for {set n 0} {$n < $dim} {incr n} {
        set cnt [expr $cnt * [lindex $NodeSave(dim) $n]]
      }
      if {$NodeSave(type) == "C1"} {
        if {$dim == 1 && $perline < 1} {
          $ProgData(text) insert end $data
        } else {
          if {$perline > 0} {
            set len $perline
            set ProgData(linecnt) $len
          } else {
            set len [lindex $NodeSave(dim) 0]
          }
          for {set n1 0} {$n1 < $cnt} {incr n1 $len} {
            set n2 [expr $n1 + $len - 1]
            $ProgData(text) insert end "[string range $data $n1 $n2]\n"
          }
        }
      } else {
        if {$perline > 0} {
          set len $perline
        } else {
          set len [values_perline]
        }
        for {set n1 0} {$n1 < $cnt} {incr n1 $len} {
          set n2 [expr $n1 + $len - 1]
          $ProgData(text) insert end "[lrange $data $n1 $n2]\n"
        }
        set ProgData(linecnt) $len
      }
    }
    . configure -cursor {}
  }
  $ProgData(text) mark set insert 1.0
  $ProgData(text) configure -state $state
  get_location
}

proc node_clear {} {
  global ProgData
  set state [$ProgData(text) cget -state]
  if {$state == "normal"} {
    $ProgData(text) delete 1.0 end
  } else {
    $ProgData(text) configure -state normal
    $ProgData(text) delete 1.0 end
    $ProgData(text) configure -state $state
  }
  set ProgData(linenum) ""
  set ProgData(lineval) ""
  set ProgData(linecnt) ""
}

proc node_delete {} {
  global ProgData Node
  set node [get_node [string trim $Node(parent)] \
    [string trim $Node(name)]]
  if {$node != "" && $node != "/"} {
    delete_node $node
  }
}

#---------- utility routines

proc full_path {filename} {
  global tcl_platform
  set cdir [pwd]
  if {[catch {cd [file dirname $filename]}]} {
    return $filename
  }
  set pathname "[pwd]/[file tail $filename]"
  cd $cdir
  if {$tcl_platform(platform) == "windows"} {
    return [string tolower $pathname]
  }
  return $pathname
}

proc same_file {file1 file2} {
  global tcl_platform
  if {$tcl_platform(platform) == "windows"} {
    set file1 [join [split $file1 \\] /]
    set file2 [join [split $file2 \\] /]
  }
  set n [expr [file exists $file1] + [file exists $file2]]
  if {$n == 1} {return 0}
  if [string compare [full_path $file1] [full_path $file2]] {return 0}
  return 1
}

proc ignore_quit {} {}

proc run_command {title cmd {height 10} {width 60}} {
  global ProgData tcl_platform

  if {$tcl_platform(platform) == "windows"} {
    set exe [join [split [lindex $cmd 0] \\] /]
    set cmd [lreplace $cmd 0 0 $exe]
  }

  set w .command
  catch {destroy $w}
  toplevel $w
  wm title $w $title
  wm transient $w .
  wm protocol $w WM_DELETE_WINDOW ignore_quit

  frame $w.f
  pack $w.f -side top -fill both -padx 5 -pady 5 -expand 1
  scrollbar $w.f.s -command "$w.f.t yview"
  pack $w.f.s -side right -fill y
  text $w.f.t -width $width -height $height -yscroll "$w.f.s set"
  pack $w.f.t -side top -fill both -expand 1

  button $w.close -text Close -command "destroy $w" -state disabled
  pack $w.close -side bottom

  $w.f.t insert end "EXECUTING:\n$cmd\n\n"

  center_window $w .
  tkwait visibility $w
  update

  if {[catch {open |$cmd r} f]} {
    destroy $w
    errormsg $f
    return 0
  }
  set ProgData(status) 0
  fileevent $f readable "run_event $w $f"
  tkwait window $w
  return $ProgData(status)
}

proc run_event {w f} {
  global ProgData
  if {![eof $f]} {
    fconfigure $f -blocking 0
    while {[gets $f line] >= 0} {
      $w.f.t insert end "$line\n"
    }
    fconfigure $f -blocking 1
    $w.f.t yview -pickplace end
    update idletasks
    return
  }
  if {[catch {close $f} msg]} {
    errormsg $msg
    $w.f.t insert end "\nERROR:\n$msg"
    set ProgData(status) 0
  } else {
    $w.f.t insert end "\nFINISHED\n"
    set ProgData(status) 1
  }
  $w.f.t yview -pickplace end
  $w.close configure -state normal
}

#---------- initialize and display window

if {[info commands etLoadIcon] != ""} {
  update idletasks
  catch {etLoadIcon adfviewer}
}

update_node ""

if {$argc} {
  set fname [lindex $argv [expr $argc - 1]]
  if {[file isfile $fname] && [file readable $fname]} {
    file_load $fname
  }
}

