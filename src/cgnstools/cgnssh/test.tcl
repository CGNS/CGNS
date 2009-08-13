set fn [cg_open test.cgns w]
set base [cg_base_write $fn Base 3 3]

foreach family {family1 family2 family3} {
  set f [cg_family_write $fn $base $family]
  cg_fambc_write $fn $base $f FamBC BCWall
  set g [cg_geo_write $fn $base $f geoname filename cadsystem]
  cg_part_write $fn $base $f $g part
}

foreach zone {zone1 zone2 zone3} {
  set z [cg_zone_write $fn $base $zone {10 20 30 9 19 29 0 0 0} Structured]
}

cg_close $fn

set fn [cg_open test.cgns r]
set nb [cg_nbases $fn]
puts "nbases = $nb"
for {set b 1} {$b <= $nb} {incr b} {
  puts [cg_base_read $fn $b]
  set nf [cg_nfamilies $fn $b]
  puts "nfamilies = $nf"
  for {set f 1} {$f <= $nf} {incr f} {
    puts [cg_family_read $fn $b $f]
  }
  set nz [cg_nzones $fn $b]
  puts "nzones = $nz"
  for {set z 1} {$z <= $nz} {incr z} {
    puts [cg_zone_read $fn $b $z]
  }
}


