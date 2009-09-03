set num_sides 5

proc write_reference {fn bn} {
  cg_goto $fn $bn
  cg_state_write "reference state quantities"

  cg_goto $fn $bn ReferenceState_t 1
  cg_dataclass_write Dimensional
  cg_units_write Kilogram Meter Second Kelvin Radian

  set n 0
  foreach state {\
      {Mach 0.2 {}} \
      {VelocitySound 330.0 {0 1 -1 0 0}} \
      {VelocityMagnitude 66.0 {0 1 -1 0 0}} \
      {VelocityUnitVectorX 1.0 {}} \
      {VelocityUnitVectorY 0.0 {}} \
      {VelocityUnitVectorZ 0.0 {}} \
      {Reynolds 3.0e6 {}} \
      {Temperature 300.0 {0 0 0 1 0}} \
      {Pressure 1.0e5 {1 -1 -2 0, 0}} \
      {LengthReference 10.0 {0 1 0 0 0}}} {
    incr n
    set exps [lindex $state 2]
    cg_goto $fn $bn ReferenceState_t 1
    cg_array_write [lindex $state 0] RealSingle 1 1 [lindex $state 1]
    cg_gorel $fn DataArray_t $n
    if {[llength $exps] > 0} {
        cg_exponents_write RealSingle $exps
    } else {
        cg_dataclass_write NondimensionalParameter
    }
  }
}

proc write_equationset {fn bn} {
# flow equation set

  cg_goto $fn $bn
  cg_equationset_write 3

  cg_gorel $fn FlowEquationSet_t 1
  cg_governing_write NSTurbulent
  cg_model_write GasModel_t Ideal
  cg_model_write ViscosityModel_t SutherlandLaw
  cg_model_write ThermalConductivityModel_t PowerLaw
  cg_model_write TurbulenceClosure_t EddyViscosity
  cg_model_write TurbulenceModel_t Algebraic_BaldwinLomax

  cg_dataclass_write Dimensional
  cg_units_write Kilogram Meter Second Kelvin Radian

# diffusion model

  set diff {0 0 1 0 0 0}

  cg_goto $fn $bn FlowEquationSet_t 1 GoverningEquations_t 1
  cg_diffusion_write $diff
  cg_gorel $fn .. 0 TurbulenceModel_t 1
  cg_diffusion_write $diff

# gas model

  set g 1.4
  set R 53.352

  cg_goto $fn $bn FlowEquationSet_t 1 GasModel_t 1
  cg_dataclass_write DimensionlessConstant
  cg_array_write SpecificHeatRatio RealSingle 1 1 $g
  cg_array_write IdealGasConstant RealSingle 1 1 $R
  cg_gorel $fn DataArray_t 2
  cg_dataclass_write Dimensional
  cg_units_write Slug Foot Second Rankine Radian

# viscosity model

  set ts 110.6;
  set mu 1.716e-5;

  cg_goto $fn $bn FlowEquationSet_t 1 ViscosityModel_t 1
  cg_array_write SutherlandLawConstant RealSingle 1 1 $ts
  cg_array_write ViscosityMolecularReference RealSingle 1 1 $mu

# thermal conductivity model

  set exp 0.666;

  cg_goto $fn $bn FlowEquationSet_t 1 ThermalConductivityModel_t 1
  cg_array_write PowerLawExponent RealSingle 1 1 $exp
  cg_gorel $fn DataArray_t 1
  cg_dataclass_write DimensionlessConstant

# turbulence closure model

  set pt 0.9;

  cg_goto $fn $bn FlowEquationSet_t 1 TurbulenceClosure_t 1
  cg_array_write PrandtlTurbulent RealSingle 1 1 $pt
  cg_gorel $fn DataArray_t 1
  cg_dataclass_write DimensionlessConstant
}

proc write_coords {fn bn zn ioff joff koff} {
  global num_sides

  set xcoord {}
  set ycoord {}
  set zcoord {}
  for {set k 0} {$k < $num_sides} {incr k} {
    set z [expr $k + $koff]
    for {set j 0} {$j < $num_sides} {incr j} {
      set y [expr $j + $joff]
      for {set i 0} {$i < $num_sides} {incr i} {
        lappend xcoord [expr $i + $ioff]
        lappend ycoord $y
        lappend zcoord $z
      }
    }
  }

  cg_coord_write $fn $bn $zn RealSingle CoordinateX $xcoord
  cg_coord_write $fn $bn $zn RealSingle CoordinateY $ycoord
  cg_coord_write $fn $bn $zn RealSingle CoordinateZ $zcoord
}

proc node_index {i j k} {
  global num_sides
  return [expr $i + $num_sides * (($j - 1) + \
    $num_sides * ($k - 1))]
}

proc cell_index {i j k} {
  global num_sides
  return [expr $i + ($num_sides - 1) * (($j - 1) + \
    ($num_sides - 1) * ($k - 1))]
}

proc write_elements {fn bn zn} {
  global num_sides;

# create and write hex elements

  set n2 [expr $num_sides * $num_sides]
  set elem {}
  set nelem 0
 
  for {set k 1} {$k < $num_sides} {incr k} {
    for {set j 1} {$j < $num_sides} {incr j} {
      for {set i 1} {$i < $num_sides} {incr i} {
        set n [node_index $i $j $k]
        lappend elem $n [expr $n + 1] \
          [expr $n + 1 + $num_sides] [expr $n + $num_sides]
        incr n $n2
        lappend elem $n [expr $n + 1] \
          [expr $n + 1 + $num_sides] [expr $n + $num_sides]
        incr nelem
      }
    }
  }

  cg_section_write $fn $bn $zn Elements HEXA_8 1 $nelem 0 $elem

# create and write face (quad) elements

  set quad {}
  set nquad 0
  set parent {}
  set pface {}
  set n3 [expr $num_sides * ($num_sides + 1)]

  set i 1
  for {set k 1} {$k < $num_sides} {incr k} {
    for {set j 1} {$j < $num_sides} {incr j} {
      set n [node_index $i $j $k]
      lappend quad $n [expr $n + $n2] \
        [expr $n + $n3] [expr $n + $num_sides]
      lappend parent [cell_index $i $j $k]
      lappend pface 5
      incr nquad
    }
  }

  set i $num_sides
  for {set k 1} {$k < $num_sides} {incr k} {
    for {set j 1} {$j < $num_sides} {incr j} {
      set n [node_index $i $j $k]
      lappend quad $n [expr $n + $num_sides] \
        [expr $n + $n3] [expr $n + $n2]
      lappend parent [cell_index [expr $i - 1] $j $k]
      lappend pface 3
      incr nquad
    }
  }

  set j 1
  for {set k 1} {$k < $num_sides} {incr k} {
    for {set i 1} {$i < $num_sides} {incr i} {
      set n [node_index $i $j $k]
      lappend quad $n [expr $n + 1] \
        [expr $n + 1 + $n2] [expr $n + $n2]
      lappend parent [cell_index $i $j $k]
      lappend pface 2
      incr nquad
    }
  }

  set j $num_sides
  for {set k 1} {$k < $num_sides} {incr k} {
    for {set i 1} {$i < $num_sides} {incr i} {
      set n [node_index $i $j $k]
      lappend quad $n [expr $n + $n2] \
        [expr $n + 1 + $n2] [expr $n + 1]
      lappend parent [cell_index $i [expr $j - 1] $k]
      lappend pface 4
      incr nquad
    }
  }

  set k 1
  for {set j 1} {$j < $num_sides} {incr j} {
    for {set i 1} {$i < $num_sides} {incr i} {
      set n [node_index $i $j $k]
      lappend quad $n [expr $n + $num_sides] \
        [expr $n + 1 + $num_sides] [expr $n + 1]
      lappend parent [cell_index $i $j $k]
      lappend pface 1
      incr nquad
    }
  }

  set k $num_sides
  for {set j 1} {$j < $num_sides} {incr j} {
    for {set i 1} {$i < $num_sides} {incr i} {
      set n [node_index $i $j $k]
      lappend quad $n [expr $n + 1] \
        [expr $n + 1 + $num_sides] [expr $n + $num_sides]
      lappend parent [cell_index $i $j [expr $k - 1]]
      lappend pface 6
      incr nquad
    }
  }

  for {set n 0} {$n < $nquad} {incr n} {
    lappend parent 0
  }
  for {set n 0} {$n < $nquad} {incr n} {
    lappend parent [lindex $pface $n]
  }
  for {set n 0} {$n < $nquad} {incr n} {
    lappend parent 0
  }

  set ns [expr $nelem + 1]
  set ne [expr $nelem + $nquad] 
  set sn [cg_section_write $fn $bn $zn Faces QUAD_4 $ns $ne 0 $quad]
  cg_parent_data_write $fn $bn $zn $sn $ns $ne $parent
}

proc write_zone_link {fn bn zn basename nodename} {
  set pathname "/$basename/Zone$zn/$nodename"
  cg_goto $fn $bn Zone_t $zn
  cg_link_write $nodename "" $pathname
}

#---------- write the structured base

proc write_structured {fn} {
  global num_sides

  puts "writing structured base"

  set base [cg_base_write $fn Structured 3 3]
  
  cg_goto $fn $base
  cg_descriptor_write Descriptor "Multi-block Structured Grid"
  cg_dataclass_write Dimensional
  cg_units_write Kilogram Meter Second Kelvin Radian
  
  cg_simulation_type_write $fn $base NonTimeAccurate

  write_reference $fn $base
  write_equationset $fn $base

# write zones

  set np $num_sides
  set nc [expr $num_sides - 1]
  set sizes [list $np $np $np $nc $nc $nc 0 0 0]

  set koff [expr 1 - $num_sides]
  for {set n 1} {$n <= 2} {incr n} {
    set name "Zone$n"
    set zone [cg_zone_write $fn $base $name $sizes Structured]
    write_coords $fn $base $zone 0 0 $koff
    set koff 0
  }

# write zone 1 to zone 2 connectivity as 1to1

  set range [list 1 1 $np $np $np $np]
  set drange [list 1 1 1 $np $np 1]
  set trans [list 1 2 3]
  cg_1to1_write $fn $base 1 "1to1 -> Zone2" "Zone2" $range $drange $trans

# write zone 2 to zone 1 connectivity as Abutting1to1

  set range $drange
  set dpts {}
  set ndpnts 0
  for {set j 1} {$j <= $np} {incr j} {
    for {set i 1} {$i <= $np} {incr i} {
      lappend dpts $i $j 1
      incr ndpnts
    }
  }
  cg_conn_write $fn $base 2 "Abutting1to1 -> Zone1" Vertex \
    Abutting1to1 PointRange 2 $range "Zone1" Structured \
    PointListDonor Integer $ndpnts $dpts

# write inlet BC (zone 1) as point range

  cg_boco_write $fn $base 1 Inlet BCInflow PointRange 2 $range

# write outlet BC (zone 2) as point list

  set pts {}
  set npts 0
  for {set j 1} {$j <= $np} {incr j} {
    for {set i 1} {$i <= $np} {incr i} {
      lappend pts $i $j $np
      incr npts
    }
  }
  cg_boco_write $fn $base 2 Outlet BCOutflow PointList $npts $pts

# write zone 1 wall BC as point ranges using a family to group them

  set fam [cg_family_write $fn $base WallFamily]
  cg_fambc_write $fn $base $fam WallBC BCWall

# write out some bogus geometry info for the family */

  set geo [cg_geo_write $fn $base $fam Geometry geometry.file CADsystem]
  cg_part_write $fn $base $fam $geo "imin part"
  cg_part_write $fn $base $fam $geo "imax part"
  cg_part_write $fn $base $fam $geo "jmin part"
  cg_part_write $fn $base $fam $geo "jmax part"

# write the family BC

  set range [list 1 1 1 1 $np $np]
  set bc [cg_boco_write $fn $base 1 imin FamilySpecified PointRange 2 $range]
  cg_goto $fn $base Zone_t 1 ZoneBC_t 1 BC_t $bc
  cg_famname_write WallFamily

  set range [list $np 1 1 $np $np $np]
  set bc [cg_boco_write $fn $base 1 imax FamilySpecified PointRange 2 $range]
  cg_goto $fn $base Zone_t 1 ZoneBC_t 1 BC_t $bc
  cg_famname_write WallFamily

  set range [list 1 1 1 $np 1 $np]
  set bc [cg_boco_write $fn $base 1 jmin FamilySpecified PointRange 2 $range]
  cg_goto $fn $base Zone_t 1 ZoneBC_t 1 BC_t $bc
  cg_famname_write WallFamily

  set range [list 1 $np 1 $np $np $np]
  set bc [cg_boco_write $fn $base 1 jmax FamilySpecified PointRange 2 $range]
  cg_goto $fn $base Zone_t 1 ZoneBC_t 1 BC_t $bc
  cg_famname_write WallFamily

# write zone 2 wall BC as point list

  set pts {}
  set npts 0
  for {set k 1} {$k <= $np} {incr k} {
    for {set i 1} {$i < $np} {incr i} {
      lappend pts [expr $i + 1] 1 $k
      incr npts
      lappend pts $i $np $k
      incr npts
    }
    for {set j 1} {$j < $np} {incr j} {
      lappend pts 1 $j $k
      incr npts
      lappend pts $np [expr $j + 1] $k
      incr npts
    }
  }
  cg_boco_write $fn $base 2 Walls BCWall PointList $npts $pts

# write solution for zone 1 as vertex with rind points */

  set rind [list 1 1 1 1 1 1]
  set nsol [expr ($np + 2) * ($np + 2) * ($np + 2)]
  set solution {}
  for {set n 0} {$n < $nsol} {incr n} {
    lappend solution $n
  }
  set sol [cg_sol_write $fn $base 1 VertexSolution Vertex]
  cg_goto $fn $base Zone_t 1 FlowSolution_t $sol
  cg_rind_write $rind
  cg_field_write $fn $base 1 $sol RealSingle Density $solution

# write solution for zone 2 as cell center with rind points

  set rind [list 0 0 1 1 1 1]
  set nsol [expr $nc * ($nc + 2) * ($nc + 2)]
  set solution {}
  for {set n 0} {$n < $nsol} {incr n} {
    lappend solution $n
  }
  set sol [cg_sol_write $fn $base 2 CellCenterSolution CellCenter]
  cg_goto $fn $base Zone_t 2 FlowSolution_t $sol
  cg_rind_write $rind
  cg_field_write $fn $base 2 $sol RealSingle Density $solution
}

#---------- write the unstructured base

proc write_unstructured {fn} {
  global num_sides

  puts "writing unstructured base"

  set base [cg_base_write $fn Unstructured 3 3]
  
  cg_goto $fn $base
  cg_descriptor_write Descriptor "Multi-block Unstructured Grid"
  cg_dataclass_write NormalizedByDimensional
  cg_units_write Kilogram Meter Second Kelvin Radian

# write zones

  set np $num_sides
  set nc [expr $num_sides - 1]
  set nnode [expr $np * $np * $np]
  set nelem [expr $nc * $nc * $nc]
  set nface [expr 6 * $nc * $nc]
  set sizes [list $nnode $nelem $nface]

  set koff [expr 1 - $num_sides]
  for {set n 1} {$n <= 2} {incr n} {
    set name "Zone$n"
    set zone [cg_zone_write $fn $base $name $sizes Unstructured]
    write_coords $fn $base $zone 0 0 $koff
    write_elements $fn $base $zone
    set koff 0
  }

# zone 1 to zone 2 connectivity as Abutting1to1 with point range

  set range [list [node_index 1 1 $num_sides] \
                  [node_index $num_sides $num_sides $num_sides]]
  set dpts {}
  set npts 0
  for {set j 1} {$j <= $num_sides} {incr j} {
    for {set i 1} {$i <= $num_sides} {incr i} {
      lappend dpts [node_index $i $j 1]
      incr npts
    }
  }

  cg_conn_write $fn $base 1 "Abutting1to1 -> Zone2" Vertex Abutting1to1 \
    PointRange 2 $range Zone2 Unstructured PointListDonor Integer $npts $dpts

# zone 2 to zone 1 connectivity as Abutting1to1 with point list

  set pts $dpts
  set dpts {}
  for {set j 1} {$j <= $num_sides} {incr j} {
    for {set i 1} {$i <= $num_sides} {incr i} {
      lappend dpts [node_index $i $j $num_sides]
    }
  }

  cg_conn_write $fn $base 2 "Abutting1to1 -> Zone1" Vertex Abutting1to1 \
    PointList $npts $pts Zone1 Unstructured PointListDonor Integer $npts $dpts

#  write inlet BC (zone 1) and outlet BC (zone 2) as element lists
#  and zone 1 and zone 2 walls as element range

  set ne [expr $nc * $nc]
  set ptin {}
  set ptout {}
  set num [expr $nelem + $nface - 2 * $ne]
  for {set n 0} {$n < $ne} {incr n} {
    incr num
    lappend ptin $num
    lappend ptout [expr $num + $ne]
  }
  set range [list [expr $nelem + 1] [expr $nelem + 4 * $ne]]

  cg_boco_write $fn $base 1 Inlet BCInflow ElementList $ne $ptin
  cg_boco_write $fn $base 2 Outlet BCOutflow ElementList $ne $ptout
  cg_boco_write $fn $base 1 Walls BCWall ElementRange 2 $range
  cg_boco_write $fn $base 2 Walls BCWall ElementRange 2 $range

#  write solution for zone 1 as vertex and zone 2 as cell center

  set solution {}
  for {set n 0} {$n < $nnode} {incr n} {
    lappend solution $n
  }
  set sn [cg_sol_write $fn $base 1 VertexSolution Vertex]
  cg_field_write $fn $base 1 $sn RealSingle Density $solution

  set solution {}
  for {set n 0} {$n < $nelem} {incr n} {
    lappend solution $n
  }
  set sn [cg_sol_write $fn $base 2 CellCenterSolution CellCenter]
  cg_field_write $fn $base 2 $sn RealSingle Density $solution
}

#---------- write the mixed structured and unstructured base

proc write_mixed {fn} {
  global num_sides

  puts "writing mixed base"

  set base [cg_base_write $fn Mixed 3 3]
  
  cg_goto $fn $base
  cg_descriptor_write Descriptor "Mixed Structured and Unstructured Grid"
  cg_dataclass_write Dimensional
  cg_units_write Kilogram Meter Second Kelvin Radian

# zone 1 is structured

  set np $num_sides
  set nc [expr $num_sides - 1]
  set sizes [list $np $np $np $nc $nc $nc 0 0 0]

  cg_zone_write $fn $base StructuredZone $sizes Structured
  write_zone_link $fn $base 1 Structured GridCoordinates

# zone 2 is unstructured

  set nnode [expr $np * $np * $np]
  set nelem [expr $nc * $nc * $nc]
  set nface [expr 6 * $nc * $nc]
  set sizes [list $nnode $nelem $nface]

  cg_zone_write $fn $base UnstructuredZone $sizes Unstructured
  write_zone_link $fn $base 2 Unstructured GridCoordinates
  write_zone_link $fn $base 2 Unstructured Elements
  write_zone_link $fn $base 2 Unstructured Faces

# zone 1 -> zone 2 connectivity as point range

  set range [list 1 1 $np $np $np $np]
  set npts [expr $np * $np]
  set dpts {}
  for {set n 1} {$n <= $npts} {incr n} {
    lappend dpts $n
  }

  cg_conn_write $fn $base 1 "Structured -> Unstructured" Vertex Abutting1to1 \
    PointRange 2 $range UnstructuredZone Unstructured PointListDonor \
    Integer $npts, $dpts

# zone 2 -> zone 1 connectivity as point range (k=1 surface)

  set range [list 1 $npts]
  set dpts {}
  for {set j 1} {$j <= $np} {incr j} {
    for {set i 1} {$i <= $np} {incr i} {
      lappend dpts $i $j $np
    }
  }

  cg_conn_write $fn $base 2 "Unstructured -> Structured" Vertex Abutting1to1 \
    PointRange 2 $range StructuredZone Structured PointListDonor \
    Integer $npts $dpts

# write inlet BC (zone 1) and outlet (zone 2) as point range

  set range [list 1 1 1 $np $np 1]
  cg_boco_write $fn $base 1 Inlet BCInflow PointRange 2 $range

  set range [list [expr $nnode - $npts + 1] $nnode]
  cg_boco_write $fn $base 2 Outlet BCOutflow PointRange 2 $range

# write zone 1 and 2 wall BC as point list

  set npts 0
  set pts {}
  for {set k 1} {$k <= $np} {incr k} {
    for {set i 1} {$i < $np} {incr i} {
      lappend pts [expr $i + 1] 1 $k
      incr npts
      lappend pts $i $np $k
      incr npts
    }
    for {set j 1} {$j < $np} {incr j} {
      lappend pts 1 $j $k
      incr npts
      lappend pts $np [expr $j + 1] $k
      incr npts
    }
  }

  cg_boco_write $fn $base 1 Walls BCWall PointList $npts $pts

  set npts 0
  set pts {}
  for {set k 1} {$k <= $np} {incr k} {
    for {set i 1} {$i < $np} {incr i} {
      lappend pts [node_index [expr $i + 1] 1 $k]
      incr npts
      lappend pts [node_index $i $np $k]
      incr npts
    }
    for {set j 1} {$j < $np} {incr j} {
      lappend pts [node_index 1 $j $k]
      incr npts
      lappend pts [node_index $np [expr $j + 1] $k]
      incr npts
    }
  }

  cg_boco_write $fn $base 2 Walls BCWall PointList $npts $pts
}

#---------- write the mismatched base

proc write_mismatched {fn} {
  global num_sides

  puts "writing mismatched base"

  set base [cg_base_write $fn Mismatched 3 3]
  
  cg_goto $fn $base
  cg_descriptor_write Descriptor "Mismatched Grid"
  cg_dataclass_write Dimensional
  cg_units_write Kilogram Meter Second Kelvin Radian

# zone 1 is cartesian

  set np $num_sides
  set nc [expr $num_sides - 1]
  set sizes [list $np $np $np $nc $nc $nc 0 0 0]

  set offset [expr -0.5 * $nc];

  cg_zone_write $fn $base CartesianZone $sizes Structured
  write_coords $fn $base 1 $offset $offset [expr 1 - $np]

# zone 2 is cylindrical

  set nj [expr 2 * $np];
  set sizes [list $np $nj $np $nc [expr $nj - 1] $nc 0 0 0]
  cg_zone_write $fn $base CylindricalZone $sizes Structured

  set twopi 6.2831853
  set dtheta [expr $twopi / ($nj - 1)]
  set rcoord {}
  set tcoord {}
  set zcoord {}
  for {set k 0} {$k < $np} {incr k} {
    for {set j 0} {$j < $nj} {incr j} {
      set y [expr $dtheta * $j]
      for {set i 0} {$i < $np} {incr i} {
        lappend rcoord $i
        lappend tcoord $y
        lappend zcoord $k
      }
    }
  }

  cg_coord_write $fn $base 2 RealSingle CoordinateR $rcoord
  cg_coord_write $fn $base 2 RealSingle CoordinateTheta $tcoord
  cg_coord_write $fn $base 2 RealSingle CoordinateZ $zcoord

# zone 1 -> zone 2 connectivity

  set pts {}
  set dpts {}
  set intp {}
  set npts 0
  for {set j 0} {$j < $np} {incr j} {
    for {set i 0} {$i < $np} {incr i} {
      set x [expr double($i) + $offset]
      set y [expr double($j) + $offset]
      set r [expr sqrt($x * $x + $y * $y)]
      if [expr $r > double($np - 1)] continue
      set ic [expr int($r)]
      if [expr $ic >= $np - 1] {set ic [expr $np - 2]}
      set t [expr atan2($y, $x)]
      if [expr $t < 0.0] {set t [expr $t + $twopi]}
      set jc [expr int($t / $dtheta)]
      if [expr $jc >= $nj - 1] {set jc [expr $nj - 2]}
      lappend pts [expr $i + 1] [expr $j + 1] $np
      lappend dpts [expr $ic + 1] [expr $jc + 1] 1
      lappend intp [expr $r - double($ic)] \
        [expr $t / $dtheta - double($jc)] 0
      incr npts
    }
  }

  set conn [cg_conn_write $fn $base 1 "Cartesian -> Cylindrical" Vertex \
    Abutting PointList $npts $pts CylindricalZone Structured CellListDonor \
    Integer $npts $dpts]
 
  cg_goto $fn $base Zone_t 1 ZoneGridConnectivity_t 1 GridConnectivity_t $conn
  cg_array_write InterpolantsDonor RealSingle 2 [list 3 $npts] $intp

# zone 2 -> zone 1 connectivity

  set pts {}
  set dpts {}
  set intp {}
  set npts 0
  for {set j 0} {$j < $nj} {incr j} {
    for {set i 0} {$i < $np} {incr i} {
      set r $i
      set t [expr double($j) * $dtheta]
      set x [expr cos($t) * $r - $offset]
      set y [expr sin($t) * $r - $offset]
      if [expr $x < 0.0 || $x > double($np - 1) || \
               $y < 0.0 || $y > double($np - 1)] continue
      set ic [expr int($x)]
      if [expr $ic >= $np - 1] {set ic [expr $np - 2]}
      set jc [expr int($y)]
      if [expr $jc >= $np - 1] {set jc [expr $np - 2]}
      lappend pts [expr $i + 1] [expr $j + 1] 1
      lappend dpts [expr $ic + 1] [expr $jc + 1] $np
      lappend intp [expr $x - double($ic)] [expr $y - double($jc)] 0
      incr npts
    }
  }

  set conn [cg_conn_write $fn $base 2 "Cylindrical -> Cartesian" Vertex \
    Abutting PointList $npts $pts CartesianZone Structured CellListDonor \
    Integer $npts $dpts]
 
  cg_goto $fn $base Zone_t 2 ZoneGridConnectivity_t 1 GridConnectivity_t $conn
  cg_array_write InterpolantsDonor RealSingle 2 [list 3 $npts] $intp

# periodic boundary for zone 2

  set range [list 1 1 1 $np 1 $np]
  set drange [list 1 $nj 1 $np $nj $np]
  set trans [list 1 2 3]

  cg_1to1_write $fn $base 2 Periodic CylindricalZone $range $drange $trans

# write inlet BC (zone 1) and outlet (zone 2) as point range

  set range [list 1 1 1 $np $np 1]
  cg_boco_write $fn $base 1 Inlet BCInflow PointRange 2 $range

  set range [list 1 1 $np $np $nj $np]
  cg_boco_write $fn $base 2 Outlet BCOutflow PointRange 2 $range

# write zone 1 and 2 wall BC as point list

  set npts 0
  set pts {}
  for {set k 1} {$k <= $np} {incr k} {
    for {set i 1} {$i < $np} {incr i} {
      lappend pts [expr $i + 1] 1 $k
      incr npts
      lappend pts $i $np $k
      incr npts
    }
    for {set j 1} {$j < $np} {incr j} {
      lappend pts 1 $j $k
      incr npts
      lappend pts $np [expr $j + 1] $k
      incr npts
    }
  }

  cg_boco_write $fn $base 1 Walls BCWall PointList $npts $pts

  set npts 0
  set pts {}
  for {set k 1} {$k <= $np} {incr k} {
    for {set j 1} {$j <= $nj} {incr j} {
      lappend pts $np $j $k
      incr npts
    }
  }

  cg_boco_write $fn $base 2 Walls BCWall PointList $npts $pts
}

set fn [cg_open test.cgns w]
write_structured $fn
write_unstructured $fn
write_mixed $fn
write_mismatched $fn
cg_close $fn

