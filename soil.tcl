proc Assign_soilProps {} {
    global soilProps
    global soilPropsTMP
    
    # Make all soil layers the same as per user input
    global setAllLayers
    global whichLayer
    global layerItems
    if {$setAllLayers != 0} {
       for {set i 1} {$i <= $soilPropsTMP(Nlayers)} {incr i} {
          foreach item "gamma phi nh eps50 c type depth space multiplier" {
             set soilPropsTMP($item,$i) $soilPropsTMP($item,$whichLayer) 
          }
       }
    }
    set setAllLayers 0

    foreach var [array names soilPropsTMP] {
       if {[info exists soilProps($var)] && $soilProps($var) != $soilPropsTMP($var)} {
          ModelIsDirty
       }
       set soilProps($var) $soilPropsTMP($var)
    }
}

proc AssignTMP_soilProps {} {
    global soilProps
    global soilPropsTMP
    
    foreach var [array names soilProps] {
	set soilPropsTMP($var) $soilProps($var)
    }
}

set soilProps(Nlayers) 3
set soilProps(subgradeHeight) 60.0
set soilProps(waterTableDepth) 3.0
set soilProps(frozenSoilDepth) 8.0
set soilProps(gamma,default) 125.0
set soilProps(phi,default) 28.0
set soilProps(nh,default) 50.0
set soilProps(eps50,default) 0.02
set soilProps(c,default) 2.5
set soilProps(space,default) 1.0
set soilProps(multiplier,default) 1.0
for {set i 1} {$i <= $soilProps(Nlayers)} {incr i} {
    set soilProps(depth,$i) [expr $soilProps(subgradeHeight)/$soilProps(Nlayers)]
    set soilProps(gamma,$i) $soilProps(gamma,default)
    set soilProps(phi,$i) $soilProps(phi,default)
    set soilProps(nh,$i) $soilProps(nh,default)
    set soilProps(type,$i) "Sand"
    set soilProps(eps50,$i) $soilProps(eps50,default)
    set soilProps(c,$i) $soilProps(c,default)
    set soilProps(space,$i) $soilProps(space,default)
    set soilProps(multiplier,$i) $soilProps(multiplier,default)
}
AssignTMP_soilProps


proc ChangeSoilLayers {} {
    global soilProps
    global soilPropsTMP

    if {$soilPropsTMP(Nlayers) <= 0} {
	set soilPropsTMP(Nlayers) 1
    }

    # 1. If adding new layers
    if {$soilPropsTMP(Nlayers) > $soilProps(Nlayers)} {
	# Do nothing if attempting to increase layers but decrease depth
	if {$soilPropsTMP(subgradeHeight) < $soilProps(subgradeHeight)} {
	    set soilPropsTMP(Nlayers) $soilProps(Nlayers)
	    set soilPropsTMP(subgradeHeight) $soilProps(subgradeHeight)
	    return
	}
	for {set i [expr $soilProps(Nlayers)+1]} {$i <= $soilPropsTMP(Nlayers)} {incr i} {
	    # Assign default values
	    set soilPropsTMP(gamma,$i) $soilProps(gamma,default)
	    set soilPropsTMP(phi,$i) $soilProps(phi,default)
	    set soilPropsTMP(nh,$i) $soilProps(nh,default)
	    set soilPropsTMP(type,$i) "Sand"
	    set soilPropsTMP(eps50,$i) $soilProps(eps50,default)
	    set soilPropsTMP(c,$i) $soilProps(c,default)
	    set soilPropsTMP(space,$i) $soilProps(space,default)
	    set soilPropsTMP(multiplier,$i) $soilProps(multiplier,default)
	    
	    # User is not also attempting to increase subgrade depth
	    if {$soilPropsTMP(subgradeHeight) == $soilProps(subgradeHeight)} {
		set soilPropsTMP(depth,$i) 10.0
		set soilPropsTMP(subgradeHeight) [expr $soilProps(subgradeHeight)+10.0]
	    } else {
		# Distribute new subgrade depth to new layers
		set soilPropsTMP(depth,$i) [expr ($soilPropsTMP(subgradeHeight)-$soilProps(subgradeHeight))/($soilPropsTMP(Nlayers)-$soilProps(Nlayers))]
	    }
	}
    }

    # 2. If removing layers
    if {$soilPropsTMP(Nlayers) < $soilProps(Nlayers)} {
	set sgh 0.0
	for {set i 1} {$i <= $soilPropsTMP(Nlayers)} {incr i} {
	    set sgh [expr $sgh + $soilPropsTMP(depth,$i)]
	}
	set soilPropsTMP(subgradeHeight) $sgh
    }

    # 3. If increasing subgrade depth, but not Nlayers
    if {$soilPropsTMP(subgradeHeight) > $soilProps(subgradeHeight) && $soilPropsTMP(Nlayers) == $soilProps(Nlayers)} {
	set increasedDepth [expr $soilPropsTMP(subgradeHeight)-$soilProps(subgradeHeight)]
	set N $soilPropsTMP(Nlayers)
	set soilPropsTMP(depth,$N) [expr $soilPropsTMP(depth,$N)+$increasedDepth]
    }

    # 4. If reducing subgrade depth, but not Nlayers
    if {$soilPropsTMP(subgradeHeight) < $soilProps(subgradeHeight) && $soilPropsTMP(Nlayers) == $soilProps(Nlayers)} {
	set y 0.0
	set sum 0.0
	for {set i 1} {$i <= $soilProps(Nlayers)} {incr i} {
	    set y [expr $y + $soilProps(depth,$i)]
	    if {$y >= $soilPropsTMP(subgradeHeight)} {
		break
	    }
	    set sum [expr $sum + $soilProps(depth,$i)]
	}
	set decreasedDepth [expr $soilPropsTMP(subgradeHeight)-$sum]

	set soilPropsTMP(depth,$i) $decreasedDepth
	set soilPropsTMP(Nlayers) $i ;# remove layers if necessary
    }

}

proc ChangeSoilLayerMenu {m} {
    global soilProps
    global soilPropsTMP

    for {set i [expr $soilProps(Nlayers)+1]} {$i <= $soilPropsTMP(Nlayers)} {incr i} {
	$m add command -label "Layer $i" -command "set whichLayer $i; DefineSoil .soil"
    }
    for {set i $soilProps(Nlayers)} {$i > $soilPropsTMP(Nlayers)} {incr i -1} {
	$m delete [expr $i+1] ;# For water table in first entry
    }
}

# tag -- of spring uniaxialMaterial
# depth -- to location of spring
# xTrib -- tributary length of pile
# pShadow -- p-multiplier for *shadowing*
proc OpenSeesSoilSpring {tag depth {xTrib 1.0} {pShadow 1.0}} {
    global units
    set lb $units(lb)
    set ft $units(ft)
    set ksi $units(ksi)
    set psi $units(psi)
    set pcf $units(pcf)
    set pci $units(pci)
    set in $units(in)
    set deg $units(deg)

    global soilPropsTMP
    set Nlayers $soilPropsTMP(Nlayers)
    set subgradeHeight $soilPropsTMP(subgradeHeight)
    set waterTableDepth $soilPropsTMP(waterTableDepth)
    
    set depthLimit 0.0
    for {set theLayer 1} {$theLayer <= $Nlayers} {incr theLayer} {
	set depthLimit [expr $depthLimit + $soilPropsTMP(depth,$theLayer)]
	if {$depth <= $depthLimit} {
	    break
	}
    }
    
    global sectionPropsTMP
    set b [expr $sectionPropsTMP(Dj,2)*$in]
    set gamma [expr $soilPropsTMP(gamma,$theLayer)*$pcf]
    set x $depth

    global analysisPropsTMP
    set subtractWater $analysisPropsTMP(subtractWater)
    set allElastic $analysisPropsTMP(allElastic)

    set gammaWater [expr {62.4*$pcf}]
    if {$subtractWater && $x > $waterTableDepth} {
       set gamma [expr $gamma-$gammaWater]
    }

    # this is a p-multiplier for the layer
    set pmult $soilPropsTMP(multiplier,$theLayer)

    if {$soilPropsTMP(type,$theLayer) == "N.C. Clay"} {
	set c [expr $soilPropsTMP(c,$theLayer)*$psi]
	set J 0.5
	set eps50 $soilPropsTMP(eps50,$theLayer)
	
	set pu [expr {(3 + $gamma/$c*$x + $J/$b*$x)*$c*$b}]
	if {$pu > [expr {9*$c*$b}]} {
	    set pu [expr {9*$c*$b}]
	}
	set y50 [expr {2.5*$eps50*$b}]
	
	hystereticBackbone ReeseSoftClay $tag [expr {$pu*$xTrib}] $y50 3.0
	
    } elseif {$soilPropsTMP(type,$theLayer) == "O.C. Clay"} {
	set c [expr $soilPropsTMP(c,$theLayer)*$psi]
	set eps50 $soilPropsTMP(eps50,$theLayer)

      # Below water table
	if {$x > $waterTableDepth} {
	    set nh [expr $soilPropsTMP(nh,$theLayer)*$pci]

	    set pc [expr {2*$c*$b + $gamma*$b*$x + 2.83*$c*$x}]
	    if {$pc > [expr {11*$c*$b}]} {
		set pc [expr {11*$c*$b}]
	    }

	    set kx [expr {$nh*$x}]
	    
	    set xOverb [expr {$x/$b}]
	    
	    if {$xOverb >= 4.0} {
		set As 0.6
	    } elseif {$xOverb >= 2.0} {
		set As [expr {0.55 + 0.025*($xOverb-2.0)}]
	    } else {
		set As [expr {0.2 + 0.175*$xOverb}]
	    }
	    
	    set y50 [expr {$eps50*$b}]

	    hystereticBackbone ReeseStiffClayBelowWS $tag $kx $y50 $As [expr {$pc*$xTrib}]

	} else {
	    set J 0.5
	 
	    set pu [expr {(3 + $gamma/$c*$x + $J/$b*$x)*$c*$b}]
	    if {$pu > [expr {9*$c*$b}]} {
		set pu [expr {9*$c*$b}]
	    }
	    set y50 [expr {2.5*$eps50*$b}]

	    hystereticBackbone ReeseSoftClay $tag [expr {$pu*$xTrib}] $y50 4.0
	}

    } elseif {$soilPropsTMP(type,$theLayer) == "Sand"} {
	set phi [expr {$soilPropsTMP(phi,$theLayer)*$deg}]
	
	set nh [expr {$soilPropsTMP(nh,$theLayer)*$pci}]
	# Modify if below water table
	if {$x > $waterTableDepth} {
	    set nh [expr {0.5184*$nh + 9.5797*$pci}]
	}

	set alpha [expr {0.5*$phi}]
	set beta [expr {45.0*$deg + $alpha}]
	set Ko 0.4
	set Ka [expr pow(tan(45.0*$deg-$phi/2),2)]

	set pst [expr {$Ko*$x*tan($phi)*sin($beta)/(tan($beta-$phi)*cos($alpha)) + tan($beta)/tan($beta-$phi)*($b+$x*tan($beta)*tan($alpha)) + $Ko*$x*tan($beta)*(tan($phi)*sin($beta)-tan($alpha)) - $Ka*$b}]
	set pst [expr {$gamma*$x*$pst}]

	set psd [expr {$Ka*$b*$gamma*$depth*(pow(tan($beta),8)-1) + $Ko*$b*$gamma*$depth*tan($phi)*pow(tan($beta),4)}]

	set ps $pst
	if {$psd < $pst} {set ps $psd}

	set kx [expr {$nh*$x}]

	set xOverb [expr {$x/$b}]

	if {$xOverb >= 4.0} {
	    set As 0.88
	} elseif {$xOverb >= 3.0} {
	    set As 1.0
	} elseif {$xOverb >= 2.0} {
	    set As [expr {1.5 - 0.5*($xOverb-2.0)}]
	} else {
	    set As [expr {2.8 - 0.65*$xOverb}]
	}
	set yu [expr {3.0*$b/80.0}]
	set pu [expr {$As*$ps}]

	if {$xOverb >= 4.0} {
	    set Bs 0.5
	} elseif {$xOverb >= 1.5} {
	    set Bs [expr {1.25 - 0.3*($xOverb-1.5)}]
	} else {
	    set Bs [expr {2.25 - 0.6667*$xOverb}]
	}
	set ym [expr {$b/60.0}]
	set pm [expr {$Bs*$ps}]

	hystereticBackbone ReeseSand $tag $kx $ym [expr {$pm*$xTrib}] $yu [expr {$pu*$xTrib}]
    }

    uniaxialMaterial Backbone -$tag $tag
    uniaxialMaterial Multiplier $tag -$tag [expr {$pShadow*$pmult}]

    return [expr {0.2*$b}]
}

set waterTableWindowOpen 0
proc DefineWaterTable {w} {

    global waterTableWindowOpen
    if {$waterTableWindowOpen == 1} {raise $w; return}

    createTopLevel $w "Water Table"

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left
    set m [menu $w.mbar.file.menu]

    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"

    set text(0)  "The input value is the depth from the ground surface to the water table."
    
    createHelp $w $w.mbar water text


    set waterTableWindowOpen 1
    bind $w <Destroy> {set waterTableWindowOpen 0}
    
    global whichColumn

    AssignTMP_soilProps

    frame $w.input

    label $w.input.col -text "Depth to Water Table" 
    grid $w.input.col -row 0 -column 0 -columnspan 2
    entry $w.input.load -width 5 -textvariable soilPropsTMP(waterTableDepth)
    bind $w.input.load <Return> "DefineWaterTable_OK $w"
    grid $w.input.load -row 1 -column 0 -sticky e
    label $w.input.ft -text "ft"
    grid $w.input.ft -row 1 -column 1 -sticky w

    pack $w.input -side top

    frame $w.okcancel
    
    button $w.okcancel.ok -text "OK" -command "DefineWaterTable_OK $w"
    grid $w.okcancel.ok -row 0 -column 0
    button $w.okcancel.cancel -text "Cancel" -command "DefineWaterTable_Cancel $w"
    grid $w.okcancel.cancel -row 0 -column 1

    pack $w.okcancel -side top
}

proc DefineWaterTable_Cancel {w} {
    AssignTMP_soilProps

    destroy $w
}

proc DefineWaterTable_OK {w} {
    
    set ok [CheckWaterTable]
    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input for $ok" -icon error -parent $w
	return
    }

    Assign_soilProps

    DefineModel [getCanvas]

    destroy $w
}

proc CheckWaterTable {} {
    global soilPropsTMP

    set wtd $soilPropsTMP(waterTableDepth)
    if {![isValidDouble $wtd] || $wtd < 0.0} {
	return "water table depth"
    }

}


set frozenSoilWindowOpen 0
proc DefineFrozenSoil {w} {

    global frozenSoilWindowOpen
    if {$frozenSoilWindowOpen == 1} {raise $w; return}

    createTopLevel $w "Frozen Soil"

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left
    set m [menu $w.mbar.file.menu]

    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"

    set text(0)  "The input value is the depth from the ground surface to frozen soil."
    
    createHelp $w $w.mbar frozensoil text


    set frozenSoildWindowOpen 1
    bind $w <Destroy> {set frozenSoilWindowOpen 0}
    
    global whichColumn

    AssignTMP_soilProps

    frame $w.input

    label $w.input.col -text "Depth to Frozen Soil" 
    grid $w.input.col -row 0 -column 0 -columnspan 2
    entry $w.input.load -width 5 -textvariable soilPropsTMP(frozenSoilDepth)
    bind $w.input.load <Return> "DefineFrozenSoil_OK $w"
    grid $w.input.load -row 1 -column 0 -sticky e
    label $w.input.ft -text "ft"
    grid $w.input.ft -row 1 -column 1 -sticky w

    pack $w.input -side top

    frame $w.okcancel
    
    button $w.okcancel.ok -text "OK" -command "DefineFrozenSoil_OK $w"
    grid $w.okcancel.ok -row 0 -column 0
    button $w.okcancel.cancel -text "Cancel" -command "DefineFrozenSoil_Cancel $w"
    grid $w.okcancel.cancel -row 0 -column 1

    pack $w.okcancel -side top
}

proc DefineFrozenSoil_Cancel {w} {
    AssignTMP_soilProps

    destroy $w
}

proc DefineFrozenSoil_OK {w} {
    
    set ok [CheckFrozenSoil]
    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input for $ok" -icon error -parent $w
	return
    }

    Assign_soilProps

    DefineModel [getCanvas]

    destroy $w
}

proc CheckFrozenSoil {} {
    global soilPropsTMP

    set fsd $soilPropsTMP(frozenSoilDepth)
    if {![isValidDouble $fsd] || $fsd < 0.0} {
	return "frozen soil depth"
    }

}


# Create a canvas to view the soil properties
set setAllLayers 0
set soilWindowOpen 0
proc DefineSoil {w} {
    global soilWindowOpen

    # If already open, do not attempt to open a duplicate
    if {$soilWindowOpen == 1} {raise $w; return}

    createTopLevel $w "Soil Properties"

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left
    set m [menu $w.mbar.file.menu]

    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"

    set text(0) "Select the soil type for the indicated layer in order to edit its properties."

    createHelp $w $w.mbar soilType text


    set soilWindowOpen 1
    bind $w <Destroy> {set soilWindowOpen 0}

    AssignTMP_soilProps
    
    global soilPropsTMP
    global whichLayer

    frame $w.input

    set row 0

    label $w.input.labellayer -text "Soil Layer $whichLayer"
    grid $w.input.labellayer -row $row -column 0 -columnspan 3

    incr row

    label $w.input.labeldepth -text "Depth"
    grid $w.input.labeldepth -row $row -column 0
    entry $w.input.entrydepth -textvariable soilPropsTMP(depth,$whichLayer) -width 5
    bind $w.input.entrydepth <Return> "DefineSoil_OK $w"
    grid $w.input.entrydepth -row $row -column 1 -sticky e
    label $w.input.unitsdepth -text "ft"
    grid $w.input.unitsdepth -row $row -column 2 -sticky w

    incr row

    label $w.input.labelmult -text "Multiplier"
    grid $w.input.labelmult -row $row -column 0
    entry $w.input.entrymult -textvariable soilPropsTMP(multiplier,$whichLayer) -width 5
    bind $w.input.entrymult <Return> "DefineSoil_OK $w"
    grid $w.input.entrymult -row $row -column 1 -sticky e
    #label $w.input.unitsmult -text ""
    #grid $w.input.unitsmult -row $row -column 2 -sticky w

    incr row

    label $w.input.labelspacing -text "p-y Spacing"
    grid $w.input.labelspacing -row $row -column 0
    entry $w.input.entryspacing -textvariable soilPropsTMP(space,$whichLayer) -width 5 -state disabled
    bind $w.input.entryspacing <Return> "DefineSoil_OK $w"
    grid $w.input.entryspacing -row $row -column 1 -sticky e
    label $w.input.unitsspacing -text "ft"
    grid $w.input.unitsspacing -row $row -column 2 -sticky w

    set m [tk_optionMenu $w.input.soilmenu soilPropsTMP(type,$whichLayer) junk]
    $m delete 0
    #$m entryconfigure 0 -command {puts HELLO} ;# This works too

    $m insert 0 radiobutton -label "Sand" -variable soilPropsTMP(type,$whichLayer) -command "EnableSoilButtons $w.input Sand"
    $m insert 1 radiobutton -label "N.C. Clay" -variable soilPropsTMP(type,$whichLayer) -command "EnableSoilButtons $w.input SoftClay"
    $m insert 2 radiobutton -label "O.C. Clay" -variable soilPropsTMP(type,$whichLayer) -command "EnableSoilButtons $w.input StiffClay"

    incr row

    label $w.input.type -text "Type"
    grid $w.input.type -row $row -column 0
    grid $w.input.soilmenu -row $row -column 1 -columnspan 2

    incr row

    foreach {lbl ent unt} {"gamma" gamma "pcf"  "phi" phi "deg"  "c" c "psi"  "eps50" eps50 "in/in"  "nh" nh "pci"} {
	label $w.input.label$row -text $lbl -anchor e
	grid $w.input.label$row -row $row -column 0
	entry $w.input.entry$row -width 5 -textvariable soilPropsTMP($ent,$whichLayer)
	bind $w.input.entry$row <Return> "DefineSoil_OK $w"
	grid $w.input.entry$row -row $row -column 1
	label $w.input.units$row -text $unt
	grid $w.input.units$row -row $row -column 2 -sticky w
	incr row
    }

    EnableSoilButtons $w.input $soilPropsTMP(type,$whichLayer)

    global setAllLayers
    set setAllLayers 0

    checkbutton $w.input.all -text "Apply to all soil layers" -variable setAllLayers
    grid $w.input.all -row $row -column 0 -columnspan 3 -sticky ew

    incr row

    button $w.input.py -text "P-Y Curves" -command "SoilPY .soilpy $w"
    grid $w.input.py -row $row -column 0 -columnspan 3 -sticky ew

    pack $w.input -side top

    frame $w.okcancel
    
    button $w.okcancel.ok -text "OK" -command "DefineSoil_OK $w"
    grid $w.okcancel.ok -row 0 -column 0
    button $w.okcancel.cancel -text "Cancel" -command "DefineSoil_Cancel $w"
    grid $w.okcancel.cancel -row 0 -column 1

    pack $w.okcancel -side top
}

proc DefineSoil_Cancel {w} {
    AssignTMP_soilProps

    destroy $w
}

proc DefineSoil_OK {w} {
    
    global soilPropsTMP
    set Nlayers $soilPropsTMP(Nlayers)

    for {set i 1} {$i <= $Nlayers} {incr i} {
       set ok [CheckSoil $i]
       if {[string length $ok] > 0} {
          set ok "Layer $i -- $ok"
          break
       }
    }

    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input: $ok" -icon error -parent $w
	return
    }

    Assign_soilProps

    DefineModel [getCanvas]

    destroy $w
}

proc CheckSoil {whichLayer} {

    global soilPropsTMP

    set s $soilPropsTMP(space,$whichLayer)
    if {![isValidDouble $s] || $s <= 0.0} {
	return "p-y spring spacing"
    }

    set depth $soilPropsTMP(depth,$whichLayer)
    if {![isValidDouble $depth] || $depth <= 0.0} {
	return "layer depth"
    }

    set multiplier $soilPropsTMP(multiplier,$whichLayer)
    if {![isValidDouble $multiplier] || $multiplier <= 0.0} {
	return "p-multiplier"
    }

    set nh $soilPropsTMP(nh,$whichLayer)
    if {![isValidDouble $nh] || $nh <= 0.0} {
	return nh
    }

    set c $soilPropsTMP(c,$whichLayer)
    if {![isValidDouble $c] || $c <= 0.0} {
	return c
    }

    set eps50 $soilPropsTMP(eps50,$whichLayer)
    if {![isValidDouble $eps50] || $eps50 <= 0.0} {
	return eps50
    }

    set phi $soilPropsTMP(phi,$whichLayer)
    if {![isValidDouble $phi] || $phi <= 0.0 || $phi >= 90.0} {
	return phi
    }

    set gamma $soilPropsTMP(gamma,$whichLayer)
    if {![isValidDouble $gamma] || $gamma <= 0.0} {
	return gamma
    }

    set Nlayers $soilPropsTMP(Nlayers)
    set endDepth 0
    for {set i 1} {$i <= $whichLayer} {incr i} {
       set depth $soilPropsTMP(depth,$i)
       set endDepth [expr {$endDepth + $depth}]
    }
    
    global analysisProps
    set buoyantWeight $analysisProps(subtractWater)

    set waterTableDepth $soilPropsTMP(waterTableDepth)

    global units
    set pcf $units(pcf)
    set gamma [expr {$gamma*$pcf}]
    set gammaWater [expr {62.4*$pcf}]

    if {$buoyantWeight && $waterTableDepth < $endDepth && $gamma < $gammaWater} {
       return "gammaSoil < gammaWater"
    }




}

proc EnableSoilButtons {w type} {

   for {set row 5} {$row <= 9} {incr row} {
      $w.entry$row config -state disabled
   }

   set whichRows ""
   if {$type == "Sand"} {
      set whichRows "5 6 9"
   }
   if {$type == "SoftClay" || $type == "Soft Clay" || $type == "N.C. Clay"} {
      set whichRows "5 7 8"
   }
   if {$type == "StiffClay" || $type == "Stiff Clay" || $type == "O.C. Clay"} {
      set whichRows "5 7 8 9"
   }

   foreach row $whichRows {
      $w.entry$row config -state normal
   }

}

set soilPYWindowOpen 0
proc SoilPY {w p} {

    global whichLayer

    set ok [CheckSoil $whichLayer]
    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input for $ok" -icon error -parent $p
	return
    }

    global soilPYWindowOpen
    if {$soilPYWindowOpen == 1} {raise $w; return}

    global units
    set ksi $units(ksi)
    set ft $units(ft)

    global soilPropsTMP

    createTopLevel $w "Soil P-Y Analysis"

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left
    set m [menu $w.mbar.file.menu]

    set width 600
    set height 600
    set graph foo
    set c [createEMUGraph $w $graph $width $height]

    $m add command -label "Print (Ctrl+P)" -command "PrintCanvas $c $w"
    bind $w <Control-Key-p> "PrintCanvas $c $w"

    $m add cascade -label "Export" -menu $m.soils
    set m2 [menu $m.soils]

    $m add separator
    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"


    set soilPYWindowOpen 1
    bind $w <Destroy> {set soilPYWindowOpen 0}

    set startDepth 0.0
    for {set i 1} {$i < $whichLayer} {incr i} {
	set startDepth [expr $startDepth + $soilPropsTMP(depth,$i)]
    }
    set layerDepth $soilPropsTMP(depth,$whichLayer)
    set endDepth [expr $startDepth + $layerDepth]

    set spacing $soilPropsTMP(space,$whichLayer)
    set numSprings 0

    set NpyLayers [expr int($layerDepth/$spacing)]
    set r [expr 0.5*($layerDepth-$spacing*$NpyLayers)]

    # Do first spring if there is a remainder layer
    if {$r > 0.0} {
       set depth [expr {$startDepth + 0.5*$r}]
       set coords($numSprings) [OpenSeesSoilAnalysis $depth $r]
       set coords($numSprings,depth) $depth
       incr numSprings
    }

    # Do springs in main p-y layes
    for {set i 1} {$i <= $NpyLayers} {incr i} {
       set depth [expr {$startDepth + $r + ($i-0.5)*$spacing}]
       set coords($numSprings) [OpenSeesSoilAnalysis $depth $spacing]
       set coords($numSprings,depth) $depth
       incr numSprings
    }

    # Do last spring if there is a remainder layer
    if {$r > 0.0} {
       set depth [expr {$endDepth - 0.5*$r}]
       set coords($numSprings) [OpenSeesSoilAnalysis $depth $r]
       set coords($numSprings,depth) $depth
       incr numSprings
    }


    set emax 0
    set smax 0
    for {set i 0} {$i < $numSprings} {incr i} {
       foreach {e s} $coords($i) {
          if {$e > $emax} {set emax $e}
          if {$s > $smax} {set smax $s}
       }
    }

    for {set k 0} {$k < $numSprings} {incr k} {
	set color blue
	set depth $coords($k,depth)
	if {$depth < $soilPropsTMP(waterTableDepth)} {
	    set color black
	}
	$graph data d$k -colour $color -points 0 -lines 1 -coords $coords($k)
    }

    axesEMUGraph $graph $emax $smax
    $graph redraw

    for {set k 0} {$k < $numSprings} {incr k} {
	set depth $coords($k,depth)

	$m2 add command -label "Spring at x = $depth ft" -command "ExportData [list $coords($k)] $w"

	set x [lindex $coords($k) end-1]
	set y [lindex $coords($k) end]
	$c create text [$graph x2canvas $x] [$graph y2canvas $y] -text "x = $depth ft" -anchor w -tag dd$k
	$c bind dd$k <Double-Button-1> "ExportData [list $coords($k)] $w"
    }
    
    set graphInfo(title) "Soil Layer $whichLayer ($soilPropsTMP(type,$whichLayer)), Multiplier = $soilPropsTMP(multiplier,$whichLayer)"
    set graphInfo(xlabel) "y (ft)"
    set graphInfo(ylabel) "p (kip/ft)"
    labelEMUGraph $c $graph graphInfo $width $height


    frame $w.output

    text $w.output.text -width 15
    grid $w.output.text -row 1 -column 0 -sticky nsew
    label $w.output.label -text "Numeric Output"
    grid $w.output.label -row 0 -column 0 -sticky nsew
    pack $w.output -side left

    $w.output.text insert end "Use the File->Export menu or double-click on a depth, e.g., x = 2.5 ft to export data"

    $w.output.text configure -state disabled
}

proc OpenSeesSoilAnalysis {depth {xTrib 1.0}} {

	wipe

	model basic -ndm 1 -ndf 1
	
	node 1 0.0
	node 2 1.0
	
	fix 2 1
	
	set maxDisp [OpenSeesSoilSpring 1 $depth $xTrib]

	element truss 1 1 2 1.0 1
	
	pattern Plain 1 Linear {
	    load 1 1.0
	}
	
	integrator DisplacementControl 1 1 [expr 0.01*$maxDisp]
	constraints Plain
	test NormUnbalance 1.0e-8 10 0
	algorithm Newton
	numberer Plain
	system UmfPack
       
	analysis Static
	
	set coords "0 0"
	
	set ok 0
	set j 0
	
	while {$ok >= 0 && [nodeDisp 1 1] < $maxDisp} {
	    
	    set ok [analyze 1]
	    
	    set strain [nodeDisp 1 1]
	    set stress [getTime]
	    lappend coords $strain $stress
	    
	    incr j
	}

      return $coords

}
