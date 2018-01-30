proc Assign_sectionProps {} {
    global sectionProps
    global sectionPropsTMP

    foreach var [array names sectionPropsTMP] {
       if {[info exists sectionProps($var)] && $sectionProps($var) != $sectionPropsTMP($var)} {
          ModelIsDirty
          SectionIsDirty
       }
       set sectionProps($var) $sectionPropsTMP($var)
    }

    if {$sectionProps(pileSameAsColumn) == 1} {
	foreach var "Dj tj ds cover Nbars dshoop fyhoop sphoop noConcrete" {
	    set sectionProps($var,2) $sectionProps($var,1)
	}
    }
}

proc AssignTMP_sectionProps {} {
    global sectionProps
    global sectionPropsTMP

    foreach var [array names sectionProps] {
	set sectionPropsTMP($var) $sectionProps($var)
    }
}

proc BarArea {barNum} {

   global units
   set in $units(in)

   set pi [expr acos(-1.0)]
   if {$barNum <= 8} {
      set diam [expr {0.125*$barNum*$in}]
      return [expr {0.25*$pi*$diam*$diam}]
   }
   if {$barNum == 9}  {return [expr {1.00*$in*$in}]}
   if {$barNum == 10} {return [expr {1.27*$in*$in}]}
   if {$barNum == 11} {return [expr {1.56*$in*$in}]}
   if {$barNum == 14} {return [expr {2.25*$in*$in}]}
   if {$barNum == 18} {return [expr {4.00*$in*$in}]}
}

proc BarDiam {barNum} {

   global units
   set in $units(in)

   if {$barNum <= 8} {
       return [expr {0.125*$barNum*$in}]
   }
   if {$barNum == 9}  {return [expr {1.128*$in}]}
   if {$barNum == 10} {return [expr {1.270*$in}]}
   if {$barNum == 11} {return [expr {1.410*$in}]}
   if {$barNum == 14} {return [expr {1.693*$in}]}
   if {$barNum == 18} {return [expr {2.257*$in}]}
}

# Column
set sectionProps(Dj,1) 36.0
set sectionProps(tj,1) 0.75
set sectionProps(ds,1) 11
set sectionProps(cover,1) 2.0
set sectionProps(Nbars,1) 16
set sectionProps(dshoop,1) 5
set sectionProps(fyhoop,1) 60.0
set sectionProps(sphoop,1) 3.0
set sectionProps(noConcrete,1) 0

set sectionProps(pileSameAsColumn) 1
if {$sectionProps(pileSameAsColumn) == 1} {
    foreach prop "Dj tj ds cover Nbars dshoop fyhoop sphoop noConcrete" {
	set sectionProps($prop,2) $sectionProps($prop,1)
    }
}
set sectionProps(changePile) 5.0
set sectionProps(MKductility) 25.0
set sectionProps(NMductility) 1.0
AssignTMP_sectionProps

proc CISSSection {} {
   return 1
}
proc GapSection {} {
   return 2
}
proc PileSection {} {
   return 3
}

set sectionWindowOpen 0
proc DefineSection {w} {
    global sectionWindowOpen
    global whichSection

    # If already open, do not attempt to open a duplicate
    if {$sectionWindowOpen == 1} {raise $w; return}

    # Create new window
    if {$whichSection == 2} {
	createTopLevel $w "Pile Section"
    } else {
	createTopLevel $w "Column Section"
    }

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left
    set m [menu $w.mbar.file.menu]

    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"

    # Create "Materials" pull down menu
    menubutton $w.mbar.materials -text Materials -menu $w.mbar.materials.menu
    pack $w.mbar.materials -side left
    set m [menu $w.mbar.materials.menu]

    $m add command -label "Concrete" -command "DefineConcrete .concrete"
    $m add command -label "Reinforcing Steel" -command "DefineRebarSteel .rebarSteel"
    $m add command -label "Steel Shell" -command "DefineJacketSteel .jacketSteel"

    # Create "Analysis" pull down menu
    menubutton $w.mbar.analysis -text Analysis -menu $w.mbar.analysis.menu
    pack $w.mbar.analysis -side left
    set m [menu $w.mbar.analysis.menu]

    set withShell [CISSSection]
    set withoutShell [GapSection]
    set pile [PileSection]

    if {$whichSection == 1} {
	$m add cascade -label "Moment-Curvature" -menu $m.mkmenu
	set m2 [menu $m.mkmenu]
	$m2 add command -label "Gap Section" -command "DefineMomentCurvature .moment-curvature $withoutShell"
	$m2 add command -label "CISS Section" -command "DefineMomentCurvature .moment-curvature $withShell"
    } else {
	$m add command -label "Moment-Curvature" -command "DefineMomentCurvature .moment-curvature $pile"
    }
    if {$whichSection == 1} {
	$m add cascade -label "N-M Interaction" -menu $m.nmmenu
	set m2 [menu $m.nmmenu]
	$m2 add command -label "Gap Section" -command "DefineNMAnalysis .nm $withoutShell"
	$m2 add command -label "CISS Section" -command "DefineNMAnalysis .nm $withShell"
    } else {
	$m add command -label "N-M Interaction" -command "DefineNMAnalysis .nm $pile"
    }

    set text(0)  "The input values are the parameters required to simulate the respone of a CISS section by fiber discretization.  Updates to these parameters will cause the section to be re-drawn automatically with the current input values."
    set text(1) "Double-clicking section items, or selecting from the Materials menu, will bring up an edit menu for the associated material properties."
    set text(2) "Two section analysis options (moment-curvature and axial-moment interaction) are available from the Analysis menu."

    createHelp $w $w.mbar section text

    set sectionWindowOpen 1
    bind $w <Destroy> {set sectionWindowOpen 0}

    AssignTMP_sectionProps

    frame $w.input

    set myFrame [frame $w.input.shell]
    label $myFrame.title -text "Steel Shell" -font {helvetica 8 bold}
    grid $myFrame.title -row 0 -column 0 -columnspan 3
    set row 1
    foreach {lbl ent unt} {"Diameter" Dj in "Thickness" tj in} {
	label $myFrame.label$row -text $lbl
	grid $myFrame.label$row -row $row -column 0 -sticky e
	entry $myFrame.entry$row -width 5 -textvariable sectionPropsTMP($ent,$whichSection)
	bind $myFrame.entry$row <KeyRelease> "DrawSection $w.show.canvas"
	bind $myFrame.entry$row <Return> "DefineSection_OK $w"
	grid $myFrame.entry$row -row $row -column 1
	label $myFrame.unit$row -text $unt
	grid $myFrame.unit$row -row $row -column 2 -sticky w
	incr row
    }

    set myFrame [frame $w.input.longrf]
    label $myFrame.title -text "Longitudinal R/F" -font {helvetica 8 bold}
    grid $myFrame.title -row 0 -column 0 -columnspan 3
    set row 1
    foreach {lbl ent unt} {"Clear Cover" cover in "Number of Bars" Nbars ""} {
	label $myFrame.label$row -text $lbl
	grid $myFrame.label$row -row $row -column 0 -sticky e
	entry $myFrame.entry$row -width 5 -textvariable sectionPropsTMP($ent,$whichSection)
	bind $myFrame.entry$row <KeyRelease> "DrawSection $w.show.canvas"
	bind $myFrame.entry$row <Return> "DefineSection_OK $w"
	grid $myFrame.entry$row -row $row -column 1
	label $myFrame.unit$row -text $unt
	grid $myFrame.unit$row -row $row -column 2 -sticky w
	incr row
    }
    label $myFrame.labelbsm -text "Bar Size"
    grid $myFrame.labelbsm -row $row -column 0 -sticky e
    set menu [tk_optionMenu $myFrame.barsizemenua sectionPropsTMP(ds,$whichSection) junk]
    $menu delete 0
    set i 0
    foreach size {3 4 5 6 7 8 9 10 11 14 18} {
	$menu insert $i radiobutton -label $size -variable sectionPropsTMP(ds,$whichSection) -command "DrawSection $w.show.canvas"
	incr i
    }
    grid $myFrame.barsizemenua -row $row -column 1 -columnspan 2

    set myFrame [frame $w.input.hooprf]
    label $myFrame.title -text "Transverse R/F" -font {helvetica 8 bold}
    grid $myFrame.title -row 0 -column 0 -columnspan 3
    set row 1
    foreach {lbl ent unt} {"Spacing" sphoop in "Yield Stress" fyhoop ksi} {
	label $myFrame.label$row -text $lbl
	grid $myFrame.label$row -row $row -column 0 -sticky e
	entry $myFrame.entry$row -width 5 -textvariable sectionPropsTMP($ent,$whichSection)
	bind $myFrame.entry$row <KeyRelease> "DrawSection $w.show.canvas"
	bind $myFrame.entry$row <Return> "DefineSection_OK $w"
	grid $myFrame.entry$row -row $row -column 1
	label $myFrame.unit$row -text $unt
	grid $myFrame.unit$row -row $row -column 2 -sticky w
	incr row
    }
    label $myFrame.labelbsm -text "Bar Size"
    grid $myFrame.labelbsm -row $row -column 0 -sticky e
    set menu [tk_optionMenu $myFrame.barsizemenua sectionPropsTMP(dshoop,$whichSection) junk]
    $menu delete 0
    set i 0
    foreach size {3 4 5 6 7 8 9 10 11 14 18} {
	$menu insert $i radiobutton -label $size -variable sectionPropsTMP(dshoop,$whichSection) -command "DrawSection $w.show.canvas"
	incr i
    }
    grid $myFrame.barsizemenua -row $row -column 1 -columnspan 2

    pack $w.input.shell $w.input.longrf $w.input.hooprf -side left -ipadx 5 -ipady 5 -anchor n

    pack $w.input -side top

    
    set myFrame [frame $w.noconcrete]

    incr row
    checkbutton $myFrame.pileConc -text "No concrete in section" -variable sectionPropsTMP(noConcrete,$whichSection) -command "EnableRCSectionEntries $w $whichSection; DrawSection $w.show.canvas"
    grid $myFrame.pileConc -row $row -column 0 -sticky ew -columnspan 3
	
    pack $w.noconcrete -side top

    EnableRCSectionEntries $w $whichSection
    
    if {$whichSection == 2} {
	incr row
	set myFrame [frame $w.pile]

	label $myFrame.ltrans -text "Change at depth"
	grid $myFrame.ltrans -row $row -column 0 -sticky e
	entry $myFrame.etrans -width 5 -textvariable sectionPropsTMP(changePile)
	bind $myFrame.etrans <Return> "DefineSection_OK $w"
	grid $myFrame.etrans -row $row -column 1
	label $myFrame.utrans -text "ft"
	grid $myFrame.utrans -row $row -column 2 -sticky w

	incr row

	checkbutton $myFrame.pile -text "Make pile section same as column" -variable sectionPropsTMP(pileSameAsColumn) -command "EnablePileSectionEntries $w; DrawSection $w.show.canvas"
	grid $myFrame.pile -row $row -column 0 -sticky ew -columnspan 3

	pack $w.pile -side top

	EnablePileSectionEntries $w
    }

    frame $w.okcancel
    
    button $w.okcancel.ok -text "OK" -command "DefineSection_OK $w"
    grid $w.okcancel.ok -row 0 -column 0
    button $w.okcancel.cancel -text "Cancel" -command "DefineSection_Cancel $w"
    grid $w.okcancel.cancel -row 0 -column 1

    pack $w.okcancel -side top    

    frame $w.show
    canvas $w.show.canvas -height 400 -width 400
    grid $w.show.canvas -row 0 -column 2 -rowspan $row
    pack $w.show -side top

    DrawSection $w.show.canvas
}

proc EnableRCSectionEntries {w whichSection} {
    global sectionPropsTMP

    if {$sectionPropsTMP(noConcrete,$whichSection) == 0} {
	set state normal
    } else {
	set state disabled
    }

    $w.input.longrf.entry1 config -state $state
    $w.input.longrf.entry2 config -state $state
    $w.input.longrf.barsizemenua config -state $state

    $w.input.hooprf.entry1 config -state $state
    $w.input.hooprf.entry2 config -state $state
    $w.input.hooprf.barsizemenua config -state $state    
}

proc EnablePileSectionEntries {w} {
    global sectionPropsTMP

    if {$sectionPropsTMP(pileSameAsColumn) == 1} {
	set state disabled
    } else {
	set state normal
    }

    $w.input.shell.entry1 config -state $state
    $w.input.shell.entry2 config -state $state

    $w.input.longrf.entry1 config -state $state
    $w.input.longrf.entry2 config -state $state
    $w.input.longrf.barsizemenua config -state $state

    $w.input.hooprf.entry1 config -state $state
    $w.input.hooprf.entry2 config -state $state
    $w.input.hooprf.barsizemenua config -state $state

    $w.noconcrete.pileConc config -state $state
    $w.pile.etrans config -state $state
}

proc DefineSection_Cancel {w} {
    AssignTMP_sectionProps

    destroy $w
}

proc DefineSection_OK {w} {

    set ok [CheckSection]
    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input for $ok" -icon error -parent $w
	return
    }

    global sectionProps
    global sectionPropsTMP

    if {$sectionPropsTMP(pileSameAsColumn) == 1 && $sectionProps(pileSameAsColumn) == 0} {
	foreach tmp "Dj tj ds cover Nbars dshoop fyhoop sphoop noConcrete" {
	    set sectionPropsTMP($tmp,2) $sectionPropsTMP($tmp,1)
	}
    }

    Assign_sectionProps

    global hingeProps
    global hingePropsTMP
    Computelp
    set hingeProps(lp) $hingePropsTMP(lp)

    DefineModel [getCanvas]

    destroy $w
}

proc CheckSection {} {
    global sectionPropsTMP
    global whichSection

    set Dj $sectionPropsTMP(Dj,$whichSection)
    if {![isValidDouble $Dj] || $Dj <= 0.0} {
	return "shell diameter"
    }

    set tj $sectionPropsTMP(tj,$whichSection)
    if {![isValidDouble $tj] || $tj < 0.0} {
	return "shell thickness"
    }

    if {$tj <= 0.0 && $sectionPropsTMP(noConcrete,$whichSection) == 1} {
	return "no concrete and no steel shell"
    }

    set cover $sectionPropsTMP(cover,$whichSection)
    if {![isValidDouble $cover] || $cover < 0.0} {
	return cover
    }

    set Nbars $sectionPropsTMP(Nbars,$whichSection)
    if {![isValidInt $Nbars] || $Nbars < 0} {
	return Nbars
    }

    set ds $sectionPropsTMP(ds,$whichSection)
    # Not really needed since it's a menu choice
    if {![isValidInt $ds]} {
	return ds
    }

    set fyhoop $sectionPropsTMP(fyhoop,$whichSection)
    if {![isValidDouble $fyhoop] || $fyhoop <= 0.0} {
	return "hoop yield strength"
    }

    set tj $sectionPropsTMP(sphoop,$whichSection)
    if {![isValidDouble $tj] || $tj <= 0.0} {
	return "hoop spacing"
    }

    set dshoop $sectionPropsTMP(dshoop,$whichSection)
    # Not really needed since it's a menu choice
    if {![isValidInt $dshoop]} {
	return dshoop
    }

    global columnPropsTMP
    set L $columnPropsTMP(L)
    set changePile $sectionPropsTMP(changePile)
    if {![isValidDouble $changePile] || $changePile < -$L} {
	return "depth of column/pile section interface"
    }
}

# A proc to draw the CISS section
# input: c -- canvas to which objects will be drawn
proc DrawSection {c {color on}} {
    global sectionPropsTMP
    global whichSection
    global colors

    set sectionToDraw $whichSection
    if {$sectionPropsTMP(pileSameAsColumn) == 1} {
	set sectionToDraw 1
    }

    set width [winfo reqwidth $c]
    set height [winfo reqheight $c]
    set dim [expr {0.95*(0.5*$width)}]

    set Dj $sectionPropsTMP(Dj,$sectionToDraw)
    if {![isValidDouble $Dj] || $Dj == 0.0} {return}
    set tj $sectionPropsTMP(tj,$sectionToDraw)
    if {![isValidDouble $tj]} {return}
    set cover $sectionPropsTMP(cover,$sectionToDraw)
    if {![isValidDouble $cover]} {return}

    set Nbars $sectionPropsTMP(Nbars,$sectionToDraw)
    if {![isValidInt $Nbars] || $Nbars < 0} {return}
    set ds $sectionPropsTMP(ds,$sectionToDraw)
    if {![isValidInt $ds]} {return}
    set dshoop $sectionPropsTMP(dshoop,$sectionToDraw)
    if {![isValidInt $dshoop]} {return}

    global units
    set in $units(in)

    clearCanvas $c

    set concreteColor $colors(concrete)
    set steelColor $colors(steel)
    if {$color != "on"} {
        set concreteColor white
        set steelColor white
    }
    set drawRC 1
    #if {$whichSection != 1 && $sectionPropsTMP(pileNoConcrete) == 1 && $sectionPropsTMP(pileSameAsColumn) == 0} 
    if {$sectionPropsTMP(noConcrete,$whichSection) == 1} {
	set drawRC 0
	set concreteColor white
    }

    $c create oval -$dim -$dim $dim $dim -fill $steelColor -tag jacket 
    set dim1 [expr {-$dim+(2*$dim)*$tj/$Dj}]
    set dim2 [expr -$dim1]
    $c create oval $dim1 $dim1 $dim2 $dim2 -fill $concreteColor -tag concrete

    # Draw transverse r/f
    if {$drawRC} {
	set dim3 [expr {-$dim + (2*$dim)/$Dj*($tj+$cover)}]
	$c create oval $dim3 $dim3 [expr -$dim3] [expr -$dim3]

	set dim3 [expr {-$dim + (2*$dim)/$Dj*($tj+$cover+[BarDiam $dshoop]/$in)}]
	$c create oval $dim3 $dim3 [expr -$dim3] [expr -$dim3]
    }
    
    set pi [expr {acos(-1.0)}]
    set dtheta [expr {2*$pi/$Nbars}]
    set radius [expr {$dim-(2*$dim)*($tj+$cover+[BarDiam $dshoop]/$in+0.5*[BarDiam $ds]/$in)/$Dj}]    
    set cor [expr {0.5*[BarDiam $ds]/$in*(2*$dim)/$Dj}]

    # Draw longitudinal rebars
    for {set i 0} {$drawRC && $i < $Nbars} {incr i} {
        set theta [expr {$i*$dtheta}]
        set dx [expr {$radius*cos($theta)}]
        set dy [expr {$radius*sin($theta)}]
        $c create oval [expr {$cor+$dx}] [expr {$cor+$dy}] [expr {-$cor+$dx}] [expr {-$cor+$dy}] -fill $steelColor -tag rebar
    }

    $c bind jacket <Double-Button-1> {DefineJacketSteel .jacketSteel}
    $c bind rebar <Double-Button-1> {DefineRebarSteel .rebarSteel}
    $c bind concrete <Double-Button-1> {DefineConcrete .concrete}

    $c move all [expr {0.5*$width}] [expr {0.5*$height}]
}

proc DefineOpenSeesSection {ciss gap pile} {
    global units
    set in $units(in)
    set ksi $units(ksi)

    global sectionPropsTMP

    set asj 1.0

    set tagUnconf 1
    set tagCISS 2
    set tagHoop 3
    set tagPile 4
    OpenSeesConcrete $tagUnconf $tagCISS $tagHoop $tagPile

    
    set tagRebar 5
    OpenSeesRebarSteel $tagRebar

    set tagJacket 6
    OpenSeesJacketSteel $tagJacket
    
    # Define cross-section for nonlinear columns
    # ------------------------------------------
    
    set Dj $sectionPropsTMP(Dj,1)
    set Dj [expr {$Dj*$in}]
    set tj $sectionPropsTMP(tj,1)
    set tj [expr {$tj*$in}]
    set cover $sectionPropsTMP(cover,1)
    set cover [expr {$cover*$in}]

    set ds $sectionPropsTMP(ds,1)
    set As [BarArea $ds]
    set Dc [expr {$Dj-2*$tj}] ;# Inside diameter of steel shell

    set Nbars $sectionPropsTMP(Nbars,1)
    set halfBar [expr 0.5*[BarDiam $ds]]

    set discretize2D 1
    #set discretize2D 0
    set Nl 20

	set coverTag $tagCISS
	# If there is no shell, the cover concrete is unconfined
    if {$tj <= 0.0} {
       set coverTag $tagUnconf
    }

    section Fiber $ciss {

	if {$sectionPropsTMP(noConcrete,1) == 0} {
	    # Core concrete
	    if {$discretize2D == 0} {
		patch circ $tagCISS 20 10 0.0 0.0 0.0 [expr {0.5*$Dc-$cover}] 0.0 360.0
	    } else {
		set wl [expr {($Dc-2*$cover)/$Nl}]
		set radius [expr {0.5*$Dc-$cover}]
		for {set i 1} {$i <= $Nl/2} {incr i} {
		    set yi [expr {($i-0.5)*$wl}]
		    set zi [expr sqrt($radius*$radius - $yi*$yi)]
		    set Ai [expr {$wl*2*$zi}]
		    fiber $yi 0 $Ai $tagCISS
		    fiber [expr -$yi] 0 $Ai $tagCISS
		}
	    }
		
	    # Cover concrete
	    patch circ $coverTag 20 2  0.0 0.0 [expr {0.5*$Dc-$cover}] [expr {0.5*$Dc}] 0.0 360.0

	    # Longitudinal steel
	    layer circ $tagRebar $Nbars $As 0.0 0.0 [expr {0.5*$Dc-$cover-$halfBar}]
	}
	
	# Steel shell
	if {$tj > 0.0} {
	    set ri [expr {($Dc-$Dj)*0.25*$asj + ($Dc+$Dj)*0.25}]
	    set re [expr {($Dj-$Dc)*0.25*$asj + ($Dc+$Dj)*0.25}]
	    patch circ $tagJacket 20 1 0.0 0.0 $ri $re 0.0 360.0  
	}
    }    
    
    global hingeProps
    set coreTag $tagCISS
    if {$hingeProps(coreConfined) == 0} {
       set coreTag $tagHoop
    }
    set coverTag $coreTag
    if {$hingeProps(coverConfined) == 0} {
       set coverTag $tagUnconf
    }

    section Fiber $gap {

	if {$sectionPropsTMP(noConcrete,1) == 0} {
	    # Core concrete
	    if {$discretize2D == 0} {
		patch circ $coreTag 20 10 0.0 0.0 0.0 [expr {0.5*$Dc-$cover}] 0.0 360.0
	    } else {
		set wl [expr {($Dc-2*$cover)/$Nl}]
		set radius [expr {0.5*$Dc-$cover}]
		for {set i 1} {$i <= $Nl/2} {incr i} {
		    set yi [expr {($i-0.5)*$wl}]
		    set zi [expr sqrt($radius*$radius - $yi*$yi)]
		    set Ai [expr {$wl*2*$zi}]
		    fiber  $yi 0 $Ai $coreTag
		    fiber [expr -$yi] 0 $Ai $coreTag
		}
	    }
	    
	    # Cover concrete
	    patch circ $coverTag 20 2  0.0 0.0 [expr {0.5*$Dc-$cover}] [expr {0.5*$Dc}] 0.0 360.0
	    
	    # Longitudinal steel
	    layer circ $tagRebar $Nbars $As 0.0 0.0 [expr {0.5*$Dc-$cover-$halfBar}]
	} else {
	    # Make the gap section an empty steel shell if there is no concrete;
	    # otherwise, big numerical problems will occur!
	    set ri [expr {($Dc-$Dj)*0.25*$asj + ($Dc+$Dj)*0.25}]
	    set re [expr {($Dj-$Dc)*0.25*$asj + ($Dc+$Dj)*0.25}]
	    patch circ $tagJacket 20 1 0.0 0.0 $ri $re 0.0 360.0  	    
	}
    }


    # Pile section
    set Dj $sectionPropsTMP(Dj,2)
    set Dj [expr {$Dj*$in}]
    set tj $sectionPropsTMP(tj,2)
    set tj [expr {$tj*$in}]
    set cover $sectionPropsTMP(cover,2)
    set cover [expr {$cover*$in}]

    set ds $sectionPropsTMP(ds,2)
    set As [BarArea $ds]
    set Dc [expr {$Dj-2*$tj}] ;# Inside diameter of steel shell

    set Nbars $sectionPropsTMP(Nbars,2)
    set halfBar [expr 0.5*[BarDiam $ds]]

	set coverTag $tagPile
	# If there is no shell, the cover concrete is unconfined
    if {$tj <= 0.0} {
       set coverTag $tagUnconf
    }
	
    section Fiber $pile {
	
	if {$sectionPropsTMP(noConcrete,2) == 0} {
	    # Core concrete
	    if {$discretize2D == 0} {
		patch circ $tagPile 20 10 0.0 0.0 0.0 [expr {0.5*$Dc-$cover}] 0.0 360.0
	    } else {
		set wl [expr {($Dc-2*$cover)/$Nl}]
		set radius [expr {0.5*$Dc-$cover}]
		for {set i 1} {$i <= $Nl/2} {incr i} {
		    set yi [expr {($i-0.5)*$wl}]
		    set zi [expr sqrt($radius*$radius - $yi*$yi)]
		    set Ai [expr {$wl*2*$zi}]
		    fiber  $yi 0 $Ai $tagPile
		    fiber [expr -$yi] 0 $Ai $tagPile
		}
	    }
		
	    # Cover concrete
	    patch circ $coverTag 20 2  0.0 0.0 [expr {0.5*$Dc-$cover}] [expr {0.5*$Dc}] 0.0 360.0
		
	    # Longitudinal steel
	    layer circ $tagRebar $Nbars $As 0.0 0.0 [expr {0.5*$Dc-$cover-$halfBar}]
	}

	# Steel shell
	if {$tj > 0.0} {
	    set ri [expr {($Dc-$Dj)*0.25*$asj + ($Dc+$Dj)*0.25}]
	    set re [expr {($Dj-$Dc)*0.25*$asj + ($Dc+$Dj)*0.25}]
	    patch circ $tagJacket 20 1 0.0 0.0 $ri $re 0.0 360.0  
	}
    }
}

set nmWindowOpen 0
proc DefineNMAnalysis {w shell} {
    global nmWindowOpen

    if {$nmWindowOpen == 1} {raise $w; return}

    createTopLevel $w "Axial-Moment Interaction"

    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left
    set m [menu $w.mbar.file.menu]

    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"

    set text(0) "The input value is the curvature ductility at which the moment is reported for varying levels of axial load."

    createHelp $w $w.mbar nmInteraction text


    set nmWindowOpen 1
    bind $w <Destroy> {set nmWindowOpen 0}
    


    frame $w.input

    label $w.input.labelduct -text "Target Ductility"
    grid $w.input.labelduct -row 1 -column 0
    entry $w.input.duct -width 5 -textvariable sectionPropsTMP(NMductility)
    grid $w.input.duct -row 1 -column 1

    button $w.input.analyze -text "Analyze" -command "NMAnalysis .nmAnalysis $shell"
    grid $w.input.analyze -row 2 -column 0 -columnspan 2 -sticky ew

    button $w.input.close -text "Close" -command "destroy $w"
    grid $w.input.close -row 3 -column 0 -columnspan 2 -sticky ew

    pack $w.input -side top

}

set OpenSeesNMResults(withShell) ""
set OpenSeesNMResults(withoutShell) ""
set OpenSeesNMResults(pile) ""
proc OpenSeesNMAnalysis {shell} {
    set withShell [CISSSection]
    set withoutShell [GapSection]
    set pile [PileSection]

    global OpenSeesNMResults
    if {![IsSectionDirty] && $shell == $withShell && [llength $OpenSeesNMResults(withShell)] > 0} {
	return $OpenSeesNMResults(withShell)
    }
    if {![IsSectionDirty] && $shell == $withoutShell && [llength $OpenSeesNMResults(withoutShell)] > 0} {
	return $OpenSeesNMResults(withoutShell)
    }
    if {![IsSectionDirty] && $shell == $pile && [llength $OpenSeesNMResults(pile)] > 0} {
	return $OpenSeesNMResults(pile)
    }


    global units
    set in $units(in)
    set ft $units(ft)
    set ksi $units(ksi)

    global sectionPropsTMP
    global whichSection
    set Dj [expr {$sectionPropsTMP(Dj,$whichSection)*$in}]
    set tj [expr {$sectionPropsTMP(tj,$whichSection)*$in}]
    set cover [expr {$sectionPropsTMP(cover,$whichSection)*$in}]
    set ductility $sectionPropsTMP(NMductility)

    global concretePropsTMP
    set fc [expr {$concretePropsTMP(fc)*$ksi}]

    global rfsteelPropsTMP
    set E [expr {$rfsteelPropsTMP(E)*$ksi}]
    set fye [expr {$rfsteelPropsTMP(fye)*$ksi}]
    set epsye [expr {$fye/$E}]

    set Dc [expr {$Dj-2*$tj}] ;# Inside diameter of steel shell
    set Pmax [AxialLoadCapacity]

    set coords ""


    set nu 2.0

    # Estimate yield curvature
    set Ky [expr {$epsye/(0.5*$Dc)}]

    global stopReport

    while {$nu > -1.0} {
	wipe

	update
	if {$stopReport == 1} {
	    break
	}
	
	model basic -ndm 2 -ndf 3
	
	DefineOpenSeesSection $withShell $withoutShell $pile
	
	set numIncr 100;	# Number of analysis increments
	
	set axialLoad [expr {$nu*$Pmax}]

	MomentCurvature $shell $axialLoad [expr {$ductility*$Ky}] $numIncr

	set ok 0
	set j 0
	while {$ok >= 0 && $j < $numIncr} {

	    set ok [analyze 1]
	    if {$ok < 0} {
		break
	    }
	    
	    incr j
	}
	
	set Mmax [getTime]
	
	if {$ok >= 0 && $Mmax > 0.0} {
	    lappend coords [expr {$Mmax/$ft}] $axialLoad
	}
	
	# Kill the analysis if not converged and in tension
	if {$ok < 0 && $nu < 0.0} {
	    break
	}
	
	set nu [expr {$nu - 0.05}]
    }

    if {$shell == $withShell} {
	set OpenSeesNMResults(withShell) $coords
    } 
    if {$shell == $withoutShell} {
	set OpenSeesNMResults(withoutShell) $coords
    }
    if {$shell == $pile} {
	set OpenSeesNMResults(pile) $coords
    }
    
    return $coords
}

set nmProgressOpen 0
set nmAnalysisWindowOpen 0
proc NMAnalysis {w shell} {

    global nmProgressOpen
    if {$nmProgressOpen == 1} {raise $w; return}

    global nmAnalysisWindowOpen
    if {$nmAnalysisWindowOpen == 1} {raise $w; return}

    createTopLevel $w "Analysis Progress"

    set nmProgressOpen 1
    bind $w <Destroy> {set nmProgressOpen 0; set nmStopAnalysis 1}

    label $w.label -text "Moment-curvature analysis at"
    grid $w.label -row 0 -column 0 -sticky ew
    label $w.progress
    grid $w.progress -row 1 -column 0 -sticky ew

    global nmStopAnalysis
    set nmStopAnalysis 0

    button $w.cancel -text "Stop" -command "set nmStopAnalysis 1"
    grid $w.cancel -row 2 -column 0 -sticky ew


    global units
    set in $units(in)
    set ft $units(ft)
    set ksi $units(ksi)

    global sectionPropsTMP
    global whichSection
    set Dj [expr {$sectionPropsTMP(Dj,$whichSection)*$in}]
    set tj [expr {$sectionPropsTMP(tj,$whichSection)*$in}]
    set cover [expr {$sectionPropsTMP(cover,$whichSection)*$in}]
    set ductility $sectionPropsTMP(NMductility)

    global concretePropsTMP
    set fc [expr {$concretePropsTMP(fc)*$ksi}]

    global rfsteelPropsTMP
    set E [expr {$rfsteelPropsTMP(E)*$ksi}]
    set fye [expr {$rfsteelPropsTMP(fye)*$ksi}]
    set epsye [expr {$fye/$E}]

    set Dc [expr {$Dj-2*$tj}] ;# Inside diameter of steel shell
    set Pmax [AxialLoadCapacity]

    set coords ""

    set withShell [CISSSection]
    set withoutShell [GapSection]
    set pile [PileSection]

    set nuMax 2.0
    set nu $nuMax
    set nuMin -1.0

    # Estimate yield curvature
    set Ky [expr {$epsye/(0.5*$Dc)}]

    set Mmax 0

    while {$nu > $nuMin && $nmStopAnalysis == 0} {
	wipe

	model basic -ndm 2 -ndf 3
	
	DefineOpenSeesSection $withShell $withoutShell $pile
	
	set numIncr 100;	# Number of analysis increments
	
	set axialLoad [expr {$nu*$Pmax}]

	$w.progress config -text "N = [format %.1f $axialLoad] kip"

	update

	MomentCurvature $shell $axialLoad [expr {$ductility*$Ky}] $numIncr

	set ok 0
	set j 0
	while {$ok >= 0 && $j < $numIncr} {

	    set ok [analyze 1]
          if {$ok < 0} {
             break
          }

	    incr j
	}

	set Mx [getTime]

      if {$ok >= 0 && $Mx > 0.0} {
         lappend coords [expr {$Mx/$ft}] $axialLoad
      }

      # Kill the analysis if not converged and in tension
      if {$ok < 0 && $nu < 0.0} {
         break
      }

      if {$Mx > $Mmax} {
         set Mmax $Mx
      }

	set nu [expr {$nu - 0.05}]
    }

    global OpenSeesNMResults
    if {$shell == $withShell} {
	 set OpenSeesNMResults(withShell) $coords
    } 
    if {$shell == $withoutShell} {
	 set OpenSeesNMResults(withoutShell) $coords
    }
    if {$shell == $pile} {
	 set OpenSeesNMResults(pile) $coords
    }

    destroy $w

    createTopLevel $w "N-M Interaction"

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left
    set m [menu $w.mbar.file.menu]

    set height 600
    set width 600
    set graph foo
    set c [createEMUGraph $w $graph $width $height]

    $m add command -label "Print (Ctrl+P)" -command "PrintCanvas $c $w"
    bind $w <Control-Key-p> "PrintCanvas $c $w"
    $m add command -label "Export (Ctrl+X)" -command "ExportData [list $coords] $w"
    bind $w <Control-Key-x> "ExportData [list $coords] $w"
    $m add separator
    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"


    set nmAnalysisWindowOpen 1
    bind $w <Destroy> {set nmAnalysisWindowOpen 0}

    $graph data d2 -colour blue -points 0 -lines 1 -coords $coords

    set Nmin [expr {$nuMin*$Pmax}]
    set Nmax [expr {$nuMax*$Pmax}]
    axesEMUGraph $graph $Mmax $Nmax 0 $Nmin
    $graph redraw

    $graph hmark 0.0 tagZero black

    if {$shell == $withShell} {
	set graphInfo(title) "Axial-Moment Interaction (CISS Section)"
    }
    if {$shell == $withoutShell} {
	set graphInfo(title) "Axial-Moment Interaction (Gap Section)"
    }
    if {$shell == $pile} {
	set graphInfo(title) "Axial-Moment Interaction (Pile Section)"
    }
    set graphInfo(xlabel) "M (kip-ft)"
    set graphInfo(ylabel) "N (kip)"
    labelEMUGraph $c $graph graphInfo $width $height

    set width [string length "[format %+.3e 0],[format %+.3e 0]"]

    frame $w.output
    text $w.output.text -width $width
    grid $w.output.text -row 1 -column 0 -sticky nsew
    label $w.output.label -text "Numeric Output"
    grid $w.output.label -row 0 -column 0 -sticky nsew
    pack $w.output -side left

    foreach {x y} $coords {
	$w.output.text insert end "[format %+.3e $x],[format %+.3e $y]\n"
    }

    $w.output.text configure -state disabled
}

# A proc to do moment-curvature analysis of the CISS section
# input: w -- new popup window name
set momentCurvatureWindowOpen 0
proc DefineMomentCurvature {w shell} {
    global momentCurvatureWindowOpen

    if {$momentCurvatureWindowOpen == 1} {raise $w; return}

    createTopLevel $w "Moment-Curvature Analysis"

    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left
    set m [menu $w.mbar.file.menu]

    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"

    set text(0) "The inputs define the parameters for a moment-curvature analysis with fixed axial load and target curvature ductility."

    createHelp $w $w.mbar momentCurvature text


    set momentCurvatureWindowOpen 1
    bind $w <Destroy> {set momentCurvatureWindowOpen 0}

    global sectionPropsTMP
    global columnPropsTMP
    global whichColumn
    set sectionPropsTMP(axialLoad) $columnPropsTMP(axialLoad,$whichColumn)

    frame $w.input

    label $w.input.labelLoad -text "Axial Load"
    grid $w.input.labelLoad -row 0 -column 0
    entry $w.input.axialLoad -width 5 -textvariable sectionPropsTMP(axialLoad)
    grid $w.input.axialLoad -row 0 -column 1

    label $w.input.labelduct -text "Target Ductility"
    grid $w.input.labelduct -row 1 -column 0
    entry $w.input.duct -width 5 -textvariable sectionPropsTMP(MKductility)
    grid $w.input.duct -row 1 -column 1

    button $w.input.analyze -text "Analyze" -command "MomentCurvatureAnalysis .momentCurvAnalysis $shell -123456789"
    grid $w.input.analyze -row 2 -column 0 -columnspan 2 -sticky ew

    button $w.input.close -text "Close" -command "destroy $w"
    grid $w.input.close -row 3 -column 0 -columnspan 2 -sticky ew

    pack $w.input -side top
}

set momentCurvatureAnalysisWindowOpen 0
proc MomentCurvatureAnalysis {w shell axialLoad} {

    global momentCurvatureAnalysisWindowOpen
    if {$momentCurvatureAnalysisWindowOpen == 1} {raise $w; return}

    global sectionPropsTMP
    if {$axialLoad == -123456789} {
       set axialLoad $sectionPropsTMP(axialLoad)
    }

    set coords [OpenSeesMKAnalysis $shell $axialLoad]

    createTopLevel $w "Moment-Curvature Analysis"

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
    $m add command -label "Export (Ctrl+X)" -command "ExportData [list $coords] $w"
    bind $w <Control-Key-x> "ExportData [list $coords] $w"
    $m add separator
    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"


    set momentCurvatureAnalysisWindowOpen 1
    bind $w <Destroy> {set momentCurvatureAnalysisWindowOpen 0}

    # Find max moment and curvature
    set emax 0
    set smax 0
    foreach {e s} $coords {
       if {$e > $emax} {set emax $e}
       if {$s > $smax} {set smax $s}
    }

    $graph data d2 -colour blue -points 0 -lines 1 -coords $coords

    axesEMUGraph $graph $emax $smax
    $graph redraw

    set withShell [CISSSection]
    set withoutShell [GapSection]
    set pile [PileSection]

    set axialLoad [format %.1f $axialLoad]
    if {$shell == $withShell} {
       set graphInfo(title) "Moment-Curvature (CISS Section), N = $axialLoad kips"
    }
    if {$shell == $withoutShell} {
       set graphInfo(title) "Moment-Curvature (Gap Section), N = $axialLoad kips"
    }
    if {$shell == $pile} {
       set graphInfo(title) "Moment-Curvature (Pile Section), N = $axialLoad kips"
    }
    set graphInfo(xlabel) "Curvature (1/ft)"
    set graphInfo(ylabel) "Moment (kip-ft)"
    labelEMUGraph $c $graph graphInfo $width $height

    set width [string length "[format %+.3e 0],[format %+.3e 0]"]

    frame $w.output
    text $w.output.text -width $width
    grid $w.output.text -row 1 -column 0 -sticky nsew
    label $w.output.label -text "Numeric Output"
    grid $w.output.label -row 0 -column 0 -sticky nsew
    pack $w.output -side left

    foreach {x y} $coords {
	$w.output.text insert end "[format %+.3e $x],[format %+.3e $y]\n"
    }

    $w.output.text configure -state disabled
}

proc OpenSeesMKAnalysis {shell axialLoad} {

    global units
    set in $units(in)
    set ft $units(ft)
    set ksi $units(ksi)

    global rfsteelPropsTMP
    set E [expr {$rfsteelPropsTMP(E)*$ksi}]
    set fye [expr {$rfsteelPropsTMP(fye)*$ksi}]
    set epsye [expr {$fye/$E}]

    global sectionPropsTMP
    global whichSection
    set Dj [expr {$sectionPropsTMP(Dj,$whichSection)*$in}]
    set tj [expr {$sectionPropsTMP(tj,$whichSection)*$in}]
    set ductility $sectionPropsTMP(MKductility)

    global concretePropsTMP
    set fc [expr {$concretePropsTMP(fc)*$ksi}]

    set Dc [expr {$Dj-2*$tj}] ;# Inside diameter of steel shell

    wipe

    model basic -ndm 2 -ndf 3

    set withShell [CISSSection]
    set withoutShell [GapSection]
    set pile [PileSection]
    DefineOpenSeesSection $withShell $withoutShell $pile

    # Estimate yield curvature
    set Ky [expr {$epsye/(0.5*$Dc)}]
    
    set numIncr 100;	# Number of analysis increments

    MomentCurvature $shell $axialLoad [expr {$Ky*$ductility}] $numIncr

    set coords "0 0"

    # Compute moment-curvature
    set j 0
    set ok 0
    while {$ok >= 0 && $j < $numIncr} {

	set ok [analyze 1]

	set curv   [expr {[nodeDisp 2 3]*$ft}]
	set moment [expr {[getTime]/$ft}]

	lappend coords $curv $moment
	
	incr j
    }

    return $coords
}

proc MomentCurvature {secTag axialLoad maxK {numIncr 100} } {
# Axial load: assume positive is compression
	
    # Define two nodes at (0,0)
    node 1 0.0 0.0
    node 2 0.0 0.0
    
    # Fix all degrees of freedom except axial and bending
    fix 1 1 1 1
    fix 2 0 1 0
    
    # Define element
    #                         tag ndI ndJ  secTag
    element zeroLengthSection  1   1   2  $secTag

    # Define constant axial load
    pattern Plain 1 "Constant" {
	load 2 [expr -$axialLoad] 0.0 0.0
    }

    # Define analysis parameters
    integrator LoadControl 0.0
    system UmfPack
    test NormUnbalance 1.0e-9 10 
    numberer Plain
    constraints Plain
    algorithm Newton
    analysis Static
    
    # Do one analysis for constant axial load
    analyze 1
    
    # Define reference moment
    pattern Plain 2 "Linear" {
	load 2 0.0 0.0 1.0
    }
    
    # Compute curvature increment
    set dK [expr {$maxK/$numIncr}]
    
    # Use displacement control at node 2 for section analysis
    integrator DisplacementControl 2 3 $dK 1 $dK $dK
}

proc DefaultA706 {} {
    global sectionProps
    global whichSection
    set barNum $sectionProps(ds,$whichSection)

    global rfsteelProps
    
    # Values common to all bar sizes
    set rfsteelProps(E) 29000.0
    set rfsteelProps(fye) 68.0
    set rfsteelProps(fue) 95.0
    set rfsteelProps(epsye) 0.0023

    # Onset of strain hardening
    if {$barNum >= 3 && $barNum <= 8} {set rfsteelProps(epssh) 0.015}
    if {$barNum == 9} {set rfsteelProps(epssh) 0.0125}
    if {$barNum >= 10 && $barNum <= 11} {set rfsteelProps(epssh) 0.0115}
    if {$barNum == 14} {set rfsteelProps(epssh) 0.0075}
    if {$barNum == 18} {set rfsteelProps(epssh) 0.005}
    
    # (Reduced) ultimate tensile strain
    if {$barNum <= 10} {
	set rfsteelProps(epssuR) 0.09
	set rfsteelProps(epssu) 0.12
    } else {
	set rfsteelProps(epssuR) 0.06
	set rfsteelProps(epssu) 0.09
    }
}



proc AxialLoadCapacity {} {
# Approximation of max compressive axial load
    global units
    set in $units(in)
    set ksi $units(ksi)

    global sectionPropsTMP
    global whichSection
    set Dj [expr {$sectionPropsTMP(Dj,$whichSection)*$in}]
    set tj [expr {$sectionPropsTMP(tj,$whichSection)*$in}]
    set Dc [expr {$Dj-2*$tj}] ;# Inside diameter of steel shell

    global concretePropsTMP
    set fc [expr {$concretePropsTMP(fc)*$ksi}]

    global jacketPropsTMP
    set fy [expr {$jacketPropsTMP(fye)*$ksi}]

    set pi [expr {acos(-1.0)}]
    set Pmax [expr {$fc*$pi*0.25*$Dc*$Dc + $fy*$pi*0.25*($Dj*$Dj-$Dc*$Dc)}]

    return $Pmax
}
