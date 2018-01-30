proc Assign_concreteProps {} {
    global concreteProps
    global concretePropsTMP

    foreach var [array names concretePropsTMP] {
      if {[info exists concreteProps($var)] && $concreteProps($var) != $concretePropsTMP($var)} {
         ModelIsDirty
         SectionIsDirty
      }
	set concreteProps($var) $concretePropsTMP($var)
    }
}

proc AssignTMP_concreteProps {} {
    global concreteProps
    global concretePropsTMP

    foreach var [array names concreteProps] {
	set concretePropsTMP($var) $concreteProps($var)
    }
}

set concreteProps(fc) 5.2
set concreteProps(eco) 0.002
set concreteProps(esp) 0.006
AssignTMP_concreteProps

proc ManderModel {whichSection} {
    global units
    set ksi $units(ksi)
    set in $units(in)

    global concretePropsTMP

    set fc $concretePropsTMP(fc)
    if {![isValidDouble $fc] || $fc == 0.0} {return}
    set fc [expr {$fc*$ksi}]

    global sectionPropsTMP

    set Dj $sectionPropsTMP(Dj,$whichSection)
    if {![isValidDouble $Dj]} {return}
    set Dj [expr {$Dj*$in}]

    set tj $sectionPropsTMP(tj,$whichSection)
    if {![isValidDouble $tj]} {return}
    set tj [expr {$tj*$in}]

    global jacketPropsTMP
    set fyj $jacketPropsTMP(fye)
    set fyj [expr {$fyj*$ksi}]

    # Inside diameter of steel shell
    set Dc [expr {$Dj-2*$tj}]

    # Horizontal volumetric confining ratio of steel shell, Eq. 3.4
    set rhosj [expr {4*$tj/$Dc}]

    # Radial confining stress of steel shell, Eq. 3.3
    set fl [expr {0.5*$fyj*$rhosj}]
    
    # Confined concrete compressive strength
    set fcc [expr {$fc*(2.254*sqrt(1+7.94*$fl/$fc) - 2*$fl/$fc - 1.254)}]

    set concretePropsTMP(ecc,$whichSection) [expr {0.002*(1+5*($fcc/$fc-1))}]
    set concretePropsTMP(fcc,$whichSection) [expr {$fcc/$ksi}]


    set s $sectionPropsTMP(sphoop,$whichSection)
    if {![isValidDouble $s]} {return}
    set s [expr {$s*$in}]

    set fy $sectionPropsTMP(fyhoop,$whichSection)
    if {![isValidDouble $fy]} {return}
    set fy [expr {$fy*$ksi}]

    set cover $sectionPropsTMP(cover,$whichSection)
    if {![isValidDouble $cover]} {return}
    set cover [expr {$cover*$in}]

    set ds $sectionPropsTMP(dshoop,$whichSection)
    set Asp [BarArea $ds]
    set hoopDiam [BarDiam $ds]

    set sp [expr {$s-$hoopDiam}]

    set Nbars $sectionPropsTMP(Nbars,$whichSection)
    set dds $sectionPropsTMP(ds,$whichSection)
    set Adds [BarArea $dds]

    # Diameter of core section
    set Dcore [expr {$Dc-2*$cover}]
    set pi [expr {2*asin(1.0)}]
    set Acore [expr {$pi*0.25*$Dcore*$Dcore}]

    set rhocc [expr {$Nbars*$Adds/$Acore}]

    set ds [expr {$Dcore-$hoopDiam}]
    set ke [expr {(1-$sp/(2*$ds))/(1-$rhocc)}]

    set fl [expr {0.5*$ke*4*$Asp/($ds*$s)*$fy}]

    # Confined concrete compressive strength
    set fcc [expr {$fc*(2.254*sqrt(1+7.94*$fl/$fc) - 2*$fl/$fc - 1.254)}]

    set concretePropsTMP(ecchoop,$whichSection) [expr {0.002*(1+5*($fcc/$fc-1))}]
    set concretePropsTMP(fcchoop,$whichSection) [expr {$fcc/$ksi}]

    # If there is no steel shell, then set the section concrete props to be the same as the gap region
    if {$tj <= 0.0} {
	set concretePropsTMP(ecc,$whichSection) $concretePropsTMP(ecchoop,$whichSection)
	set concretePropsTMP(fcc,$whichSection) $concretePropsTMP(fcchoop,$whichSection)
    }
}

ManderModel 1
ManderModel 2
Assign_concreteProps

set concreteWindowOpen 0
proc DefineConcrete {w} {

    global concreteWindowOpen
    if {$concreteWindowOpen == 1} {raise $w; return}

    createTopLevel $w "Concrete Properties"

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left
    set m [menu $w.mbar.file.menu]

    $m add command -label "Close (Ctrl+W)" -command "destroy .concreteStressStrain; destroy $w"
    bind $w <Control-Key-w> "destroy .concreteStressStrain; destroy $w"

    set text(0) "The first three inputs are nominal values for unconfined concrete: \"f_ce\" is the expected concrete strength; \"e_co\" is the strain at which f_c is reached; and \"e_sp\" is the strain at which spalling occurs."
    set text(1) "Updating these values will cause the program to compute \"f_cc\" and \"e_cc\", the peak compressive stress and strain, respectively, according to Mander-based CISS procedure of Chai et al (1994).  The parameters \"f_cc\" and \"e_cc\" can be overwritten if desired."
    set text(2) "Pressing the Stress-Strain button will produce a plot of the stress-strain behavior of the confined and unconfined concrete."

    createHelp $w $w.mbar concrete text


    set concreteWindowOpen 1
    bind $w <Destroy> {set concreteWindowOpen 0}

    AssignTMP_concreteProps

    frame $w.input

    ManderModel 1
    ManderModel 2

    set row 0
    foreach {lbl ent unt} {"Unconfined: f_ce" fc ksi "e_co" eco "in/in" "e_sp" esp "in/in"} {
	label $w.input.label$row -text $lbl
	grid $w.input.label$row -row $row -column 0 -sticky e
	entry $w.input.entry$row -width 5 -textvariable concretePropsTMP($ent)
	bind $w.input.entry$row <KeyRelease> "ManderModel 1; ManderModel 2"
	grid $w.input.entry$row -row $row -column 1
	label $w.input.unit$row -text $unt
	grid $w.input.unit$row -row $row -column 2 -sticky w
	incr row
    }

    #button $w.input.mander -text "Mander Model" -command "ManderModel"
    #grid $w.input.mander -column 0 -row $row -columnspan 2 -sticky ew
    incr row

    foreach {lbl ent unt} {"Gap Section: f_cc" fcchoop ksi "e_cc" ecchoop "in/in"} {
	label $w.input.label$row -text $lbl
	grid $w.input.label$row -row $row -column 0 -sticky e
	entry $w.input.entry$row -width 5 -textvariable concretePropsTMP($ent,1) -state disabled
	grid $w.input.entry$row -row $row -column 1
	label $w.input.unit$row -text $unt
	grid $w.input.unit$row -row $row -column 2 -sticky w
	incr row
    }

    incr row

    foreach {lbl ent unt} {"CISS Section: f_cc" fcc ksi "e_cc" ecc "in/in"} {
	label $w.input.label$row -text $lbl
	grid $w.input.label$row -row $row -column 0 -sticky e
	entry $w.input.entry$row -width 5 -textvariable concretePropsTMP($ent,1) -state disabled
	grid $w.input.entry$row -row $row -column 1
	label $w.input.unit$row -text $unt
	grid $w.input.unit$row -row $row -column 2 -sticky w
	incr row
    }

    incr row

    foreach {lbl ent unt} {"Pile Section: f_cc" fcc ksi "e_cc" ecc "in/in"} {
	label $w.input.label$row -text $lbl
	grid $w.input.label$row -row $row -column 0 -sticky e
	entry $w.input.entry$row -width 5 -textvariable concretePropsTMP($ent,2) -state disabled
	grid $w.input.entry$row -row $row -column 1
	label $w.input.unit$row -text $unt
	grid $w.input.unit$row -row $row -column 2 -sticky w
	incr row
    }

    button $w.input.sigeps -text "Stress-Strain" -command "ManderModel 1; ManderModel 2; ConcreteStressStrain .concreteStressStrain $w"
    grid $w.input.sigeps -column 0 -row $row -columnspan 3 -sticky ew

    pack $w.input -side top

    frame $w.okcancel
    
    button $w.okcancel.ok -text "OK" -command "DefineConcrete_OK $w"
    grid $w.okcancel.ok -row 0 -column 0
    button $w.okcancel.cancel -text "Cancel" -command "DefineConcrete_Cancel $w"
    grid $w.okcancel.cancel -row 0 -column 1

    pack $w.okcancel -side top
}

proc DefineConcrete_Cancel {w} {
    AssignTMP_concreteProps

    destroy $w
}

proc DefineConcrete_OK {w} {

    set ok [CheckConcrete]
    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input for $ok" -icon error -parent $w
	return
    }

    Assign_concreteProps

    DefineModel [getCanvas]

    destroy $w
}

proc CheckConcrete {} {
    global concretePropsTMP

    set fc $concretePropsTMP(fc)
    if {![isValidDouble $fc] || $fc == 0.0} {
	return fc
    }

    set eco $concretePropsTMP(eco)
    if {![isValidDouble $eco] || $eco == 0.0} {
	return eco
    }

    set esp $concretePropsTMP(esp)
    if {![isValidDouble $esp] || $esp == 0.0} {
	return esp
    }
}

proc OpenSeesConcrete {tagUnconf tagCISS tagHoop tagPile} {
    global units
    set ksi $units(ksi)
    set psi $units(psi)
    
    global concretePropsTMP
    set fc $concretePropsTMP(fc)
    set fc [expr {$fc*$ksi}]

    set eco $concretePropsTMP(eco)
    set esp $concretePropsTMP(esp)

    global analysisProps
    set overstrength $analysisProps(overstrength)
    set allElastic $analysisProps(allElastic)

    set Ec [expr {57*sqrt($fc/$psi)*$ksi}]
    #set Ec [expr {2*$fc/$eco}]
    
    if {$allElastic} {
	uniaxialMaterial Elastic -$tagUnconf $Ec
    } else {
	uniaxialMaterial Concrete01 -$tagUnconf $fc $eco 0.0 $esp
    }
    uniaxialMaterial Multiplier $tagUnconf -$tagUnconf $overstrength

    # Force recalc of confined props to ensure
    # they are used in all analyses
    ManderModel 1

    set ecc $concretePropsTMP(ecc,1)
    set fcc $concretePropsTMP(fcc,1)
    set fcc [expr {$fcc*$ksi}]

    if {$allElastic} {
	uniaxialMaterial Elastic -$tagCISS $Ec
    } else {
	hystereticBackbone Mander $tagCISS $fcc $ecc $Ec
	uniaxialMaterial Backbone -$tagCISS $tagCISS
    }
    uniaxialMaterial Multiplier $tagCISS -$tagCISS $overstrength

    set ecchoop $concretePropsTMP(ecchoop,1)
    set fcchoop $concretePropsTMP(fcchoop,1)
    set fcchoop [expr {$fcchoop*$ksi}]
   
    if {$allElastic} {
	uniaxialMaterial Elastic -$tagHoop $Ec
    } else {
	hystereticBackbone Mander $tagHoop $fcchoop $ecchoop $Ec
	uniaxialMaterial Backbone -$tagHoop $tagHoop
    }
    uniaxialMaterial Multiplier $tagHoop -$tagHoop $overstrength

    ManderModel 2

    set ecc $concretePropsTMP(ecc,2)
    set fcc $concretePropsTMP(fcc,2)
    set fcc [expr {$fcc*$ksi}]

    if {$allElastic} {
	uniaxialMaterial Elastic -$tagPile $Ec
    } else {
	hystereticBackbone Mander $tagPile $fcc $ecc $Ec
	uniaxialMaterial Backbone -$tagPile $tagPile
    }
    uniaxialMaterial Multiplier $tagPile -$tagPile $overstrength
}

proc OpenSeesConcreteAnalysis {type} {
    global units
    set ksi $units(ksi)
    set psi $units(psi)

    global concretePropsTMP
    foreach var [array names concretePropsTMP] {
	set $var $concretePropsTMP($var)
    }
    set ec $concretePropsTMP(eco)
    set fc $concretePropsTMP(fc)
    set fc [expr {$fc*$ksi}]

    wipe

    model basic -ndm 1 -ndf 1

    node 1 0.0
    node 2 1.0

    fix 2 1

    OpenSeesConcrete 1 2 3 4

    set matTag 1; set strainLimit $esp
    if {$type == "CISS"} {
	set ecc $concretePropsTMP(ecc,1)
        set matTag 2; set strainLimit [expr {2*$ecc}]
    }
    if {$type == "Hoop"} {
	set ecc $concretePropsTMP(ecchoop,1)
        set matTag 3; set strainLimit [expr {2*$ecc}]
    }
    if {$type == "Pile"} {
	set ecc $concretePropsTMP(ecc,2)
        set matTag 4; set strainLimit [expr {2*$ecc}]
    }

    element truss 1 1 2 1.0 $matTag

    uniaxialMaterial Elastic 5 [expr {0.001*$fc/$ec}]
    element truss 2 1 2 1.0 5

    pattern Plain 1 Linear {
	load 1 1.0
    }

    integrator DisplacementControl 1 1 0.0001
    constraints Plain
    numberer Plain
    test NormUnbalance 1.0e-8 10
    algorithm Newton
    system UmfPack

    analysis Static

    set coords "0 0"

    set ok 0
    set j 0
    while {$ok >= 0 && [nodeDisp 1 1] < $strainLimit} {

	set ok [analyze 1]

	set strain [nodeDisp 1 1]
	set stress [expr {[getTime]/$ksi}]
	lappend coords $strain $stress

	incr j
    }

    return $coords
}

set concreteStressStrainWindowOpen 0
proc ConcreteStressStrain {w p} {

    set ok [CheckConcrete]
    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input for $ok" -icon error -parent $p
	return
    }

    global concreteStressStrainWindowOpen
    if {$concreteStressStrainWindowOpen == 1} {raise $w; return}

    createTopLevel $w "Concrete Stress-Strain"

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

    set emax 0
    set smax 0

    set coords1 [OpenSeesConcreteAnalysis CISS]
    $graph data d1 -colour blue -points 0 -lines 1 -coords $coords1

    set coords2 [OpenSeesConcreteAnalysis Unconfined]
    $graph data d2 -colour red -points 0 -lines 1 -coords $coords2

    set coords3 [OpenSeesConcreteAnalysis Hoop]
    $graph data d3 -colour black -points 0 -lines 1 -coords $coords3

    set coords4 [OpenSeesConcreteAnalysis Pile]
    $graph data d4 -colour black -points 0 -lines 1 -coords $coords4

    foreach {e s} "$coords1 $coords2 $coords3 $coords4" {
       if {$e > $emax} {set emax $e}
       if {$s > $smax} {set smax $s}
    }

    axesEMUGraph $graph $emax $smax
    $graph redraw

    global sectionPropsTMP
    set pileSameAsColumn $sectionPropsTMP(pileSameAsColumn)

    $m add command -label "Print (Ctrl+P)" -command "PrintCanvas $c $w"
    bind $w <Control-Key-p> "PrintCanvas $c $w"
    $m add cascade -label "Export" -menu $m.concretes
    set m2 [menu $m.concretes]
    $m2 add command -label "Unconfined" -command "ExportData [list $coords2] $w"
    $m2 add command -label "Gap" -command "ExportData [list $coords3] $w"
    if {$pileSameAsColumn} {
	$m2 add command -label "CISS/Pile" -command "ExportData [list $coords1] $w"
    } else {
	$m2 add command -label "CISS" -command "ExportData [list $coords1] $w"	
	$m2 add command -label "Pile" -command "ExportData [list $coords4] $w"
    }
    $m add separator
    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"


    set concreteStressStrainWindowOpen 1
    bind $w <Destroy> {set concreteStressStrainWindowOpen 0}


    global concretePropsTMP

    if {$pileSameAsColumn} {
	set xC [lindex $coords1 end-1]
	set yC [lindex $coords1 end]
	$c create text [$graph x2canvas $xC] [$graph y2canvas $yC] \
	    -text "CISS/Pile" -anchor ne -tag ciss
	$c bind ciss <Double-Button-1> "ExportData [list $coords1] $w"
    } else {
	set xC [lindex $coords1 end-1]
	set yC [lindex $coords1 end]
	$c create text [$graph x2canvas $xC] [$graph y2canvas $yC] \
	    -text "CISS" -anchor ne -tag ciss
	$c bind ciss <Double-Button-1> "ExportData [list $coords1] $w"

	set xC [lindex $coords4 end-1]
	set yC [lindex $coords4 end]
	$c create text [$graph x2canvas $xC] [$graph y2canvas $yC] \
	    -text "Pile" -anchor ne -tag pile
	$c bind pile <Double-Button-1> "ExportData [list $coords4] $w"
    }

    set xC [lindex $coords2 end-1]
    set yC [lindex $coords2 end]
    $c create text [$graph x2canvas $xC] [$graph y2canvas $yC] \
	-text "Unconfined" -anchor sw -tag unconfined
    $c bind unconfined <Double-Button-1> "ExportData [list $coords2] $w"
    
    set xC [lindex $coords3 end-1]
    set yC [lindex $coords3 end]
    $c create text [$graph x2canvas $xC] [$graph y2canvas $yC] \
	-text "Gap" -anchor ne -tag hoop
    $c bind hoop <Double-Button-1> "ExportData [list $coords3] $w"

    set graphInfo(title) "Concrete Stress-Strain"
    set graphInfo(xlabel) "Strain (in/in)"
    set graphInfo(ylabel) "Stress (ksi)"
    labelEMUGraph $c $graph graphInfo $width $height


    frame $w.output

    text $w.output.text -width 15
    grid $w.output.text -row 1 -column 0 -sticky nsew
    label $w.output.label -text "Numeric Output"
    grid $w.output.label -row 0 -column 0 -sticky nsew
    pack $w.output -side left

    $w.output.text insert end "Use the File->Export menu or double-click on a label, e.g., CISS, to export data"

    $w.output.text configure -state disabled


}
