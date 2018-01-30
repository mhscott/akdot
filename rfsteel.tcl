proc Assign_rfsteelProps {} {
    global rfsteelProps
    global rfsteelPropsTMP

    foreach var [array names rfsteelPropsTMP] {
       if {[info exists rfsteelProps($var)] && $rfsteelProps($var) != $rfsteelPropsTMP($var)} {
          ModelIsDirty
          SectionIsDirty
       }
	 set rfsteelProps($var) $rfsteelPropsTMP($var)
    }
}

proc AssignTMP_rfsteelProps {} {
    global rfsteelProps
    global rfsteelPropsTMP

    foreach var [array names rfsteelProps] {
	set rfsteelPropsTMP($var) $rfsteelProps($var)
    }
}

set rfsteelProps(E) 29000.0
set rfsteelProps(fye) 68.0
set rfsteelProps(fue) 95.0
set rfsteelProps(C1) 1.5
set rfsteelProps(epssh) 0.0115
set rfsteelProps(epssuR) 0.06
set rfsteelProps(epssu) 0.09
AssignTMP_rfsteelProps


set rebarSteelWindowOpen 0
proc DefineRebarSteel {w} {

    global rebarSteelWindowOpen
    if {$rebarSteelWindowOpen == 1} {raise $w; return}

    createTopLevel $w "Reinforcing Steel Properties"

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left
    set m [menu $w.mbar.file.menu]

    $m add command -label "Close (Ctrl+W)" -command "destroy .rebarStressStrain; destroy $w"
    bind $w <Control-Key-w> "destroy .rebarStressStrain; destroy $w"

    set text(0) "The input parameters define the stress-strain behavior of steel according to the model proposed by Raynor et al (2002)."
    set text(1) "Pressing the Stress-Strain button will produce a plot of the stress-strain behavior of the reinforcing steel."

    createHelp $w $w.mbar rfsteel text

    set rebarSteelWindowOpen 1
    bind $w <Destroy> {set rebarSteelWindowOpen 0}

    AssignTMP_rfsteelProps

    frame $w.input

    set row 0
    foreach {lbl ent unt} {"E" E ksi "f_ye" fye ksi "f_ue" fue ksi "e_sh" epssh "in/in" "e_suR" epssuR "in/in" "e_su" epssu "in/in" "C1" C1 ""} {
	label $w.input.label$row -text $lbl
	grid $w.input.label$row -row $row -column 0 -sticky e
	entry $w.input.entry$row -width 7 -textvariable rfsteelPropsTMP($ent)
	grid $w.input.entry$row -row $row -column 1
	label $w.input.unit$row -text $unt
	grid $w.input.unit$row -row $row -column 2 -sticky w
	incr row
    }

    button $w.input.sigeps -text "Stress-Strain" -command "RebarStressStrain .rebarStressStrain $w"
    grid $w.input.sigeps -row $row -column 0 -columnspan 3 -sticky ew

    pack $w.input -side top

    frame $w.okcancel
    
    button $w.okcancel.ok -text "OK" -command "DefineRebarSteel_OK $w"
    grid $w.okcancel.ok -row 0 -column 0
    button $w.okcancel.cancel -text "Cancel" -command "DefineRebarSteel_Cancel $w"
    grid $w.okcancel.cancel -row 0 -column 1

    pack $w.okcancel -side top
}

proc DefineRebarSteel_Cancel {w} {
    AssignTMP_rfsteelProps

    destroy $w
}

proc DefineRebarSteel_OK {w} {
    
    set ok [CheckRebarSteel]
    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input for $ok" -icon error -parent $w
	return
    }

    Assign_rfsteelProps

    DefineModel [getCanvas]

    destroy $w
}

proc CheckRebarSteel {} {
    global rfsteelPropsTMP

    set E $rfsteelPropsTMP(E)
    if {![isValidDouble $E] || $E <= 0.0} {
	return E
    }

    set fye $rfsteelPropsTMP(fye)
    if {![isValidDouble $fye] || $fye < 0.0} {
	return fye
    }

    set fue $rfsteelPropsTMP(fue)
    if {![isValidDouble $fue] || $fue < 0.0} {
	return fue
    }

    set epssh $rfsteelPropsTMP(epssh)
    if {![isValidDouble $epssh] || $epssh <= 0.0} {
	return epssh
    }

    set epssuR $rfsteelPropsTMP(epssuR)
    if {![isValidDouble $epssuR] || $epssuR <= 0.0} {
	return epssuR
    }

    set epssu $rfsteelPropsTMP(epssu)
    if {![isValidDouble $epssu] || $epssu <= 0.0} {
	return epssu
    }

    set C1 $rfsteelPropsTMP(C1)
    if {![isValidDouble $C1] || $C1 <= 0.0} {
	return C1
    }
}

proc OpenSeesRebarSteel {tag} {
    global units
    set ksi $units(ksi)

    global analysisProps
    set overstrength $analysisProps(overstrength)
    set allElastic $analysisProps(allElastic)

    global rfsteelPropsTMP
    foreach var [array names rfsteelPropsTMP] {
	set $var $rfsteelPropsTMP($var)
    }
    set E [expr $E*$ksi]
    set fye [expr $fye*$ksi]
    set fue [expr $fue*$ksi]

    if {$allElastic} {
	uniaxialMaterial Elastic -$tag $E
    } else {
	hystereticBackbone Raynor $tag $E $fye $fue $epssh $epssu $C1 [expr 0.0001*$E]
	uniaxialMaterial Backbone -$tag $tag
    }
    uniaxialMaterial Multiplier $tag -$tag $overstrength
}

proc OpenSeesRebarAnalysis {} {
    global rfsteelPropsTMP
    set epssu $rfsteelPropsTMP(epssu)
    set fye $rfsteelPropsTMP(fye)
    set fue $rfsteelPropsTMP(fue)

    global units
    set ksi $units(ksi)

    wipe

    model basic -ndm 1 -ndf 1

    node 1 0.0
    node 2 1.0

    fix 2 1

    OpenSeesRebarSteel 1

    element truss 1 1 2 1.0 1

    pattern Plain 1 Linear {
	load 1 1.0
    }

    integrator DisplacementControl 1 1 0.0001
    constraints Penalty 1.0e8 1.0e8
    test NormUnbalance 1.0e-8 10
    algorithm Newton
    numberer Plain
    system UmfPack

    analysis Static

    set coords "0 0"

    set ok 0
    set j 0
    while {$ok >= 0 && [nodeDisp 1 1] < [expr $epssu]} {

	set ok [analyze 1]

	set strain [nodeDisp 1 1]
	set stress [expr {[getTime]/$ksi}]
	lappend coords $strain $stress

	incr j
    }

    return $coords
}

set rebarStressStrainWindowOpen 0
proc RebarStressStrain {w p} {

    set ok [CheckRebarSteel]
    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input for $ok" -icon error -parent $p
	return
    }

    global rebarStressStrainWindowOpen
    if {$rebarStressStrainWindowOpen == 1} {return}

    createTopLevel $w "Reinforcing Steel Stress-Strain"

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

    set coords [OpenSeesRebarAnalysis]
    $graph data d2 -colour blue -points 0 -lines 1 -coords $coords

    # Find max stress and strain
    set emax 0
    set smax 0
    foreach {e s} $coords {
       if {$e > $emax} {set emax $e}
       if {$s > $smax} {set smax $s}
    }

    axesEMUGraph $graph $emax $smax
    $graph redraw

    set graphInfo(title) "Reinforcing Steel Stress-Strain"
    set graphInfo(xlabel) "Strain (in/in)"
    set graphInfo(ylabel) "Stress (ksi)"
    labelEMUGraph $c $graph graphInfo $width $height


    $m add command -label "Print (Ctrl+P)" -command "PrintCanvas $c $w"
    bind $w <Control-Key-p> "PrintCanvas $c $w"
    $m add command -label "Export (Ctrl+X)" -command "ExportData [list $coords] $w"
    bind $w <Control-Key-x> "ExportData [list $coords] $w"
    $m add separator
    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"


    set rebarStressStrainWindowOpen 1
    bind $w <Destroy> {set rebarStressStrainWindowOpen 0}

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
