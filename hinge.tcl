proc Assign_hingeProps {} {
    global hingeProps
    global hingePropsTMP

    foreach var [array names hingePropsTMP] {
      if {[info exists hingeProps($var)] && $hingeProps($var) != $hingePropsTMP($var)} {
         ModelIsDirty
      }
	set hingeProps($var) $hingePropsTMP($var)
    }
}

proc AssignTMP_hingeProps {} {
    global hingeProps
    global hingePropsTMP

    foreach var [array names hingeProps] {
	set hingePropsTMP($var) $hingeProps($var)
    }
}

set hingeProps(Gf) 2.0
set hingeProps(coverConfined) 1
set hingeProps(coreConfined) 0
AssignTMP_hingeProps

set hingeWindowOpen 0
proc DefineHinge {w} {
    global hingeWindowOpen

    # If already open, do not attempt to open a duplicate
    if {$hingeWindowOpen == 1} {return}

    # Create new window
    createTopLevel $w "Gap Region"

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left
    set m [menu $w.mbar.file.menu]

    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"

    set text(0) "The first input value, \"Gf\" is the gap between the bent cap and the steel jacket."
    set text(1) "Changing the value of Gf will cause the program to automatically compute the plastic hinge length from the input gap value and the reinforcing steel properties according to Eq. 4.11.6-3 in design specification. This plastic hinge length can be overwritten if desired."
    set text(2) "Indicate whether the concrete in the gap region should be modelled as confined or unconfined."

    createHelp $w $w.mbar hinge text


    set hingeWindowOpen 1
    bind $w <Destroy> {set hingeWindowOpen 0}

    AssignTMP_hingeProps

    frame $w.input

    label $w.input.labelGf -text "Gap, Gf"
    grid $w.input.labelGf -row 0 -column 0 -sticky e
    entry $w.input.entryGf -width 5 -textvariable hingePropsTMP(Gf)
    bind $w.input.entryGf <KeyRelease> "Computelp"
    bind $w.input.entryGf <Return> "DefineHinge_OK $w"
    grid $w.input.entryGf -row 0 -column 1
    label $w.input.unitGf -text "in"
    grid $w.input.unitGf -row 0 -column 2 -sticky w

    label $w.input.labellp -text "Plastic Hinge Length, lp"
    grid $w.input.labellp -row 1 -column 0 -sticky e
    entry $w.input.entrylp -width 5 -textvariable hingePropsTMP(lp)
    bind $w.input.entrylp <Return> "DefineHinge_OK $w"
    grid $w.input.entrylp -row 1 -column 1
    label $w.input.unitlp -text "ft"
    grid $w.input.unitlp -row 1 -column 2 -sticky w

    pack $w.input -side top

    frame $w.confine

    label $w.confine.radioCore -text "Core Confinement"
    grid $w.confine.radioCore -row 0 -column 0 -sticky e
    radiobutton $w.confine.coreYes -variable hingePropsTMP(coreConfined) -text "Steel Shell" -value 1
    grid $w.confine.coreYes -row 0 -column 1
    radiobutton $w.confine.coreNo -variable hingePropsTMP(coreConfined) -text "Spiral Hoop" -value 0 -command ""
    grid $w.confine.coreNo -row 0 -column 2

    label $w.confine.radioGap -text "Cover Confinement"
    grid $w.confine.radioGap -row 1 -column 0 -sticky e
    radiobutton $w.confine.gapYes -variable hingePropsTMP(coverConfined) -text "Same as Core" -value 1
    grid $w.confine.gapYes -row 1 -column 1
    radiobutton $w.confine.gapNo -variable hingePropsTMP(coverConfined) -text "Unconfined" -value 0
    grid $w.confine.gapNo -row 1 -column 2

    pack $w.confine -side top

    frame $w.okcancel
    
    button $w.okcancel.ok -text "OK" -command "DefineHinge_OK $w"
    grid $w.okcancel.ok -row 0 -column 0
    button $w.okcancel.cancel -text "Cancel" -command "DefineHinge_Cancel $w"
    grid $w.okcancel.cancel -row 0 -column 1

    pack $w.okcancel -side top
}

proc DefineHinge_Cancel {w} {
    AssignTMP_hingeProps

    destroy $w
}

proc DefineHinge_OK {w} {
    
    set ok [CheckHinge]
    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input for $ok" -icon error -parent $w
	return
    }

    Assign_hingeProps

    DefineModel [getCanvas]

    destroy $w
}

proc CheckHinge {} {
    global hingePropsTMP

    set Gf $hingePropsTMP(Gf)
    if {![isValidDouble $Gf] || $Gf < 0.0} {
	return Gf
    }

    set lp $hingePropsTMP(lp)
    if {![isValidDouble $lp] || $lp < 0.0} {
	return lp
    }
}


proc Computelp {} {
    global units
    set in $units(in)
    set ksi $units(ksi)

    global sectionPropsTMP
    set ds $sectionPropsTMP(ds,1)
    if {![isValidDouble $ds]} {return}

    global rfsteelPropsTMP
    set fye $rfsteelPropsTMP(fye)
    if {![isValidDouble $fye]} {return}

    global hingePropsTMP
    set Gf $hingePropsTMP(Gf)
    if {![isValidDouble $Gf]} {return}

    set lp [expr {$Gf*$in + 0.3*$fye*[BarDiam $ds]}]

    # Make sure computed lp is less than the column above grade height
    global columnPropsTMP
    set L $columnPropsTMP(L)
    if {$lp > $L} {
	set lp [expr {0.2*$L}]
    }

    set hingePropsTMP(lp) [format %.2f $lp]
}

# Initialize plastic hinge length
Computelp
set hingeProps(lp) $hingePropsTMP(lp)
