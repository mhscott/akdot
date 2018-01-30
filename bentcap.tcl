proc Assign_bentcapProps {} {
    global bentcapProps
    global bentcapPropsTMP

    foreach var [array names bentcapPropsTMP] {
       if {[info exists bentcapProps($var)] && $bentcapProps($var) != $bentcapPropsTMP($var)} {
          ModelIsDirty
       }
       set bentcapProps($var) $bentcapPropsTMP($var)
    }

    foreach var [array names bentcapPropsTMP] {
	set bentcapProps($var) $bentcapPropsTMP($var)
    }
}

proc AssignTMP_bentcapProps {} {
    global bentcapProps
    global bentcapPropsTMP

    foreach var [array names bentcapProps] {
	set bentcapPropsTMP($var) $bentcapProps($var)
    }
}

set bentcapProps(dcap) 48.0
set bentcapProps(bcap) 72.0
set bentcapProps(leftoverhang) 18.0
set bentcapProps(rightoverhang) 18.0
AssignTMP_bentcapProps

set bentcapWindowOpen 0
proc DefineBentcap {w} {

    global bentcapWindowOpen
    if {$bentcapWindowOpen == 1} {raise $w; return}

    createTopLevel $w "Bent Cap"

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left

    set m [menu $w.mbar.file.menu]
    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"

    set text(0) "The input values are used to determine the bent cap stiffness during the pushover analysis in lieu of rigid links."
    set text(1) "Changes in the depth are shown on screen, while changes in the width are not visible since this dimension is perpendicular to the plane of view."

    createHelp $w $w.mbar bentcap text


    set bentcapWindowOpen 1
    bind $w <Destroy> {set bentcapWindowOpen 0}

    global bentcapProps
    global bentcapPropsTMP

    foreach var [array names bentcapProps] {
	set bentcapPropsTMP($var) $bentcapProps($var)
    }

    frame $w.input

    set row 0

    label $w.input.labeld -text "Depth"
    grid $w.input.labeld -row $row -column 0 -sticky e
    entry $w.input.entryd -width 5 -textvariable bentcapPropsTMP(dcap)
    bind $w.input.entryd <Return> "DefineBentcap_OK $w"
    grid $w.input.entryd -row $row -column 1
    label $w.input.unitd -text "in"
    grid $w.input.unitd -row $row -column 2 -sticky w

    incr row

    label $w.input.labelb -text "Width"
    grid $w.input.labelb -row $row -column 0 -sticky e
    entry $w.input.entryb -width 5 -textvariable bentcapPropsTMP(bcap)
    bind $w.input.entryb <Return> "DefineBentcap_OK $w"
    grid $w.input.entryb -row $row -column 1
    label $w.input.unitb -text "in"
    grid $w.input.unitb -row $row -column 2 -sticky w

    incr row

    label $w.input.labello -text "Left Overhang"
    grid $w.input.labello -row $row -column 0 -sticky e
    entry $w.input.entrylo -width 5 -textvariable bentcapPropsTMP(leftoverhang)
    bind $w.input.entrylo <Return> "DefineBentcap_OK $w"
    grid $w.input.entrylo -row $row -column 1
    label $w.input.unitlo -text "in"
    grid $w.input.unitlo -row $row -column 2 -sticky w

    incr row

    label $w.input.labelro -text "Right Overhang"
    grid $w.input.labelro -row $row -column 0 -sticky e
    entry $w.input.entryro -width 5 -textvariable bentcapPropsTMP(rightoverhang)
    bind $w.input.entryro <Return> "DefineBentcap_OK $w"
    grid $w.input.entryro -row $row -column 1
    label $w.input.unitro -text "in"
    grid $w.input.unitro -row $row -column 2 -sticky w

    pack $w.input -side top

    frame $w.okcancel
    
    button $w.okcancel.ok -text "OK" -command "DefineBentcap_OK $w"
    grid $w.okcancel.ok -row 0 -column 0
    button $w.okcancel.cancel -text "Cancel" -command "DefineBentcap_Cancel $w"
    grid $w.okcancel.cancel -row 0 -column 1

    pack $w.okcancel -side top
}

proc DefineBentcap_Cancel {w} {
    AssignTMP_bentcapProps

    destroy $w
}

proc DefineBentcap_OK {w} {
    
    set ok [CheckBentcap]
    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input for $ok" -icon error -parent $w
	return
    }

    Assign_bentcapProps

    DefineModel [getCanvas]

    destroy $w
}

proc CheckBentcap {} {
    global bentcapPropsTMP

    set bcap $bentcapPropsTMP(bcap)
    if {![isValidDouble $bcap] || $bcap <= 0.0} {
	return width
    }

    set dcap $bentcapPropsTMP(dcap)
    if {![isValidDouble $dcap] || $dcap < 0.0} {
	return depth
    }

    set dcap $bentcapPropsTMP(leftoverhang)
    if {![isValidDouble $dcap] || $dcap < 0.0} {
	return "left overhang"
    }

    set dcap $bentcapPropsTMP(rightoverhang)
    if {![isValidDouble $dcap] || $dcap < 0.0} {
	return "right overhang"
    }
}
