proc Assign_eqProps {} {
    global eqProps
    global eqPropsTMP    

    foreach var [array names eqPropsTMP] {
       if {[info exists eqProps($var)] && $eqProps($var) != $eqPropsTMP($var)} {
          ModelIsDirty
       }
       set eqProps($var) $eqPropsTMP($var)
    }

    foreach var [array names eqPropsTMP] {
	set eqProps($var) $eqPropsTMP($var)
    }
}

proc AssignTMP_eqProps {} {
    global eqProps
    global eqPropsTMP

    foreach var [array names eqProps] {
	set eqPropsTMP($var) $eqProps($var)
    }
}

set eqProps(PGA) 1.0
set eqProps(dtGM) 0.01
set eqProps(filename) "None Selected"

AssignTMP_eqProps


set eqMotionWindowOpen 0
proc DefineEQMotion {w} {

    global eqMotionWindowOpen
    if {$eqMotionWindowOpen == 1} {raise $w; return}

    createTopLevel $w "Earthquake Motion"

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left

    set m [menu $w.mbar.file.menu]
    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"

    #set text(0) "The input value is the gravity load applied to the indicated column and held constant during the pushover analysis."
    #set text(1) "Positive values indicate compression while negative values indicate tension."

    #createHelp $w $w.mbar gravityLoad text


    set eqMotionWindowOpen 1
    bind $w <Destroy> {set eqMotionWindowOpen 0}    

    global eqProps
    global eqPropsTMP

    foreach var [array names eqProps] {
	set eqPropsTMP($var) $eqProps($var)
    }

    frame $w.input

    label $w.input.labels -text "PGA"
    grid $w.input.labels -row 0 -column 0 -sticky e
    entry $w.input.entrys -width 5 -textvariable eqPropsTMP(PGA)
    bind $w.input.entrys <Return> "DefineEQMotion_OK $w"
    grid $w.input.entrys -row 0 -column 1
    label $w.input.units -text "g"
    grid $w.input.units -row 0 -column 2 -sticky w

    label $w.input.labelb -text "Record Time Step"
    grid $w.input.labelb -row 1 -column 0 -sticky e
    entry $w.input.entryb -width 5 -textvariable eqPropsTMP(dtGM)
    bind $w.input.entryb <Return> "DefineEQMotion_OK $w"
    grid $w.input.entryb -row 1 -column 1
    label $w.input.unitb -text "sec"
    grid $w.input.unitb -row 1 -column 2 -sticky w

    button $w.input.buttong -text "Ground Motion" -command "LoadEQMotion $w"
    grid $w.input.buttong -row 2 -column 0 -columnspan 3 -sticky ew

    label $w.input.labelgm -width 20 -textvariable eqPropsTMP(filename) -anchor e
    grid $w.input.labelgm -row 3 -column 0 -columnspan 3 -sticky ew

    pack $w.input -side top

    frame $w.okcancel
    
    button $w.okcancel.ok -text "OK" -command "DefineEQMotion_OK $w"
    grid $w.okcancel.ok -row 0 -column 0
    button $w.okcancel.cancel -text "Cancel" -command "DefineEQMotion_Cancel $w"
    grid $w.okcancel.cancel -row 0 -column 1

    pack $w.okcancel -side top
}

proc LoadEQMotion {w} {
    global wd
    global eqPropsTMP
    global EQtypelist

    set openFile [tk_getOpenFile -defaultextension .txt -filetypes $EQtypelist -initialdir $wd -parent $w]
    if {[llength $openFile] <= 0} {
	return
    }

    set eqPropsTMP(filename) $openFile
}

proc DefineEQMotion_Cancel {w} {
    AssignTMP_eqProps

    destroy $w
}

proc DefineEQMotion_OK {w} {
    
    set ok [CheckEQMotion]

    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input for $ok" -icon error -parent $w
	return
    }

    Assign_eqProps

    DefineModel [getCanvas]

    destroy $w
}

proc CheckEQMotion {} {
    global eqPropsTMP

    set x $eqPropsTMP(PGA)
    if {![isValidDouble $x] || $x <= 0.0} {
	return "ground motion PGA"
    }
    set x $eqPropsTMP(dtGM)
    if {![isValidDouble $x] || $x <= 0.0} {
	return "ground motion record time digitization"
    }
}
