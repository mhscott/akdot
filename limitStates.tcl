proc Assign_limitStates {} {
    global limitStates
    global limitStatesTMP

    foreach var [array names limitStatesTMP] {
      if {$limitStates($var) != $limitStatesTMP($var)} {
         ModelIsDirty
      }
	set limitStates($var) $limitStatesTMP($var)
    }
}

proc AssignTMP_limitStates {} {
    global limitStates
    global limitStatesTMP

    foreach var [array names limitStates] {
	set limitStatesTMP($var) $limitStates($var)
    }
}

set limitStates(steel) 0.06
set limitStates(jacket) 0.026
set limitStates(concrete) 0.02
AssignTMP_limitStates

set limitstateWindowOpen 0
proc DefineLimitStates {w} {

    global limitstateWindowOpen
    if {$limitstateWindowOpen == 1} {raise $w; return}

    createTopLevel $w "Strain Limit States"

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left

    set m [menu $w.mbar.file.menu]
    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"

    set text(0) "The input values are strain levels for which, if reached during the analysis, the associated lateral displacment is shown as a vertical line on the pushover curve."

    createHelp $w $w.mbar limitStates text


    set limitstateWindowOpen 1
    bind $w <Destroy> {set limitstateWindowOpen 0}

    global limitStates
    global limitStatesTMP

    foreach var [array names limitStates] {
	set limitStatesTMP($var) $limitStates($var)
    }

    frame $w.input

    set row 0
    foreach {label tag} {"Reinforcing Steel" steel  "Steel Shell" jacket  "Concrete" concrete} {

       label $w.input.label$tag -text $label
       grid $w.input.label$tag -row $row -column 0 -sticky e
       entry $w.input.entry$tag -width 5 -textvariable limitStatesTMP($tag)
       bind $w.input.entry$tag <Return> "DefineLimitStates_OK $w"
       grid $w.input.entry$tag -row $row -column 1
       label $w.input.units$tag -text "in/in"
       grid $w.input.units$tag -row $row -column 2 -sticky w
       incr row
    }

    pack $w.input -side top

    frame $w.okcancel
    
    button $w.okcancel.ok -text "OK" -command "DefineLimitStates_OK $w"
    grid $w.okcancel.ok -row 0 -column 0
    button $w.okcancel.cancel -text "Cancel" -command "DefineLimitStates_Cancel $w"
    grid $w.okcancel.cancel -row 0 -column 1

    pack $w.okcancel -side top
}

proc DefineLimitStates_Cancel {w} {
    AssignTMP_limitStates

    destroy $w
}

proc DefineLimitStates_OK {w} {
    
    set ok [CheckLimitStates]
    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input for $ok" -icon error -parent $w
	return
    }

    Assign_limitStates

    DefineModel [getCanvas]

    destroy $w
}

proc CheckLimitStates {} {
    global limitStatesTMP

    set bcap $limitStatesTMP(steel)
    if {![isValidDouble $bcap] || $bcap <= 0.0} {
	return "reinforcing steel limit state"
    }

    set bcap $limitStatesTMP(jacket)
    if {![isValidDouble $bcap] || $bcap <= 0.0} {
	return "steel shell limit state"
    }

    set bcap $limitStatesTMP(concrete)
    if {![isValidDouble $bcap] || $bcap <= 0.0} {
	return "concrete limit state"
    }

}
