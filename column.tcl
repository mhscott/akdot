proc Assign_columnProps {} {
    global units
    set in $units(in)

    global sectionPropsTMP
    set Dj [expr $sectionPropsTMP(Dj,1)*$in]

    global columnProps
    global columnPropsTMP

    # Check that columns are not overlapping
    for {set i 2} {$i <= $columnPropsTMP(N)} {incr i} {
	if {$columnPropsTMP(space,$i) < $Dj} {
	    set columnPropsTMP(space,$i) $Dj
	}
    }

    # Make all column spacings equal as per user input
    global spaceAllColumns
    global whichColumn
    if {$spaceAllColumns != 0} {
       set space $columnPropsTMP(space,$whichColumn)
       for {set i 2} {$i <= $columnPropsTMP(N)} {incr i} {
          set columnPropsTMP(space,$i) $space
       }
    }
    set spaceAllColumns 0

    # Make all gravity loads the same as per user input
    global sameLoadAllColumns
    if {$sameLoadAllColumns != 0} {
       set load $columnPropsTMP(axialLoad,$whichColumn)
       set pmultT $columnPropsTMP(pMultT,$whichColumn)
       set pmultL $columnPropsTMP(pMultL,$whichColumn)
       for {set i 1} {$i <= $columnPropsTMP(N)} {incr i} {
          set columnPropsTMP(axialLoad,$i) $load
          set columnPropsTMP(pMultT,$i) $pmultT
          set columnPropsTMP(pMultL,$i) $pmultL
       }
    }
    set sameLoadAllColumns 0

    foreach var [array names columnPropsTMP] {
      if {[info exists columnProps($var)] && $columnProps($var) != $columnPropsTMP($var)} {
         ModelIsDirty
      }
	set columnProps($var) $columnPropsTMP($var)
    }
}

proc AssignTMP_columnProps {} {
    global columnProps
    global columnPropsTMP

    foreach var [array names columnProps] {
      set columnPropsTMP($var) $columnProps($var)
    }
}

set columnProps(L) 14.0
set columnProps(N) 3
set axialLoad [expr 0.1*[AxialLoadCapacity]]
set axialLoad [format %.1f $axialLoad]
for {set i 1} {$i <= $columnProps(N)} {incr i} {
    set columnProps(axialLoad,$i) $axialLoad
    set columnProps(pMultT,$i) 1.0
    set columnProps(pMultL,$i) 1.0
}
set columnProps(space,1) 0.0
for {set i 2} {$i <= $columnProps(N)} {incr i} {
    set columnProps(space,$i) 8.0
}
AssignTMP_columnProps

proc ChangeColumns {} {
    global columnProps
    global columnPropsTMP
    
    if {$columnPropsTMP(N) <= 0} {
	set columnPropsTMP(N) 1
    }

    # Assign default values if there are new columns
    set N $columnProps(N)
    set lastSpace $columnProps(space,$N)
    set lastAxialLoad $columnProps(axialLoad,$N)
    set lastPMultT $columnProps(pMultT,$N)
    set lastPMultL $columnProps(pMultL,$N)
    for {set i [expr $columnProps(N)+1]} {$i <= $columnPropsTMP(N)} {incr i} {
	set columnPropsTMP(space,$i) $lastSpace
	set columnPropsTMP(axialLoad,$i) $lastAxialLoad
	set columnPropsTMP(pMultT,$i) $lastPMultT
	set columnPropsTMP(pMultL,$i) $lastPMultL
    }
}

proc ChangeGravityLoadMenu {m} {
    global columnProps
    global columnPropsTMP

    for {set i [expr $columnProps(N)+1]} {$i <= $columnPropsTMP(N)} {incr i} {
	$m add command -label "Column/Pile $i" -command "set whichColumn $i; DefineGravityLoad .gravity"
    }
    for {set i $columnProps(N)} {$i > $columnPropsTMP(N)} {incr i -1} {
	$m delete $i
    }
}

set spaceAllColumns 0
set columnSpacingWindowOpen 0
proc ColumnSpacing {w} {

    global columnSpacingWindowOpen
    if {$columnSpacingWindowOpen == 1} {raise $w; return}

    global whichColumn
    if {$whichColumn == 1} {return}

    createTopLevel $w "Column Spacing"

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left

    set m [menu $w.mbar.file.menu]
    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"

    set text(0) "The input value is the center-to-center distance between the designated columns."

    createHelp $w $w.mbar columnSpace text


    set columnSpacingWindowOpen 1
    bind $w <Destroy> {set columnSpacingWindowOpen 0}

    global columnProps
    global columnPropsTMP

    foreach var [array names columnProps] {
	set columnPropsTMP($var) $columnProps($var)
    }

    frame $w.input

    global spaceAllColumns
    set spaceAllColumns 0

    label $w.input.spacing -text "Spacing between columns [expr $whichColumn-1] and $whichColumn"
    grid $w.input.spacing -row 0 -column 0 -columnspan 2
    entry $w.input.entry -textvariable columnPropsTMP(space,$whichColumn) -width 5
    grid $w.input.entry -row 1 -column 0 -sticky e
    label $w.input.ft -text "ft"
    grid $w.input.ft -row 1 -column 1 -sticky w
    checkbutton $w.input.all -text "Apply to all" -variable spaceAllColumns
    grid $w.input.all -row 2 -column 0 -columnspan 2 -sticky ew

    pack $w.input -side top

    frame $w.okcancel
    
    button $w.okcancel.ok -text "OK" -command "DefineSpacing_OK $w"
    grid $w.okcancel.ok -row 0 -column 0
    button $w.okcancel.cancel -text "Cancel" -command "DefineSpacing_Cancel $w"
    grid $w.okcancel.cancel -row 0 -column 1

    pack $w.okcancel -side top
}

proc DefineSpacing_Cancel {w} {
    AssignTMP_columnProps

    destroy $w
}

proc DefineSpacing_OK {w} {
    
    set ok [CheckSpacing]
    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input for $ok" -icon error -parent $w
	return
    }

    Assign_columnProps

    DefineModel [getCanvas]

    destroy $w
}

proc CheckSpacing {} {
    global columnPropsTMP
    set N $columnPropsTMP(N)

    for {set i 2} {$i <= $N} {incr i} {
       set load $columnPropsTMP(space,$i)
       if {![isValidDouble $load] || $load <= 0.0} {
          return "spacing between columns [expr $i-1] and $i"
       }
    }
}


set sameLoadAllColumns 0
set gravityLoadWindowOpen 0
proc DefineGravityLoad {w} {

    global gravityLoadWindowOpen
    if {$gravityLoadWindowOpen == 1} {raise $w; return}

    createTopLevel $w "Column/Pile Loads"

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left

    set m [menu $w.mbar.file.menu]
    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"

    set text(0) "The input value is the gravity load applied to the indicated column and held constant during the pushover analysis."
    set text(1) "Positive values indicate compression while negative values indicate tension."

    createHelp $w $w.mbar gravityLoad text


    set gravityLoadWindowOpen 1
    bind $w <Destroy> {set gravityLoadWindowOpen 0}
    
    global whichColumn

    global columnProps
    global columnPropsTMP

    foreach var [array names columnProps] {
	set columnPropsTMP($var) $columnProps($var)
    }

    frame $w.input

    global sameLoadAllColumns
    set sameLoadAllColumns 0

    set row 0

    label $w.input.col -text "Column/Pile $whichColumn" 
    grid $w.input.col -row $row -column 0 -columnspan 3

    incr row

    label $w.input.loadLabel -text "Gravity Load"
    grid $w.input.loadLabel -row $row -column 0 -sticky e
    entry $w.input.load -width 5 -textvariable columnPropsTMP(axialLoad,$whichColumn)
    bind $w.input.load <Return> "DefineGravityLoad_OK $w"
    grid $w.input.load -row $row -column 1 -sticky e
    label $w.input.ft -text "kip"
    grid $w.input.ft -row $row -column 2 -sticky w

    incr row

    label $w.input.pmultLabelT -text "p-Multiplier Transverse"
    grid $w.input.pmultLabelT -row $row -column 0 -sticky e
    entry $w.input.pmultT -width 5 -textvariable columnPropsTMP(pMultT,$whichColumn)
    bind $w.input.pmultT <Return> "DefineGravityLoad_OK $w"
    grid $w.input.pmultT -row $row -column 1 -sticky e

    incr row

    label $w.input.pmultLabelL -text "p-Multiplier Longitudinal"
    grid $w.input.pmultLabelL -row $row -column 0 -sticky e
    entry $w.input.pmultL -width 5 -textvariable columnPropsTMP(pMultL,$whichColumn)
    bind $w.input.pmultL <Return> "DefineGravityLoad_OK $w"
    grid $w.input.pmultL -row $row -column 1 -sticky e

    incr row

    checkbutton $w.input.all -text "Apply to all" -variable sameLoadAllColumns
    grid $w.input.all -row $row -column 0 -columnspan 3 -sticky ew

    pack $w.input -side top

    frame $w.okcancel
    
    button $w.okcancel.ok -text "OK" -command "DefineGravityLoad_OK $w"
    grid $w.okcancel.ok -row 0 -column 0
    button $w.okcancel.cancel -text "Cancel" -command "DefineGravityLoad_Cancel $w"
    grid $w.okcancel.cancel -row 0 -column 1

    pack $w.okcancel -side top
}

proc DefineGravityLoad_Cancel {w} {
    AssignTMP_columnProps

    destroy $w
}

proc DefineGravityLoad_OK {w} {
    
    set ok [CheckGravityLoad]
    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input for $ok" -icon error -parent $w
	return
    }

    Assign_columnProps

    DefineModel [getCanvas]

    destroy $w
}

proc CheckGravityLoad {} {
    global columnPropsTMP
    set N $columnPropsTMP(N)

    for {set i 1} {$i <= $N} {incr i} {
       set load $columnPropsTMP(axialLoad,$i)
       if {![isValidDouble $load]} {
          return "gravity load on column $i"
       }
       set load $columnPropsTMP(pMultT,$i)
       if {![isValidDouble $load] || $load < 0.0} {
          return "p-multiplier transverse on pile $i"
       }
       set load $columnPropsTMP(pMultL,$i)
       if {![isValidDouble $load] || $load < 0.0} {
          return "p-multiplier longitudinal on pile $i"
       }
    }
}

proc SetAxialLoad {} {
    global whichColumn

    global columnProps
    set columnProps(axialLoad,$whichColumn) $columnProps(axialLoad,0)
}
