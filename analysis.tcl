proc Assign_analysisProps {} {
   global analysisProps
   global analysisPropsTMP

   foreach var [array names analysisPropsTMP] {
      if {[info exists analysisProps($var)] && $analysisProps($var) != $analysisPropsTMP($var)} {
         ModelIsDirty
      }
      set analysisProps($var) $analysisPropsTMP($var)
   }

   Assign_viewProps
}

proc Assign_viewProps {} {
   global viewProps
   global viewPropsTMP

   # Don't want these to affect "ModelIsDirty" above
   foreach var [array names viewPropsTMP] {
      set viewProps($var) $viewPropsTMP($var)
   }
}

proc AssignTMP_analysisProps {} {
    global analysisProps
    global analysisPropsTMP

    foreach var [array names analysisProps] {
	set analysisPropsTMP($var) $analysisProps($var)
    }

    AssignTMP_viewProps
}

proc AssignTMP_viewProps {} {
   global viewProps
   global viewPropsTMP

   foreach var [array names viewProps] {
      set viewPropsTMP($var) $viewProps($var)
   }
}

set analysisProps(lateralDisp,T) 2.0
set analysisProps(lateralDisp,L) 3.0
set analysisProps(soilStructure) 1
set analysisProps(pDelta)        1
set analysisProps(postPeak)      80
set analysisProps(lateralLoad) 1 ;# 1 = distributed, 0 = concentrated
set analysisProps(overstrength)      1.0
set analysisProps(subtractWater) 1
set analysisProps(massDL) 1.0
set analysisProps(damping) 5
set analysisProps(stiffnessProp) 1 ;# 1 = last committed, 0 = initial
set analysisProps(timestep) 0.02
set analysisProps(tFinal) 30.0

set viewProps(drawD)         0
set viewProps(drawM)         1
set viewProps(drawV)         0

AssignTMP_analysisProps

set staticAnalysisWindowOpen 0
proc DefineStaticAnalysis {w} {
    global staticAnalysisWindowOpen

    # If already open, do not attempt to open a duplicate
    if {$staticAnalysisWindowOpen == 1} {return}

    # Create new window
    createTopLevel $w "Pushover Analysis Options"

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left
    set m [menu $w.mbar.file.menu]

    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"

    set text(0) "The \"Max Lateral Drift\" specifies the \% drift at which the pushover analysis will terminate."
    set text(1) "The \"Post Peak Capacity\" specifies the \% of peak capacity at which the pushover will terminate after reaching peak capacity."
    set text(2) "Analysis options for soil-structure interaction, P-Delta analysis, and subtracting the unit weight of water from that of soil below the water table can be turned on and off."

    createHelp $w $w.mbar analysis text


    set staticAnalysisWindowOpen 1
    bind $w <Destroy> {set staticAnalysisWindowOpen 0}

    AssignTMP_analysisProps

    frame $w.input

    set row 0

    label $w.input.radioLoad -text "Lateral Load"
    grid $w.input.radioLoad -row $row -column 0 -sticky e
    radiobutton $w.input.loadDist -variable analysisPropsTMP(lateralLoad) -text Distributed -value 1
    grid $w.input.loadDist -row $row -column 1
    radiobutton $w.input.loadConc -variable analysisPropsTMP(lateralLoad) -text Concentrated -value 0
    grid $w.input.loadConc -row $row -column 2

    incr row

    label $w.input.labelDrift -text "Max Displacement Transverse"
    grid $w.input.labelDrift -row $row -column 0 -sticky e
    entry $w.input.entryDrift -width 5 -textvariable analysisPropsTMP(lateralDisp,T)
    bind $w.input.entryDrift <Return> "DefineAnalysis_OK $w"
    grid $w.input.entryDrift -row $row -column 1
    label $w.input.unitDrift -text "ft"
    grid $w.input.unitDrift -row $row -column 2 -sticky w

    incr row

    label $w.input.labelDriftL -text "Max Displacement Longitudinal"
    grid $w.input.labelDriftL -row $row -column 0 -sticky e
    entry $w.input.entryDriftL -width 5 -textvariable analysisPropsTMP(lateralDisp,L)
    bind $w.input.entryDriftL <Return> "DefineAnalysis_OK $w"
    grid $w.input.entryDriftL -row $row -column 1
    label $w.input.unitDriftL -text "ft"
    grid $w.input.unitDriftL -row $row -column 2 -sticky w

    incr row

    label $w.input.labelPeak -text "Post Peak Capacity"
    grid $w.input.labelPeak -row $row -column 0 -sticky e
    entry $w.input.entryPeak -width 5 -textvariable analysisPropsTMP(postPeak)
    bind $w.input.entryPeak <Return> "DefineAnalysis_OK $w"
    grid $w.input.entryPeak -row $row -column 1
    label $w.input.unitPeak -text "\%"
    grid $w.input.unitPeak -row $row -column 2 -sticky w

    incr row





    pack $w.input -side top

    frame $w.okcancel
    
    button $w.okcancel.ok -text "OK" -command "DefineAnalysis_OK $w"
    grid $w.okcancel.ok -row 0 -column 0
    button $w.okcancel.cancel -text "Cancel" -command "DefineAnalysis_Cancel $w"
    grid $w.okcancel.cancel -row 0 -column 1

    pack $w.okcancel -side top
}

set dynamicAnalysisWindowOpen 0
proc DefineDynamicAnalysis {w} {
    global dynamicAnalysisWindowOpen

    # If already open, do not attempt to open a duplicate
    if {$dynamicAnalysisWindowOpen == 1} {return}

    # Create new window
    createTopLevel $w "Time History Analysis Options"

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left
    set m [menu $w.mbar.file.menu]

    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"

    set text(0) "The \"Max Lateral Drift\" specifies the \% drift at which the pushover analysis will terminate."
    set text(1) "The \"Post Peak Capacity\" specifies the \% of peak capacity at which the pushover will terminate after reaching peak capacity."
    set text(2) "Analysis options for soil-structure interaction, P-Delta analysis, and subtracting the unit weight of water from that of soil below the water table can be turned on and off."

    createHelp $w $w.mbar analysis text


    set dynamicAnalysisWindowOpen 1
    bind $w <Destroy> {set dynamicAnalysisWindowOpen 0}

    AssignTMP_analysisProps

    frame $w.input

    set row 0

    label $w.input.labeldt -text "Time Step"
    grid $w.input.labeldt -row $row -column 0 -sticky e
    entry $w.input.entrydt -width 5 -textvariable analysisPropsTMP(timestep)
    bind $w.input.entrydt <Return> "DefineAnalysis_OK $w"
    grid $w.input.entrydt -row $row -column 1
    label $w.input.unitdt -text "sec"
    grid $w.input.unitdt -row $row -column 2 -sticky w

    incr row

    label $w.input.labeltf -text "Time Final"
    grid $w.input.labeltf -row $row -column 0 -sticky e
    entry $w.input.entrytf -width 5 -textvariable analysisPropsTMP(tFinal)
    bind $w.input.entrytf <Return> "DefineAnalysis_OK $w"
    grid $w.input.entrytf -row $row -column 1
    label $w.input.unittf -text "sec"
    grid $w.input.unittf -row $row -column 2 -sticky w

    incr row

    label $w.input.labelMDL -text "Mass-Dead Load Multiplier"
    grid $w.input.labelMDL -row $row -column 0 -sticky e
    entry $w.input.entryMDL -width 5 -textvariable analysisPropsTMP(massDL)
    bind $w.input.entryMDL <Return> "DefineAnalysis_OK $w"
    grid $w.input.entryMDL -row $row -column 1
    label $w.input.unitMDL -text ""
    grid $w.input.unitMDL -row $row -column 2 -sticky w

    incr row

    label $w.input.labeld -text "Damping Ratio"
    grid $w.input.labeld -row $row -column 0 -sticky e
    entry $w.input.entryd -width 5 -textvariable analysisPropsTMP(damping)
    bind $w.input.entryd <Return> "DefineAnalysis_OK $w"
    grid $w.input.entryd -row $row -column 1
    label $w.input.unitd -text "\%"
    grid $w.input.unitd -row $row -column 2 -sticky w
    label $w.input.labeldamp -text "Stiffness Proportional"
    grid $w.input.labeldamp -row $row -column 3 -columnspan 2


    incr row

    #label $w.input.labeldamp -text "Stiffness Proportional"
    #grid $w.input.labeldamp -row $row -column 0 -sticky e
    radiobutton $w.input.tangent -variable analysisPropsTMP(stiffnessProp) -text Current -value 1
    grid $w.input.tangent -row $row -column 3
    radiobutton $w.input.initial -variable analysisPropsTMP(stiffnessProp) -text Initial -value 0
    grid $w.input.initial -row $row -column 4

    pack $w.input -side top



    frame $w.okcancel
    
    button $w.okcancel.ok -text "OK" -command "DefineAnalysis_OK $w"
    grid $w.okcancel.ok -row 0 -column 0
    button $w.okcancel.cancel -text "Cancel" -command "DefineAnalysis_Cancel $w"
    grid $w.okcancel.cancel -row 0 -column 1

    pack $w.okcancel -side top
}

set modelingWindowOpen 0
proc DefineModelingOptions {w} {
    global modelingWindowOpen

    # If already open, do not attempt to open a duplicate
    if {$modelingWindowOpen == 1} {return}

    # Create new window
    createTopLevel $w "Modeling Options"

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left
    set m [menu $w.mbar.file.menu]

    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"

    set text(0) "The \"Max Lateral Drift\" specifies the \% drift at which the pushover analysis will terminate."
    set text(1) "The \"Post Peak Capacity\" specifies the \% of peak capacity at which the pushover will terminate after reaching peak capacity."
    set text(2) "Analysis options for soil-structure interaction, P-Delta analysis, and subtracting the unit weight of water from that of soil below the water table can be turned on and off."

    createHelp $w $w.mbar analysis text


    set modelingWindowOpen 1
    bind $w <Destroy> {set modelingWindowOpen 0}

    AssignTMP_analysisProps

    frame $w.input

    set row 0

    label $w.input.labelOver -text "Overstrength Factor"
    grid $w.input.labelOver -row $row -column 0 -sticky e
    entry $w.input.entryOver -width 5 -textvariable analysisPropsTMP(overstrength)
    bind $w.input.entryOver <Return> "DefineAnalysis_OK $w"
    grid $w.input.entryOver -row $row -column 1
    label $w.input.unitOver -text ""
    grid $w.input.unitOver -row $row -column 2 -sticky w

    incr row

    label $w.input.radioSSI -text "Soil-Structure Interaction"
    grid $w.input.radioSSI -row $row -column 0 -sticky e
    radiobutton $w.input.ssiOn -variable analysisPropsTMP(soilStructure) -text On -value 1
    grid $w.input.ssiOn -row $row -column 1
    radiobutton $w.input.ssiOff -variable analysisPropsTMP(soilStructure) -text Off -value 0
    grid $w.input.ssiOff -row $row -column 2

    incr row

    label $w.input.radioPD -text "P-Delta Analysis"
    grid $w.input.radioPD -row $row -column 0 -sticky e
    radiobutton $w.input.pdOn -variable analysisPropsTMP(pDelta) -text On -value 1
    grid $w.input.pdOn -row $row -column 1
    radiobutton $w.input.pdOff -variable analysisPropsTMP(pDelta) -text Off -value 0
    grid $w.input.pdOff -row $row -column 2

    incr row

    label $w.input.radioWD -text "Soil Buoyant Weight below GWT"
    grid $w.input.radioWD -row $row -column 0 -sticky e
    radiobutton $w.input.wdOn -variable analysisPropsTMP(subtractWater) -text On -value 1
    grid $w.input.wdOn -row $row -column 1
    radiobutton $w.input.wdOff -variable analysisPropsTMP(subtractWater) -text Off -value 0
    grid $w.input.wdOff -row $row -column 2

    incr row

    label $w.input.radioDD -text "Draw Displaced Shape"
    grid $w.input.radioDD -row $row -column 0 -sticky e
    radiobutton $w.input.ddOn -variable viewPropsTMP(drawD) -text On -value 1
    grid $w.input.ddOn -row $row -column 1
    radiobutton $w.input.ddOff -variable viewPropsTMP(drawD) -text Off -value 0
    grid $w.input.ddOff -row $row -column 2

    incr row

    label $w.input.radioMD -text "Draw Moment Diagram"
    grid $w.input.radioMD -row $row -column 0 -sticky e
    radiobutton $w.input.mdOn -variable viewPropsTMP(drawM) -text On -value 1
    grid $w.input.mdOn -row $row -column 1
    radiobutton $w.input.mdOff -variable viewPropsTMP(drawM) -text Off -value 0
    grid $w.input.mdOff -row $row -column 2

    incr row

    label $w.input.radioVD -text "Draw Shear Diagram"
    grid $w.input.radioVD -row $row -column 0 -sticky e
    radiobutton $w.input.vdOn -variable viewPropsTMP(drawV) -text On -value 1
    grid $w.input.vdOn -row $row -column 1
    radiobutton $w.input.vdOff -variable viewPropsTMP(drawV) -text Off -value 0
    grid $w.input.vdOff -row $row -column 2

    pack $w.input -side top

    frame $w.okcancel
    
    button $w.okcancel.ok -text "OK" -command "DefineAnalysis_OK $w"
    grid $w.okcancel.ok -row 0 -column 0
    button $w.okcancel.cancel -text "Cancel" -command "DefineAnalysis_Cancel $w"
    grid $w.okcancel.cancel -row 0 -column 1

    pack $w.okcancel -side top
}

proc DefineAnalysis_Cancel {w} {
    AssignTMP_analysisProps

    destroy $w
}

proc DefineAnalysis_OK {w} {
    
    set ok [CheckAnalysis]
    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input for $ok" -icon error -parent $w
	return
    }

    Assign_analysisProps

    DefineModel [getCanvas]

    destroy $w
}

proc CheckAnalysis {} {
    global analysisPropsTMP

    set bcap $analysisPropsTMP(lateralDisp,T)
    if {![isValidDouble $bcap] || $bcap <= 0.0} {
	return "maximum displacement transverse"
    }

    set bcap $analysisPropsTMP(lateralDisp,L)
    if {![isValidDouble $bcap] || $bcap <= 0.0} {
	return "maximum displacement longitudinal"
    }

    set dcap $analysisPropsTMP(postPeak)
    if {![isValidDouble $dcap] || $dcap < 0.0 || $dcap > 100.0} {
	return "post peak capacity"
    }

    set dcap $analysisPropsTMP(damping)
    if {![isValidDouble $dcap] || $dcap < 0.0 || $dcap > 100.0} {
	return "damping ratio"
    }

    set dcap $analysisPropsTMP(overstrength)
    if {![isValidDouble $dcap] || $dcap <= 0.0} {
	return "overstrength factor"
    }

    set dcap $analysisPropsTMP(massDL)
    if {![isValidDouble $dcap] || $dcap < 0.0} {
	return "mass-dead load multiplier"
    }

    set dcap $analysisPropsTMP(timestep)
    if {![isValidDouble $dcap] || $dcap <= 0.0} {
	return "time step"
    }

    set bcap $analysisPropsTMP(tFinal)
    if {![isValidDouble $bcap] || $bcap < 0.0} {
	return "time final"
    }
}

