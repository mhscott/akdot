set tcl_precision 12

# Working directory where analysis files are located
set wd [pwd]

# Directory variable required for pdf4tcl/pkgIndex.tcl
set dir .

lappend auto_path $dir
lappend auto_path $wd

if {1 || $tcl_platform(platform) == "unix" || $tcl_platform(platform) == "linux"} {
    source /nfs/depot/cce_u1/scott/mhscott/snit1.0/snit.tcl
    source /nfs/depot/cce_u1/scott/mhscott/snit1.0/pkgIndex.tcl
    #source ./snit1.0/snit.tcl
    #source ./snit1.0/pkgIndex.tcl
    
    source /nfs/depot/cce_u1/scott/mhscott/pdf4tcl05/glyphnames.tcl
    source /nfs/depot/cce_u1/scott/mhscott/pdf4tcl05/metrics.tcl
    source /nfs/depot/cce_u1/scott/mhscott/pdf4tcl05/pdf4tcl.tcl
    source /nfs/depot/cce_u1/scott/mhscott/pdf4tcl05/pkgIndex.tcl
    #source ./pdf4tcl05/glyphnames.tcl
    #source ./pdf4tcl05/metrics.tcl
    #source ./pdf4tcl05/pdf4tcl.tcl
    #source ./pdf4tcl05/pkgIndex.tcl

    source /nfs/depot/cce_u1/scott/mhscott/emu-graph1.1.2/tcl/graph.tcl
    #source ./emu-graph1.1.2/tcl/graph.tcl
} else {
    #source m:\\snit1.0\\snit.tcl
    #source m:\\snit1.0\\pkgIndex.tcl
    
    #source m:\\pdf4tcl05\\glyphnames.tcl
    #source m:\\pdf4tcl05\\metrics.tcl
    #source m:\\pdf4tcl05\\pdf4tcl.tcl
    #source m:\\pdf4tcl05\\pkgIndex.tcl

    #source m:\\emu-graph1.1.2\\tcl\\graph.tcl
}

if {$tcl_platform(platform) == "unix" || $tcl_platform(platform) == "linux"} {
    #source /nfs/depot/cce_u1/scott/mhscott/emu-graph1.1.2/tcl/graph.tcl
    #source /nfs/depot/cce_u1/scott/mhscott/tclUtility/rgb.tcl
    #source /nfs/depot/cce_u1/scott/mhscott/tclUtility/comb.tcl
    #source /nfs/depot/cce_u1/scott/mhscott/tclUtility/shellTri.tcl
} else  {
    #source m:\\emu-graph1.1.2\\tcl\\graph.tcl
    #source m:\\tclUtility\\rgb.tcl
    #source m:\\tclUtility\\comb.tcl
    #source m:\\tclUtility\\shellTri.tcl
}

package require pdf4tcl 0.5


# Start MHS Code

set toggleFrame .fBent

proc SoftwareTitle {} {
   return "Alaska DOT&PF Pile Extension Pier Pushover Program"
}

proc VersionNumber {} {
   return "2.0"
}

#####

proc ModelIsDirty {} {
    global modelChangedSinceSave
    set modelChangedSinceSave 1
}

proc IsModelDirty {} {
    global modelChangedSinceSave
    return $modelChangedSinceSave
}

proc ModelIsClean {} {
    global modelChangedSinceSave
    set modelChangedSinceSave 0
}

ModelIsClean

#####

proc SectionIsDirty {} {
    global sectionChangedSinceSave
    set sectionChangedSinceSave 1
}

proc IsSectionDirty {} {
    global sectionChangedSinceSave
    return $sectionChangedSinceSave
}

proc SectionIsClean {} {
    global sectionChangedSinceSave
    set sectionChangedSinceSave 0
}

SectionIsClean

#####

# Window title
wm title . "[SoftwareTitle], v[VersionNumber]"

# Set to a random name that can be overwritten
set bridgeName "Bridge[expr abs([clock clicks])]"
set engineerName "John Doe, P.E."

source colors.tcl

set units(ft) 1.0
set units(kip) 1.0
set units(sec) 1.0
set units(lb) [expr $units(kip)/1000.0]
set units(in) [expr $units(ft)/12.0]
set units(ksi) [expr $units(kip)/pow($units(in),2)]
set units(psi) [expr $units(lb)/pow($units(in),2)]
set units(pci) [expr $units(lb)/pow($units(in),3)]
set units(pcf) [expr $units(lb)/pow($units(ft),3)]
set units(rad) 1.0
set units(deg) [expr 2*asin(1.0)/180.0]

proc isValidDouble {value} {
   return [expr [string is double $value] && [string length $value] > 0]
}

proc isValidInt {value} {
   return [expr [string is integer $value] && [string length $value] > 0]
}

set whichSection 1

source ./analysis.tcl

source ./section.tcl

source ./limitStates.tcl

source ./rfsteel.tcl

source ./jacket.tcl

source ./concrete.tcl

source ./column.tcl

source ./hinge.tcl

source ./bentcap.tcl

source ./soil.tcl

source ./groundmotion.tcl

source ./report.tcl

proc createHelp {w menubar label text} {

    upvar $text thisText

    set f [frame $w.help]

    #set t [text $f.t -width 42 -height 4 -wrap word -yscrollcommand "$f.sbar set"]
    #scrollbar $f.sbar -orient vertical -command "$f.t yview"
    #pack $f.sbar -side right -fill y
    #pack $f.t -side left -fill both -expand true

    menubutton $menubar.help -text Help -menu $menubar.help.menu
    pack $menubar.help -side left

    set m [menu $menubar.help.menu]
    $m add command -label "Show/Hide (Ctrl+H)" -command "AboutHelp $f $label"
    bind $w <Control-Key-h> "AboutHelp $w.help $label"

    global toggleHelp
    set toggleHelp($label) 0

    set width [winfo reqwidth $w]
    set Ntext [llength [array names thisText]]
    for {set i 0} {$i < $Ntext} {incr i} {
	message $f.text$i -text $thisText($i) -justify center -width $width
	grid $f.text$i -row $i -column 0	
	#$t insert end "$thisText($i) \n\n"
    }
    return $f
}

proc AboutHelp {f what {side top}} {
    global toggleHelp

    if {$toggleHelp($what) == 0} {
	pack $f -side $side
    } else {
	pack forget $f
    }

    set toggleHelp($what) [expr !$toggleHelp($what)]
}



set propsList    "columnProps    hingeProps    concreteProps    rfsteelProps    jacketProps    sectionProps    soilProps    bentcapProps    limitStates    analysisProps    viewProps"
set propsListTMP "columnPropsTMP hingePropsTMP concretePropsTMP rfsteelPropsTMP jacketPropsTMP sectionPropsTMP soilPropsTMP bentcapPropsTMP limitStatesTMP analysisPropsTMP viewPropsTMP"


proc getCanvas {} {
   return .fBent.canvas
}

proc createTopLevel {w title} {
    toplevel $w
    wm title $w $title
    wm minsize $w 200 100
    focus $w
    #catch {tkwait visibility $w}
    catch {grab $w}
}

proc createOKCancelFrame {w assignProc} {
    pack forget $w.okcancel

    frame $w.okcancel
    
    button $w.okcancel.ok -text "OK" -command "$assignProc; DefineModel [getCanvas]; destroy $w"
    grid $w.okcancel.ok -row 0 -column 0
    button $w.okcancel.cancel -text "Cancel" -command "destroy $w"
    grid $w.okcancel.cancel -row 0 -column 1

    pack $w.okcancel -side top    
}

proc createEMUGraph {w name {width 600} {height 600}} {
    pack forget $w.plot
    destroy $w.plot

    frame $w.plot
    canvas $w.plot.canvas -height $height -width $width
    grid $w.plot.canvas -row 0 -column 1;# -sticky nsew
    pack $w.plot -side left

    $w.plot.canvas delete all

    emu_graph::emu_graph $name -canvas $w.plot.canvas -width [expr 0.8*$width] -height [expr 0.9*$height]

    return $w.plot.canvas
}

proc axesEMUGraph {graph xMax yMax {xMin 0} {yMin 0} {nx 5} {ny 5}} {

   $graph configure -autorange 0

   set dx [niceTickMarks $xMin $xMax $nx]
   if {$xMin != 0.0} {
      set xMin [expr $dx*(int($xMin/$dx) - 1)]
   }
   if {$xMax != 0.0} {
      set xMax [expr $dx*(int($xMax/$dx) + 1)]
   }

   set dy [niceTickMarks $yMin $yMax $ny]
   if {$yMin != 0.0} {
      set yMin [expr $dy*(int($yMin/$dy) - 1)]
   }
   if {$yMax != 0.0} {
      set yMax [expr $dy*(int($yMax/$dy) + 1)]
   }

   $graph configure -xmin $xMin
   $graph configure -xmax $xMax
   $graph configure -nticks_x $nx

   $graph configure -ymin $yMin
   $graph configure -ymax $yMax
   $graph configure -nticks_y $ny
}

proc niceTickMarks {xMin xMax nx} {

   # Calculate nice tick marks for axis
   set range [expr double($xMax-$xMin)]
   set tempStep [expr $range/$nx] ;# Initial guess
   set mag [expr int(log10($tempStep))] ;# Magnitude of step size
   set magPow [expr pow(10,$mag)]
   set magMsd [expr int($tempStep/$magPow + 0.5)] ;# Most signif. digit

   # Promote msd to 2, 5, or 10
   if {$magMsd > 5.0} {
      set magMsd 10.0
   } elseif {$magMsd > 2.0} {
      set magMsd 5.0
   } elseif {$magMsd > 1.0} {
      set magMsd 2.0
   } else {
      set magMsd 1.0
   }

   return [expr $magMsd*$magPow]
}

proc labelEMUGraph {c graph graphInfo width height} {

    upvar $graphInfo thisGraphInfo

    $c create text [expr $width/2] 0 -text $thisGraphInfo(title) -anchor n
    $c create text [expr $width/2] $height -text $thisGraphInfo(xlabel) -anchor s
#    $c create text [$graph x2canvas 0] [expr $height-[$graph y2canvas 0]] -text "$thisGraphInfo(ylabel) " -anchor s
    $c create text [$graph x2canvas 0] 0 -text "$thisGraphInfo(ylabel)" -anchor n

    set bbox [$graph bbox]
    $c move all [expr -[lindex $bbox 0]+5] 0.0
}

proc clearCanvas {c {tag all}} {
    foreach id [$c find withtag $tag] {
	$c delete $id 
    }
}

proc DateTime {} {
    #return ""
    set systemTime [clock seconds]

    # This doesn't work on Windows -- MHS
    return "[clock format $systemTime -format %D] [clock format $systemTime -format %H:%M:%S]"
}

proc EngineerName {} {
    global engineerName

    return $engineerName
}

proc BridgeName {} {
    global bridgeName

    return $bridgeName
}

proc PrintCanvas {c w} {
    global PRINTtypelist
    global wd

    set openFile [tk_getSaveFile -defaultextension .ps -filetypes $PRINTtypelist -initialdir $wd -parent $w]

    if {[llength $openFile] <= 0} {
	return
    }

    set wd [file dirname $openFile]

    $c postscript -file $openFile
}

proc ExportData {l w} {
    global DATAtypelist
    global wd

    set openFile [tk_getSaveFile -defaultextension .csv -filetypes $DATAtypelist -initialdir $wd -parent $w]

    if {[llength $openFile] <= 0} {
	return
    }

    set wd [file dirname $openFile]

    set outputStream [open $openFile w]

    foreach {x y} $l {
	puts $outputStream "[format %+.3e $x],[format %+.3e $y]"
    }

    close $outputStream
}


# Create an application menu
##########
frame .mbar -borderwidth 1 -relief raised
pack .mbar -fill x

# The valid file types to display in the file dialog windows
set CISStypelist {
    {"CISS Analysis Files" ".cis"}
    {"CISS Analysis Files" ".CIS"}
    {"All Files" {*} }
}

set PRINTtypelist {
    {"Postscript" ".ps"}
    {"Postscript" ".PS"}
    {"All Files" {*} }
}

set PDFtypelist {
    {"Acrobat" ".pdf"}
    {"Acrobat" ".PDF"}
    {"All Files" {*} }
}

set DATAtypelist {
    {"CSV" ".csv"}
    {"CSV" ".CSV"}
    {"All Files" {*} }
}

set EQtypelist {
    {"CSV" ".csv"}
    {"CSV" ".CSV"}
    {"Plain Text" ".txt"}
    {"Plain Text" ".TXT"}
    {"All Files" {*} }
}

menubutton .mbar.file -text File -menu .mbar.file.menu
pack .mbar.file -side left
set m [menu .mbar.file.menu]
$m add command -label "Open (Ctrl+O)" -command "OpenFile"
bind . <Control-Key-o> {OpenFile}
$m add command -label "Save (Ctrl+S)" -command "SaveFile"
bind . <Control-Key-s> {SaveFile}
$m add command -label "Print (Ctrl+P)" -command "PrintCanvas [getCanvas] .fBent"
bind . <Control-Key-p> {PrintCanvas [getCanvas] .fBent}
$m add command -label "Working Directory (Ctrl+D)" -command "WorkingDir"
bind . <Control-Key-d> {WorkingDir}
$m add separator
$m add command -label "Exit (Ctrl+Q)" -command exit
bind . <Control-Key-q> {exit}

menubutton .mbar.edit -text Edit -menu .mbar.edit.menu
pack .mbar.edit -side left
set m [menu .mbar.edit.menu]
#$m add command -label "Section" -command "DefineSection .section"
$m add cascade -label "Section" -menu $m.sections
set m2 [menu $m.sections]
$m2 add command -label "Column" -command "set whichSection 1; DefineSection .section"
$m2 add command -label "Pile" -command "set whichSection 2; DefineSection .section"

$m add command -label "Gap Region" -command "DefineHinge .hinge"
$m add command -label "Bent Cap" -command "DefineBentcap .bentcap"

$m add cascade -label "Column/Pile Loads" -menu $m.gravity
set m2 [menu $m.gravity]
set N $columnPropsTMP(N)
for {set i 1} {$i <= $N} {incr i} {
    $m2 add command -label "Column/Pile $i" -command "set whichColumn $i; DefineGravityLoad .gravity"
}


$m add cascade -label "Soil" -menu $m.layers
set m2 [menu $m.layers]
$m2 add command -label "Water Table" -command "DefineWaterTable .watertable"
#$m2 add command -label "Frozen Soil" -command "DefineFrozenSoil .frozensoil"
set Nlayers $soilPropsTMP(Nlayers)
for {set i 1} {$i <= $Nlayers} {incr i} {
    $m2 add command -label "Layer $i" -command "set whichLayer $i; DefineSoil .soil"
}

$m add command -label "Earthquake Motion" -command "DefineEQMotion .eqmotion"

$m add separator

$m add command -label "Limit States" -command "DefineLimitStates .limitstates"
#$m add command -label "Analysis Options" -command "DefineAnalysis .analysis"
$m add cascade -label "Analysis Options" -menu $m.analysis
set m2 [menu $m.analysis]
$m2 add command -label "Modeling Options" -command "DefineModelingOptions .modeling"
$m2 add command -label "Pushover Analysis" -command "DefineStaticAnalysis .analysis"
$m2 add command -label "Time History Analysis" -command "DefineDynamicAnalysis .analysis"

$m add separator

$m add cascade -label Color -menu $m.color
set m2 [menu $m.color]
$m2 add command -label Steel -command "ChooseColor colors(steel); DefineModel [getCanvas]"
$m2 add command -label Concrete -command "ChooseColor colors(concrete); DefineModel [getCanvas]"
$m2 add command -label Soil -command "ChooseColor colors(ground); DefineModel [getCanvas]"
$m2 add command -label Pile -command "ChooseColor colors(pile); DefineModel [getCanvas]"
$m2 add separator
$m2 add command -label "Reset Defaults" -command "RestoreDefaultColors; DefineModel [getCanvas]"

proc OpenFile {} {
    global CISStypelist
    global wd

    set openFile [tk_getOpenFile -filetypes $CISStypelist -initialdir $wd]

    if {[llength $openFile] <= 0} {
	return
    }

    set wd [file dirname $openFile]

    global bridgeName
    global engineerName

    global propsList
    foreach props "$propsList colors units" {
	global $props
    }

    global propsListTMP
    foreach props $propsListTMP {
	global $props
    }

    source $openFile

    ChangeSoilLayerMenu .mbar.edit.menu.layers
    ChangeGravityLoadMenu .mbar.edit.menu.gravity

    foreach props $propsList {
	Assign_$props
    }

    #ChangeSoilLayers
    ChangeColumns

    DefineModel [getCanvas]

    ModelIsClean

    SectionIsClean
}

proc SaveFile {} {
    global CISStypelist

    global wd

    set saveFile [tk_getSaveFile -defaultextension .cis -filetypes $CISStypelist -initialdir $wd -initialfile [BridgeName]]

    if {[llength $saveFile] <= 0} {
	return
    }

    set wd [file dirname $saveFile]

    global propsList

    foreach props "$propsList colors units" {
	global $props
    }

    set outFile [open $saveFile w]

    puts $outFile "set bridgeName \"[BridgeName]\""
    puts $outFile "set engineerName \"[EngineerName]\""
    puts $outFile "set versionNumber \"[VersionNumber]\""

    foreach var [array names sectionProps] {
	puts $outFile "set sectionPropsTMP($var) \"$sectionProps($var)\""
    }
    foreach var [lsort [array names soilProps]] {
	puts $outFile "set soilPropsTMP($var) \"$soilProps($var)\""
    }
    foreach var [lsort [array names columnProps]] {
	puts $outFile "set columnPropsTMP($var) \"$columnProps($var)\""
    }
    foreach var [array names hingeProps] {
	puts $outFile "set hingePropsTMP($var) \"$hingeProps($var)\""
    }
    foreach var [array names concreteProps] {
	puts $outFile "set concretePropsTMP($var) \"$concreteProps($var)\""
    }
    foreach var [array names rfsteelProps] {
	puts $outFile "set rfsteelPropsTMP($var) \"$rfsteelProps($var)\""
    }
    foreach var [array names jacketProps] {
	puts $outFile "set jacketPropsTMP($var) \"$jacketProps($var)\""
    }
    foreach var [array names bentcapProps] {
	puts $outFile "set bentcapPropsTMP($var) \"$bentcapProps($var)\""
    }
    foreach var [array names limitStates] {
	puts $outFile "set limitStatesTMP($var) \"$limitStates($var)\""
    }
    foreach var [array names analysisProps] {
	puts $outFile "set analysisPropsTMP($var) \"$analysisProps($var)\""
    }
    foreach var [array names viewProps] {
	puts $outFile "set viewPropsTMP($var) \"$viewProps($var)\""
    }
    foreach var [array names colors] {
	puts $outFile "set colors($var) \"$colors($var)\""
    }
    foreach var [array names units] {
	puts $outFile "set units($var) \"$units($var)\""
    }



#    foreach props $propsList {
#    	foreach var [array names $props] {
#	    puts $outFile "set $props($var) \"$$props($var)\""
#	}
#    }

    close $outFile

    ModelIsClean

    SectionIsClean
}

proc WorkingDir {} {
    global CISStypelist
    global wd

    set dir [tk_chooseDirectory -initialdir $wd]

    if {[llength $dir] <= 0} {
	return
    }

    set wd $dir
}

##########

# Create a canvas to view the bridge bent
##########
frame .fBent
canvas [getCanvas] -height 600 -width 600
grid [getCanvas] -row 0 -column 0
##########

# Create a frame to enter column information
##########
frame .fColInfo -borderwidth 1 -relief raised

label .fColInfo.bname -text "Bridge"
grid .fColInfo.bname -row 0 -column 0 -sticky e
entry .fColInfo.bentry -width 30 -textvariable bridgeName
grid .fColInfo.bentry -row 0 -column 1 -columnspan 3 -sticky w

label .fColInfo.ename -text "Engineer"
grid .fColInfo.ename -row 1 -column 0 -sticky e
entry .fColInfo.eentry -width 30 -textvariable engineerName
grid .fColInfo.eentry -row 1 -column 1 -columnspan 3 -sticky w

label .fColInfo.labelNcol -text "Number of Columns"
grid .fColInfo.labelNcol -row 2 -column 0 -sticky e
entry .fColInfo.entryNcol -width 5 -textvariable columnPropsTMP(N)
bind .fColInfo.entryNcol <Return> "DefineModel [getCanvas]"
grid .fColInfo.entryNcol -row 2 -column 1

label .fColInfo.labelHcol -text "Above Grade Height (ft)"
grid .fColInfo.labelHcol -row 3 -column 0 -sticky e
entry .fColInfo.entryHcol -width 5 -textvariable columnPropsTMP(L)
bind .fColInfo.entryHcol <Return> "DefineModel [getCanvas]"
grid .fColInfo.entryHcol -row 3 -column 1

label .fColInfo.labelNlay -text "Number of Soil Layers"
grid .fColInfo.labelNlay -row 2 -column 2  -sticky e
entry .fColInfo.entryNlay -width 5 -textvariable soilPropsTMP(Nlayers)
bind .fColInfo.entryNlay <Return> "DefineModel [getCanvas]"
grid .fColInfo.entryNlay -row 2 -column 3

label .fColInfo.labelScol -text "Below Grade Depth (ft)"
grid .fColInfo.labelScol -row 3 -column 2  -sticky e
entry .fColInfo.entryScol -width 5 -textvariable soilPropsTMP(subgradeHeight)
bind .fColInfo.entryScol <Return> "DefineModel [getCanvas]"
grid .fColInfo.entryScol -row 3 -column 3

pack .fColInfo -side top

proc CheckMainInput {} {

   global columnPropsTMP

   set N $columnPropsTMP(N)
   if {![isValidInt $N] || $N <= 0} {
	return "number of columns"
   }

   set L $columnPropsTMP(L)
   if {![isValidDouble $L] || $L <= 0} {
	return "above grade height"
   }
   
   global soilPropsTMP

   set Nlayers $soilPropsTMP(Nlayers)
   if {![isValidInt $Nlayers] || $Nlayers <= 0} {
	return "number of soil layers"
   }

   set sgh $soilPropsTMP(subgradeHeight)
   if {![isValidDouble $sgh] || $sgh <= 0.0} {
	return "below grade height"
   }

}

proc DrawBent_Pressed {w} {
    set ok [CheckMainInput]
    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input for $ok" -icon error -parent $w
	return
    }

    DefineModel [getCanvas]
}

proc Analyze_Pressed {w {TorL T}} {
    set ok [CheckMainInput]
    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input for $ok" -icon error -parent $w
	return
    }

    #destroy .analysis

    Analyze .analysis $TorL
}

proc Dynamic_Pressed {w} {
    set ok [CheckMainInput]
    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input for $ok" -icon error -parent $w
	return
    }

    global eqProps
    if {$eqProps(filename) == "None Selected"} {
	tk_messageBox -type ok -title "Error" -message "No ground motion has been specified" -icon error -parent $w

	DefineEQMotion .eqmotion
	LoadEQMotion .eqmotion
	
	return
    }

    destroy .dynamic

    AnalyzeDynamic .dynamic
}

proc CreateReport_Pressed {w} {
    set ok [CheckMainInput]
    if {[string length $ok] > 0} {
	tk_messageBox -type ok -title "Error" -message "Invalid input for $ok" -icon error -parent $w
	return
    }

    CreateReport .fBent.report
}


frame .fReport -borderwidth 1 -relief raised

button .fReport.button -text "Draw Bent" -command "DrawBent_Pressed ."
grid .fReport.button -row 0 -column 0 -columnspan 1 -sticky e
button .fReport.analyze -text "Transverse Pushover" -command "Analyze_Pressed . T"
grid .fReport.analyze -row 0 -column 1 -columnspan 1 -sticky w
button .fReport.analyzeL -text "Longitudinal Pushover" -command "Analyze_Pressed . L"
grid .fReport.analyzeL -row 0 -column 2 -columnspan 1 -sticky w
button .fReport.dynamic -text "Dynamic Analysis" -command "Dynamic_Pressed ."
grid .fReport.dynamic -row 0 -column 3 -columnspan 1 -sticky w
button .fReport.report -text "Create Report" -command "CreateReport_Pressed ."
grid .fReport.report -row 0 -column 4 -columnspan 1 -sticky ew

pack .fReport -side top



menubutton .mbar.help -text Help -menu .mbar.help.menu
pack .mbar.help -side left

set m [menu .mbar.help.menu]
$m add command -label "Show/Hide (Ctrl+H)" -command "DefineModel [getCanvas]; mainHelp .help"
bind . <Control-Key-h> "DefineModel [getCanvas]; mainHelp .help"



set toggleHelp(main) 0
proc mainHelp {w} {
    global toggleHelp
    set toggleHelp(main) [expr !$toggleHelp(main)]

    destroy $w

    if {$toggleHelp(main) == 1} {
	toplevel $w
	wm title $w "Help"
	wm minsize $w 200 100

	frame $w.fHelp
	set width [winfo reqwidth $w]
	
	set text(0) "The \"Bridge Name\" and \"Engineer\" are randomly generated, but can be overwritten, and will be displayed in all data plots."
	set text(1) "Bridge and soil properties can be edited by either double-clicking objects or selecting from the Edit menu."
	set text(2) "\"Draw Bent\" will cause the system to be re-drawn with the current input values."
	set text(3) "\"Transverse Pushover\" will start a transverse pushover analysis and display a plot of the transverse load-displacement response."
	set text(4) "\"Long. Pushover\" will start a longitudinal pushover analysis and display a plot of the longitudinal load-displacement response."
	set text(5) "\"Dynamic Analysis\" will start a transverse time history analysis and display a plot of the transverse displacement time history response."
	set text(6) "\"Create Report\" will create a PDF file summarizing all bent and soil properties and analyses."
	set Ntext [llength [array names text]]
	for {set i 0} {$i < $Ntext} {incr i} {
	    message $w.fHelp.text$i -text $text($i) -justify center -width $width
	    grid $w.fHelp.text$i -row $i -column 0
	    #$helpText insert end "$text($i)\n\n"
	}
	pack $w.fHelp
    }
}

proc DefineModel {c {color on} {TorL T}} {
    global toggleFrame
    global colors

    global units
    foreach var [array names units] {
	set $var $units($var)
    }

    global hingePropsTMP
    set Gf $hingePropsTMP(Gf)
    set lp $hingePropsTMP(lp)

    global bentcapPropsTMP
    set bcap [expr {$bentcapPropsTMP(bcap)*$in}]
    set dcap [expr {$bentcapPropsTMP(dcap)*$in}]

    
    global soilProps
    global soilPropsTMP
    # If number of layers has changed, re-calc depths
    if {$soilProps(Nlayers) != $soilPropsTMP(Nlayers) || 
	$soilProps(subgradeHeight) != $soilPropsTMP(subgradeHeight)} {
	ChangeSoilLayers
	ChangeSoilLayerMenu .mbar.edit.menu.layers
    }

    Assign_soilProps
    
    set subgradeHeight $soilProps(subgradeHeight)
    set waterTableDepth $soilProps(waterTableDepth)
    set frozenSoilDepth $soilProps(frozenSoilDepth)
    set Nlayers $soilProps(Nlayers)
    if {$Nlayers <= 0} {
	return
    }

    
    global columnProps
    global columnPropsTMP
    # If number of columns has changed, assign new values
    if {$columnProps(N) != $columnPropsTMP(N)} {
	ChangeColumns
	ChangeGravityLoadMenu .mbar.edit.menu.gravity
    }
    
    Assign_columnProps
    
    set L $columnProps(L)
    set N $columnProps(N)
    if {$N <= 0} {
	return
    }



    clearCanvas $c

    pack forget $toggleFrame
    pack $toggleFrame -side bottom


    set width [winfo reqwidth $c]
    set height [winfo reqheight $c]

    set arrowLength [expr {5*$ft}]
    set totalBentHeight [expr {$subgradeHeight + $L + $dcap + $arrowLength}]

    global pixelPerFeet
    set pixelPerFeet(Y) [expr {$height/$totalBentHeight}]

    global sectionPropsTMP
    set Djc [expr {$sectionPropsTMP(Dj,1)*$in}]
    set tjc [expr {$sectionPropsTMP(tj,1)*$in}]
    set Dc [expr {$Djc-2*$tjc}]
    set Dc2 [expr {0.5*$Dc}]
    set Djc2 [expr {0.5*$Djc}]

    set Djp [expr {$sectionPropsTMP(Dj,2)*$in}]
    set tjp [expr {$sectionPropsTMP(tj,2)*$in}]
    set Dp [expr {$Djp-2*$tjp}]
    set Dp2 [expr {0.5*$Dp}]
    set Djp2 [expr {0.5*$Djp}]

    set bentCapOverhang [expr {1.0*$ft}]
    set capoverhangL [expr {$bentcapPropsTMP(leftoverhang)*$in}]
    set capoverhangR [expr {$bentcapPropsTMP(rightoverhang)*$in}]

    set columnWidth 0.0
    for {set i 1} {$i <= $N} {incr i} {
	set columnWidth [expr {$columnWidth + $columnProps(space,$i)}]
    }
    set totalBentWidth [expr {$columnWidth + $Djc + $capoverhangL + $capoverhangR}]
    set viewWidth [expr {$totalBentWidth+0.2*$L}] 
    set pixelPerFeet(X) [expr {$width/$viewWidth}]


    #set x [expr {0.5*($totalBentHeight-$totalBentWidth)}]
    set x [expr {$Djc2 + $capoverhangL + 0.5*0.2*$L}]
    #set x 0
    set x0 $x

    set totalSoilDepth 0.0
    for {set i 1} {$i <= $soilProps(Nlayers)} {incr i} {
	set totalSoilDepth [expr {$totalSoilDepth + $soilProps(depth,$i)}]
    }

    set concreteColor $colors(concrete)
    set steelColor $colors(steel)
    set groundColor $colors(ground)
    set hollowPileColor $colors(pile)
    if {$color != "on"} {
        set concreteColor white
        set steelColor white
        set groundColor white
        set hollowPileColor white
    }

    # Draw columns
    for {set i 1} {$i <= $N} {incr i} {
	set x [expr {$x + $columnProps(space,$i)}]

	if {$i > 1} {
	    $c create text [expr {$x - 0.5*$columnProps(space,$i)}] [expr {$subgradeHeight+0.5*$L}] -text "$columnProps(space,$i) ft o.c." -tag spaceText -anchor s
	}

	set totalHeight [expr {$subgradeHeight+$L}]
	set depthPile $sectionPropsTMP(changePile)

	# Column
	$c create rectangle [expr {$x-0.9*$Dc2}] [expr {$subgradeHeight-$depthPile}] [expr {$x+0.9*$Dc2}] $totalHeight -fill $concreteColor -tag column
	if {$tjc > 0.0} {
	    if {$sectionPropsTMP(noConcrete,1) == 0} {
		$c create rectangle [expr {$x-$Djc2}] [expr {$subgradeHeight-$depthPile}] [expr {$x+$Djc2}] [expr {$totalHeight-$Gf*$in}] -fill $steelColor -tag column
	    } else {
		$c create rectangle [expr {$x-$Djc2}] [expr {$subgradeHeight-$depthPile}] [expr {$x+$Djc2}] [expr {$totalHeight-$Gf*$in}] -fill $hollowPileColor -tag column
	    }
	}
	$c create text $x [expr {$subgradeHeight+0.5*($L-$depthPile)}] -text "Column $i" -anchor s -tag columnText

	# Pile
	if {$tjp > 0.0} {
            if {$sectionPropsTMP(noConcrete,2) == 0} {
		$c create rectangle [expr {$x-$Djp2}] 0 [expr {$x+$Djp2}] [expr {$subgradeHeight-$depthPile}] -fill $steelColor -tag pile
            } else {
		$c create rectangle [expr {$x-$Djp2}] 0 [expr {$x+$Djp2}] [expr {$subgradeHeight-$depthPile}] -fill $hollowPileColor -tag pile
            }
	} else {
	    $c create rectangle [expr {$x-0.9*$Dp2}] 0 [expr {$x+0.9*$Dp2}] [expr {$subgradeHeight-$depthPile}] -fill $concreteColor -tag pile
	}
	set pmT $columnPropsTMP(pMultT,$i)
	set pmL $columnPropsTMP(pMultL,$i)
	$c create text $x [expr {0.5*($subgradeHeight-$depthPile)}] -text "Pile $i\np=$pmT (T)\np=$pmL (L)" -anchor s -tag pileText

	if {$i == 1} {
	    $c create text [expr {$x+$Djc2}] [expr {$subgradeHeight+$L}] -text " Gf = [format %.2f $Gf] in" -anchor nw -tag hinge
	    #$c create text [expr {$x-$Djc2}] [expr {$subgradeHeight+0.5*$L}] -text "[format %.2f $L] ft " -anchor e -tag column
	}

   }

    $c bind spaceText <Double-Button-1> "WhichColumn %x $x0; ColumnSpacing .columnSpacing"
    $c bind hinge <Double-Button-1> {DefineHinge .hinge}
    $c bind column <Double-Button-1> "WhichColumn %x $x0; set whichSection 1; DefineSection .section"
    $c bind pile <Double-Button-1> "WhichColumn %x $x0; set whichSection 2; DefineSection .section"

    # Draw bent cap
    $c create rectangle [expr {$x0-$Djc2-$capoverhangL}] [expr {$totalHeight + $dcap}] [expr {$x0 + $columnWidth+$Djc2+$capoverhangR}] $totalHeight -fill $concreteColor -tag bentcap

    $c bind bentcap <Double-Button-1> {DefineBentcap .bentcap}

    $c create line 0 $subgradeHeight $viewWidth $subgradeHeight -width 4 -tag groundLevel

    set height0 $subgradeHeight
    set xlabel [expr {$x0 + 0.5*$columnProps(space,2)}]
    set xlabel2 [expr {$x0 + $columnProps(space,2) + 0.5*$columnProps(space,3)}]
    for {set i 1} {$i <= $Nlayers} {incr i} {
	set depthi $soilProps(depth,$i)

	set height0 [expr {$height0-$depthi}]

	$c create line 0 $height0 $viewWidth $height0 -width 2
	
	$c create text 0 [expr {$height0 + 0.5*$depthi}] -text "  Layer $i" -anchor w -tag soilText
	$c create text $xlabel [expr {$height0 + 0.5*$depthi}] -text "[format %.2f $soilProps(depth,$i)] ft" -tag soilText
	set pm $soilProps(multiplier,$i)
	set gg $soilProps(gamma,$i)
	set pp $soilProps(phi,$i)
	$c create text $xlabel2 [expr {$height0 + 0.5*$depthi}] -text "$soilProps(type,$i)\np=$pm\ngamma=$gg" -tag soilText
    }

    set waterTableHeight [expr {$subgradeHeight-$waterTableDepth}]
    $c create line 0 $waterTableHeight $viewWidth $waterTableHeight -width 2 -fill blue
    $c create text [expr {2*$x0 + $columnWidth}] $waterTableHeight -text " Water Table, [format %.2f $soilProps(waterTableDepth)] ft" -anchor se -tag water
    $c bind water <Double-Button-1> {DefineWaterTable .watertable}

    set frozenSoilHeight [expr {$subgradeHeight-$frozenSoilDepth}]
    #$c create line 0 $frozenSoilHeight $viewWidth $frozenSoilHeight -width 2 -fill blue
    #$c create text [expr {2*$x0 + $columnWidth}] $frozenSoilHeight -text " Frozen Soil, [format %.2f $soilProps(frozenSoilDepth)] ft" -anchor ne -tag frozensoil
    #$c bind frozensoil <Double-Button-1> {DefineFrozenSoil .frozensoil}

    set x $x0

    set matTag 10

    $c create rectangle 0 0 $viewWidth $subgradeHeight -fill $groundColor -tag soil -width 0
    $c lower soil

    $c bind soil <Double-Button-1> "WhichLayer %y $totalBentHeight; DefineSoil .soil"
    $c bind soilText <Double-Button-1> "WhichLayer %y $totalBentHeight; DefineSoil .soil"





    set x $x0
    # Draw gravity loads
    for {set i 1} {$i <= $N} {incr i} {
	set x [expr {$x + $columnProps(space,$i)}]

	set firstOrLast first
	if {$columnProps(axialLoad,$i) < 0} {
	    set firstOrLast last
	}

	$c create line $x [expr {$totalHeight + $dcap}] $x $totalBentHeight -width 7 -arrow $firstOrLast -tag gravityLoad
      set loadToShow [expr abs($columnProps(axialLoad,$i))]
      set loadToShow [format %.1f $loadToShow]
	$c create text $x $totalBentHeight -text "  $loadToShow\n  kip" -anchor nw -tag gravityLoadText
    }

    $c bind gravityLoad <Double-Button-1> "WhichColumn %x $x0; DefineGravityLoad .gravity"

    foreach obj "gravityLoad hinge column columnText pile pileText spaceText soilText water frozensoil" {
	$c raise $obj
    }
    #$c raise gravityLoad
    #$c raise hinge
    #$c raise column
    #$c raise columnText
    #$c raise spaceText
    #$c raise soilText
    #$c raise water
    #$c raise frozensoil

    if {$pixelPerFeet(X) < $pixelPerFeet(Y)} {
        #set pixelPerFeet(Y) $pixelPerFeet(X)
    } else {
        #set pixelPerFeet(X) $pixelPerFeet(Y)
    }

    $c scale all 0 0 [expr $pixelPerFeet(X)] [expr -$pixelPerFeet(Y)]
    $c move all 0 $height
    foreach tag "bentcap gravityLoad gravityLoadText column columnText spaceText hinge soilText water" {
	#$c move $tag [expr {0.0*(0.5*$Djc+$bentCapOverhang+0.2*$L)*$pixelPerFeet(X)}] 0
    }
}

set whichColumn 1
proc WhichColumn {x x0} {
    global pixelPerFeet
    set actualX [expr $x/$pixelPerFeet(X)]

    global sectionPropsTMP
    global units

    set Dj2 [expr 0.5*$sectionPropsTMP(Dj,1)*$units(in)]

    global columnProps
    set N $columnProps(N)

    set currentX [expr $x0+$Dj2]

    global whichColumn
    for {set i 1} {$i <= $N} {incr i} {
	if {$actualX <= $currentX} {
	    set whichColumn $i
	    break
	}
	set currentX [expr $currentX + $columnProps(space,[expr $i+1])]
    }
}

set whichLayer 1
proc WhichLayer {y height} {
    global soilProps
    foreach var [array names soilProps] {
	set $var $soilProps($var)
    }

    global pixelPerFeet
    set actualDepth [expr $subgradeHeight - ($height-$y/$pixelPerFeet(Y))]

    global units
    set in $units(in)

    set currentDepth 0.0

    global whichLayer
    for {set i 1} {$i <= $Nlayers} {incr i} {
	set currentDepth [expr $currentDepth + $soilProps(depth,$i)]
	if {$actualDepth <= $currentDepth} {
	    set whichLayer $i
	    break
	}
    }
}

proc checkStrainLimitState {which momentArm whichSections whichElems {TorL T}} {

    global analysisStates
    if {$analysisStates($which,$TorL,disp) > 0} {
	return 0
    }
    
    global limitStatesTMP
    
    set strainLimit $analysisStates($which,val)
    
    if {$analysisStates($which,$TorL,disp) < 0} {
	foreach ele $whichElems {
	    foreach j $whichSections {
		set eps [sectionDeformation $ele $j 1]
		set kap [sectionDeformation $ele $j 2]
		set strainTop [expr {$eps-$kap*$momentArm}]
		set strainBot [expr {$eps+$kap*$momentArm}]
		if {$strainBot >= $strainLimit || $strainTop >= $strainLimit} {
		    return 1
		}
	    }
	}
    }
}

proc SaveNodalDisplacements {{TorL T}} {

    global NodalDisplacements

    set NodalDisplacements(tags,$TorL) ""
    foreach node [getNodeTags] {
       lappend NodalDisplacements(tags,$TorL) $node

       set NodalDisplacements($node,X,$TorL)  [nodeCoord $node X]
       set NodalDisplacements($node,Y,$TorL)  [nodeCoord $node Y]
       set NodalDisplacements($node,UX,$TorL) [nodeDisp $node 1]
       set NodalDisplacements($node,UY,$TorL) [nodeDisp $node 2]
    }
}

proc SaveElementConnectivity {{TorL T}} {

    global ElementConnectivity

    set ElementConnectivity(tags,$TorL) ""
    foreach elem [getEleTags] {
	lappend ElementConnectivity(tags,$TorL) $elem

	set ElementConnectivity($elem,$TorL) [eleNodes $elem]
    }
}

proc SaveMomentDiagram {{TorL T}} {

    global BendingMoments

    global columnProps
    set N $columnProps(N)

    global columnElems
    global pileElems

    global analysisProps
    set SSIon $analysisProps(soilStructure)

    for {set i 1} {$i <= $N} {incr i} {

       set whichElems $columnElems($i,$TorL)
       if {$SSIon} {
          set whichElems "$whichElems $pileElems($i,$TorL)" ;# pileElems could be empty list
       }

       foreach elem $whichElems {
          set BendingMoments($elem,MI,$TorL) [eleForce $elem 3]
          set BendingMoments($elem,MJ,$TorL) [eleForce $elem 6]
          set BendingMoments($elem,P,$TorL)  [eleForce $elem 5]
          set BendingMoments($elem,VI,$TorL) [eleForce $elem 1]
          set BendingMoments($elem,VJ,$TorL) [eleForce $elem 4]
       }
    }
}

proc OpenSeesGravityAnalysis {{TorL T}} {

    test NormUnbalance 1.0e-3 10 0
    algorithm Newton
    constraints Plain
    numberer RCM
    system UmfPack
    integrator LoadControl 1.0
    analysis Static

    set ok [analyze 1]
    if {$ok < 0} {
	return $ok
    }

    global columnProps
    global columnElems
    global extremeAxialLoad

    global sectionsOfInterest

    # Initialize extreme axial loads
    for {set i 1} {$i <= $columnProps(N)} {incr i} {
       set extremeAxialLoad($i,min,when,$TorL) 0
       set extremeAxialLoad($i,max,when,$TorL) 0

       set ele [lindex $columnElems($i,$TorL) 0]
       set sec [lindex $sectionsOfInterest 0]
       set axialForce [expr -[sectionForce $ele $sec 1]]
       set extremeAxialLoad($i,min,$TorL) $axialForce
       set extremeAxialLoad($i,max,$TorL) $axialForce

       foreach ele $columnElems($i,$TorL) {
          foreach sec $sectionsOfInterest {
             set axialForce [expr -[sectionForce $ele $sec 1]]
             if {$axialForce < $extremeAxialLoad($i,min,$TorL)} {
                set extremeAxialLoad($i,min,$TorL) $axialForce
             }
             if {$axialForce > $extremeAxialLoad($i,max,$TorL)} {
                set extremeAxialLoad($i,max,$TorL) $axialForce
             }
          }
       }
    }

    return 0
}

proc DynamicAnalyzeOneStep {dt} {
    set ok [analyze 1 $dt]
    if {$ok < 0} {
	return $ok
    }
    return 0
}

proc AnalyzeOneStep {{TorL T}} {

   set ok [analyze 1]
   if {$ok < 0} {
      return $ok
   }

   set load [getTime]
   set disp [nodeDisp 3 1]

   global columnElems; global pileElems
   global gapSection; global jacketSection
   set sectionsOfInterest "$gapSection $jacketSection"

   global columnProps
   global extremeAxialLoad
   global bentCapForces

   global analysisStates
   global limitStatesTMP

   global units
   set in $units(in)

   global sectionPropsTMP
   set Dj [expr {$sectionPropsTMP(Dj,1)*$in}]
   set tj [expr {$sectionPropsTMP(tj,1)*$in}]
   set Dc [expr {$Dj-2*$tj}]

   set cover [expr {$sectionPropsTMP(cover,1)*$in}]
   set ds $sectionPropsTMP(ds,1)
   set halfBar [expr 0.5*[BarDiam $ds]]

   global rfsteelPropsTMP
   set epsye [expr $rfsteelPropsTMP(fye)/$rfsteelPropsTMP(E)]
   set analysisStates(steelYield,val) $epsye

   set analysisStates(steelLimit,val) $limitStatesTMP(steel)
   set analysisStates(jacketLimit,val) $limitStatesTMP(jacket)
   set analysisStates(concreteLimit,val) $limitStatesTMP(concrete)
   #set analysisStates(concreteLimit,val) [expr -$limitStatesTMP(concrete)]

   global analysisProps

   set N $columnProps(N)

   global bentCapElems
   foreach ele $bentCapElems {
       #puts "$ele: [expr -[eleForce $ele 3]] [eleForce $ele 6]"
   }
   
   # Make sure there is more than one column and analysis is transverse
   if {$N > 1 && $TorL == "T"} {
       # Bent cap forces at inside face of leading pile
       set ele [expr {2*$N+3*$N-1}]
       #puts "Leading element: $ele"
       set bentCapForces(lead,N) [expr [eleForce $ele 4]] ;# tension +ve
       set bentCapForces(lead,V) [expr -[eleForce $ele 5]] ;# down right face +ve
       set bentCapForces(lead,M) [expr [eleForce $ele 6]] ;# top of beam compression +ve

       set q [basicForce $ele]
       set q1 [lindex $q 0]       
       set q2 [lindex $q 1]
       set q3 [lindex $q 2]
       #set bentCapForces(lead,N) [expr $q1]
       #set bentCapForces(lead,M) [expr $q3]
       
       # Bent cap forces at inside face of trailing pile
       set ele [expr {2*$N+5}]
       #puts "Trailing element: $ele"
       set bentCapForces(trail,N) [expr -[eleForce $ele 1]] ;# tension -ve
       set bentCapForces(trail,V) [expr [eleForce $ele 2]] ;# up left face +ve
       set bentCapForces(trail,M) [expr -[eleForce $ele 3]] ;# top of beam compression +ve

       set q [basicForce $ele]
       set q1 [lindex $q 0]       
       set q2 [lindex $q 1]
       set q3 [lindex $q 2]
       #set bentCapForces(trail,N) [expr  $q1]
       #set bentCapForces(trail,M) [expr -$q2]
   }

   for {set i 1} {$i <= $N} {incr i} {
       foreach ele $columnElems($i,$TorL) {
          foreach sec $sectionsOfInterest {
             set axialForce [expr -[sectionForce $ele $sec 1]]
             if {$axialForce < $extremeAxialLoad($i,min,$TorL)} {
                set extremeAxialLoad($i,min,$TorL) $axialForce
                set extremeAxialLoad($i,min,when,$TorL) $load
             }
             if {$axialForce > $extremeAxialLoad($i,max,$TorL)} {
                set extremeAxialLoad($i,max,$TorL) $axialForce
                set extremeAxialLoad($i,max,when,$TorL) $load
             }
          }
       }

      set momentArm [expr {0.5*$Dc-$cover}]
      if {[checkStrainLimitState steelYield $momentArm $gapSection $columnElems($i,$TorL) $TorL] > 0} {
         set analysisStates(steelYield,$TorL,disp) $disp
         set analysisStates(steelYield,$TorL,load) $load
      }
      

      set momentArm [expr {0.5*$Dc-$cover-$halfBar}]
      if {[checkStrainLimitState steelLimit $momentArm $gapSection $columnElems($i,$TorL) $TorL] > 0} {
         set analysisStates(steelLimit,$TorL,disp) $disp
         set analysisStates(steelLimit,$TorL,load) $load
      }

      set momentArm [expr {0.5*$Dj-$tj}]
      if {$analysisProps(soilStructure) == 1 && [checkStrainLimitState jacketLimit $momentArm $jacketSection $pileElems($i,$TorL) $TorL] > 0} {
         set analysisStates(jacketLimit,$TorL,disp) $disp
         set analysisStates(jacketLimit,$TorL,load) $load
      }

       #set momentArm [expr {0.5*$Dc-$cover}]
       #if {[checkStrainLimitState concreteLimit $momentArm $gapSection $columnElems($i,$TorL) $TorL] > 0} {
       #   set analysisStates(concreteLimit,$TorL,disp) $disp
       #   set analysisStates(concreteLimit,$TorL,load) $load
       #}
       
       # Check for user-defined limit strain of concrete
       if {$analysisStates(concreteLimit,$TorL,disp) < 0} {
	   set momentArm [expr {0.5*$Dc-$cover}]
	   foreach ele $columnElems($i,$TorL) {
	       foreach sec $gapSection {
		   set eps [sectionDeformation $ele $sec 1]
		   set kap [sectionDeformation $ele $sec 2]
		   set strainTop [expr {$eps-$kap*$momentArm}]
		   set strainBot [expr {$eps+$kap*$momentArm}]
		   if {$strainBot <= -$limitStatesTMP(concrete) || 
		       $strainTop <= -$limitStatesTMP(concrete)} {
		       set analysisStates(concreteLimit,$TorL,disp) $disp
		       set analysisStates(concreteLimit,$TorL,load) $load
		   }
	       }
	   }
       }
       
   }
   
   return 0
}

proc SaveFinalAxialLoads {{TorL T}} {

    if {$TorL == "L"} {
	return
    }

    global finalAxialLoad
    global columnProps
    global columnElems; global sectionsOfInterest
    
    for {set i 1} {$i <= $columnProps(N)} {incr i} {
	set ele [lindex $columnElems($i,$TorL) 0]
	set sec [lindex $sectionsOfInterest 0]
	set axialForce [expr -[sectionForce $ele $sec 1]]
	set finalAxialLoad($i) $axialForce
    }
}

set OpenSeesPushoverResults(T) ""
set OpenSeesPushoverResults(L) ""
proc OpenSeesPushoverAnalysis {{TorL T}} {

    global OpenSeesPushoverResults

    if {![IsModelDirty] && [llength $OpenSeesPushoverResults($TorL)] > 0} {
       return $OpenSeesPushoverResults($TorL)
    }

    global units
    set in $units(in)
    set ft $units(ft)

    global columnProps
    set L [expr {$columnProps(L)*$ft}]

    wipe
    
    DefineModel [getCanvas]

    DefineOpenSeesModel $TorL

    set ok [OpenSeesGravityAnalysis $TorL]
    if {$ok < 0} {
       # Do something
    }

    global bentCapElems

    global columnElems
    global columnProps
    set N $columnProps(N)

    global sectionProps
    set Dj $sectionProps(Dj,1)
    global bentcapProps
    set oL $bentcapProps(leftoverhang)
    set oR $bentcapProps(rightoverhang)
    set Lbent [expr $Dj*$in + ($oL+$oR)*$in]
    for {set i 2} {$i <= $N} {incr i} {
	set Lbent [expr {$Lbent + $columnProps(space,$i)}]
    }
    
    global analysisProps

    pattern Plain 2 Linear {
	#load 3 1.0 0.0 0.0
	if {$TorL == "L"} {
	    for {set i 1} {$i <= $N} {incr i} {
		load [expr 5*$i-2] [expr 1.0/$N] 0.0 0.0
	    }
	} else {
	    foreach ele $bentCapElems {
		if {$analysisProps(lateralLoad) == 0} {
		    break
		}
		eleLoad -ele $ele -type beamUniform 0.0 [expr 1.0/$Lbent]
	    }
	    set Nloads [expr 3*$N+2]
	    #load 0 [expr 1.0/$Nloads] 0.0 0.0
	    for {set i 1} {$analysisProps(lateralLoad) == 0 && $i <= $N} {incr i} {
		#load [expr 5*$i-2] [expr 1.0/$Nloads] 0.0 0.0
		#load [expr 5*$i-1] [expr 1.0/$Nloads] 0.0 0.0
		#load [expr 5*$i-0] [expr 1.0/$Nloads] 0.0 0.0
		load [expr 5*$i-2] [expr 1.0/$N] 0.0 0.0
	    }
	    #load [expr 5*$N+1] [expr 1.0/$Nloads] 0.0 0.0
	}
    }

    integrator DisplacementControl 3 1 [expr {0.5*$in}]

    test NormUnbalance 1.0e-3 100 0
    algorithm KrylovNewton
    analysis Static

    # Compute the pushover curve
    set j 0

    global analysisProps
    set maxDisp [expr {$analysisProps(lateralDisp,$TorL)*$ft}]

    global sectionPropsTMP
    set Dj [expr {$sectionPropsTMP(Dj,1)*$in}]
    if {$Dj > $maxDisp} {
       #set maxDisp $Dj
    }

    global gapSection
    global jacketSection
    global sectionsOfInterest

    set lastLoad 0.0
    set maxLoad -1.0

    global limitStatesTMP

    global analysisStates

    set analysisStates(steelYield,$TorL,disp) -1.0
    set analysisStates(steelLimit,$TorL,disp) -1.0
    set analysisStates(jacketLimit,$TorL,disp) -1.0
    set analysisStates(concreteLimit,$TorL,disp) -1.0
    set analysisStates(stability,$TorL,disp) -1.0
    set analysisStates(postPeak,$TorL,disp) -1.0
    set analysisStates(maxLoad,$TorL,disp) -1.0

    global analysisProps
    set fractionMaxLoad [expr {$analysisProps(postPeak)/100.0}]

    global stopReport

    SaveElementConnectivity $TorL

    set ok 0
    set disp 0
    set coords "0 0"

    while {$disp < $maxDisp} {

	update
	if {$stopReport == 1} {
	    break
	}
	
	set ok [AnalyzeOneStep $TorL]
	if {$ok < 0} {
	    break
	}
	
	set load [getTime]
	set disp [nodeDisp 3 1]
	lappend coords $disp $load
	
	if {$load > $maxLoad} {
	    set maxLoad $load
	    set analysisStates(maxLoad,$TorL,disp) $disp
	    set analysisStates(maxLoad,$TorL,load) $load
	}
	
	# Check for loss of stability
	if {$analysisStates(stability,$TorL,disp) < 0 && $load < $lastLoad} {
	    set analysisStates(stability,$TorL,disp) $disp
	    set analysisStates(stability,$TorL,load) $load
	}
	set lastLoad $load
	
	# Terminate analysis at specified post-peak capacity
	if {$analysisStates(postPeak,$TorL,disp) < 0 && $load <= [expr {$fractionMaxLoad*$maxLoad}]} {
	    #set stopAnalysis 1
	    set analysisStates(postPeak,$TorL,disp) $disp
	    set analysisStates(postPeak,$TorL,load) $load
	}
	
	
	SaveNodalDisplacements $TorL
	
	SaveMomentDiagram $TorL
	
	incr j
    }
    
    SaveFinalAxialLoads $TorL
    
    set OpenSeesPushoverResults($TorL) $coords
    return $coords
}

set dynamicProgressWindowOpen 0
set dynamicAnalyzeWindowOpen 0
proc AnalyzeDynamic {w} {

    global dynamicProgressWindowOpen
    global dynamicAnalyzeWindowOpen
    if {$dynamicProgressWindowOpen == 1 || $dynamicAnalyzeWindowOpen == 1} {raise $w; return}

    global wd

    global units
    set in $units(in)
    set ft $units(ft)
    set sec $units(sec)

    global columnProps
    set L [expr {$columnProps(L)*$ft}]

    global stopDynamicAnalysis

    createTopLevel $w "Time History Progress"

    set dynamicProgressWindowOpen 1
    bind $w <Destroy> {set progressWindowOpen 0; set stopDynamicAnalysis 1}

    frame $w.progress

    label $w.progress.label -text "Time" -width 10 
    grid $w.progress.label -row 0 -column 0 -sticky ew
    label $w.progress.update -width 20
    grid $w.progress.update -row 1 -column 0 -sticky ew

    set stopDynamicAnalysis 0
    button $w.progress.cancel -text "Stop" -command "set stopDynamicAnalysis 1"
    grid $w.progress.cancel -row 2 -column 0 -sticky ew

    pack $w.progress -side top

    wipe
    
    DefineModel [getCanvas]

    DefineOpenSeesModel 

    set ok [OpenSeesGravityAnalysis]
    if {$ok < 0} {
       # Do something
    }

    global columnElems

    global eqProps
    set dtGM $eqProps(dtGM)
    set gmFile $eqProps(filename)
    set PGA $eqProps(PGA)

    global analysisProps
    set tFinal $analysisProps(tFinal)

    set g [expr 32.2*$ft/$sec**2]

    if {[file exists $gmFile]} {
	set readGM [open $gmFile r]
	set maxag 0.0
	while {[gets $readGM ag] >= 0} {
	    if {[expr abs($ag)] > $maxag} {
		set maxag [expr abs($ag)]
	    }
	}
	close $readGM
	timeSeries Path 1 -dt $dtGM -filePath $gmFile -factor [expr {($PGA/$maxag)*$g}]
    } else {
	timeSeries Path 1 -time "0 $tFinal" -values "0 0"
    }
    pattern UniformExcitation 2 1 -accel 1

    # Rayleigh damping
    set omega2 [eigen 1]
    set omega [expr sqrt($omega2)]
    set zeta [expr $analysisProps(damping)/100.0] ;# Turn % to decimal
    set aK [expr 2*$zeta/$omega]
    if {$analysisProps(stiffnessProp) == 1} {
	# Proportional to (last committed) tangent stiffness
	rayleigh 0.0 0.0 0.0 $aK
    } else {
	# Proportional to initial stiffness
	rayleigh 0.0 0.0 $aK 0.0
    }

    integrator Newmark 0.5 0.25

    test NormUnbalance 1.0e-3 100 0
    algorithm KrylovNewton
    analysis Transient


    global viewProps

    set dt $analysisProps(timestep)

    global columnProps
    set N $columnProps(N)

    set t 0.0
    set Vbase 0.0
    for {set i 1} {$i <= $N} {incr i} {
	set V [lindex [eleForce [expr 2*$i-1]] 0]
	set Vbase [expr $Vbase + $V]
    }
    set coordsU "0 [nodeDisp 3 1]"
    set coordsV "[nodeDisp 3 1] [expr -$Vbase]"
    while {$stopDynamicAnalysis == 0 && $t < $tFinal} {
	set t [expr {$t+$dt}]

	$w.progress.update config -text "[format %.2f $t] of $tFinal sec"

	update

	set ok [DynamicAnalyzeOneStep $dt]
	if {$ok < 0} {
	    break
	}

	set Vbase 0.0
	for {set i 1} {$i <= $N} {incr i} {
	    set V [lindex [eleForce [expr 2*$i-1]] 0]
	    set Vbase [expr $Vbase + $V]
	}
	lappend coordsU $t [nodeDisp 3 1]
	lappend coordsV [nodeDisp 3 1] [expr -$Vbase]

	SaveNodalDisplacements T
	if {$viewProps(drawD) == 1} {
	    DrawDisplacedShape [getCanvas] T 0
	}
	SaveMomentDiagram T
	if {$viewProps(drawM) == 1} {
	    DrawMomentDiagram [getCanvas] T 0
	}
	if {$viewProps(drawV) == 1} {
	    DrawShearDiagram [getCanvas] T 0
	}
    }

    destroy $w


    global dynamicAnalyzeWindowOpen
    if {$dynamicAnalyzeWindowOpen == 1} {raise $w; return}

    createTopLevel $w "Displacement Time History"

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    set height 600
    set width 600
    set graph foo
    set c [createEMUGraph $w $graph $width $height]

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left

    set m [menu $w.mbar.file.menu]
    $m add command -label "Print (Ctrl+P)" -command "PrintCanvas $c $w"
    bind $w <Control-Key-p> "PrintCanvas $c $w"
    $m add command -label "Export (Ctrl+X)" -command "ExportData [list $coordsU] $w"
    bind $w <Control-Key-x> "ExportData [list $coordsU] $w"
    $m add separator
    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"

    set analyzeWindowOpen 1
    bind $w <Destroy> {set analyzeWindowOpen 0}

    # Find max load-disp
    set emax 0
    set smax 0
    foreach {t u} $coordsU {
       if {$t > $emax} {set emax $t}
       if {$u > $smax} {set smax $u}
    }

    $graph data d2 -colour blue -points 0 -lines 1 -coords $coordsU

    axesEMUGraph $graph $emax [expr abs($smax)] 0 [expr -abs($smax)]
    $graph redraw

    set graphInfo(title) "Displacement Time History"
    set graphInfo(xlabel) "Time (s)"
    set graphInfo(ylabel) "Displacement (ft)"
    labelEMUGraph $c $graph graphInfo $width $height





    toplevel $w.b
    wm title $w.b "Force-Displacement Response"
    wm minsize $w.b 200 100

    set w $w.b

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    set height 600
    set width 600
    set graph foo
    set c [createEMUGraph $w $graph $width $height]

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left

    set m [menu $w.mbar.file.menu]
    $m add command -label "Print (Ctrl+P)" -command "PrintCanvas $c $w"
    bind $w <Control-Key-p> "PrintCanvas $c $w"
    $m add command -label "Export (Ctrl+X)" -command "ExportData [list $coordsV] $w"
    bind $w <Control-Key-x> "ExportData [list $coordsV] $w"
    $m add separator
    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"

    set analyzeWindowOpen 1
    bind $w <Destroy> {set analyzeWindowOpen 0}

    # Find max load-disp
    set emax 0
    set smax 0
    set emin 0
    set smin 0
    foreach {t u} $coordsV {
       if {$t > $emax} {set emax $t}
       if {$t < $emin} {set emin $t}
       if {$u > $smax} {set smax $u}
       if {$u < $smin} {set smin $u}
    }

    $graph data d2 -colour blue -points 0 -lines 1 -coords $coordsV

    axesEMUGraph $graph $emax $smax $emin $smin
    $graph redraw

    set graphInfo(title) "Force-Displacement Response"
    set graphInfo(xlabel) "Displacement (ft)"
    set graphInfo(ylabel) "Base Shear (kip)"
    labelEMUGraph $c $graph graphInfo $width $height
}




set progressWindowOpen 0
set analyzeWindowOpen 0
proc Analyze {w {TorL T}} {

    global progressWindowOpen
    global analyzeWindowOpen
    if {$progressWindowOpen == 1 || $analyzeWindowOpen == 1} {raise $w; return}

    global wd

    global units
    set in $units(in)
    set ft $units(ft)

    global columnProps
    set L [expr {$columnProps(L)*$ft}]

    global columnElems

    global stopAnalysis

    createTopLevel $w "Analysis Progress"

    set progressWindowOpen 1
    bind $w <Destroy> {set progressWindowOpen 0; set stopAnalysis 1}

    frame $w.progress

    if {$TorL == "T"} {
       label $w.progress.label -text "Transverse Displacement"
    } else {
       label $w.progress.label -text "Longitudinal Displacement"
    }
    grid $w.progress.label -row 0 -column 0 -sticky ew
    label $w.progress.update
    grid $w.progress.update -row 1 -column 0 -sticky ew

    set stopAnalysis 0
    button $w.progress.cancel -text "Stop" -command "set stopAnalysis 1"
    grid $w.progress.cancel -row 2 -column 0 -sticky ew

    pack $w.progress -side top

    
    
    wipe
    
    DefineModel [getCanvas]
    
    DefineOpenSeesModel $TorL
    
    set ok [OpenSeesGravityAnalysis $TorL]
    if {$ok < 0} {
	# Do something
    }
    
    global bentCapElems

    global columnElems
    global columnProps
    set N $columnProps(N)

    global sectionProps
    set Dj $sectionProps(Dj,1)
    global bentcapProps
    set oL $bentcapProps(leftoverhang)
    set oR $bentcapProps(rightoverhang)
    set Lbent [expr $Dj*$in + ($oL+$oR)*$in]
    for {set i 2} {$i <= $N} {incr i} {
	set Lbent [expr {$Lbent + $columnProps(space,$i)}]
    }

    global analysisProps

    pattern Plain 2 Linear {
	#load 3 1.0 0.0 0.0
	if {$TorL == "L"} {
	    for {set i 1} {$i <= $N} {incr i} {
		load [expr 5*$i-2] [expr 1.0/$N] 0.0 0.0
	    }
	} else {
	    foreach ele $bentCapElems {
		if {$analysisProps(lateralLoad) == 0} {
		    break
		}
		eleLoad -ele $ele -type beamUniform 0.0 [expr 1.0/$Lbent]
	    }
	    set Nloads [expr 3*$N+2]
	    #load 0 [expr 1.0/$Nloads] 0.0 0.0
	    for {set i 1} {$analysisProps(lateralLoad) == 0 && $i <= $N} {incr i} {
		#load [expr 5*$i-2] [expr 1.0/$Nloads] 0.0 0.0
		#load [expr 5*$i-1] [expr 1.0/$Nloads] 0.0 0.0
		#load [expr 5*$i-0] [expr 1.0/$Nloads] 0.0 0.0
		load [expr 5*$i-2] [expr 1.0/$N] 0.0 0.0
	    }
	    #load [expr 5*$N+1] [expr 1.0/$Nloads] 0.0 0.0
	}
    }
    integrator DisplacementControl 3 1 [expr {0.5*$in}]
    
    test NormUnbalance 1.0e-3 100 0
    algorithm KrylovNewton
    analysis Static
    
    
    # Compute the pushover curve
    set ok 0
    set j 0
    
    global analysisProps
    set maxDisp [expr {$analysisProps(lateralDisp,$TorL)*$ft}]

    global sectionPropsTMP
    set Dj [expr {$sectionPropsTMP(Dj,1)*$in}]
    if {$Dj > $maxDisp} {
	#set maxDisp $Dj
    }
    
    global gapSection
    global jacketSection
    global sectionsOfInterest
    
    set maxLoad 0.0
    set lastLoad 0.0
    set maxLoad -1.0
    
    global analysisStates
    
    set analysisStates(steelYield,$TorL,disp) -1.0
    set analysisStates(steelLimit,$TorL,disp) -1.0
    set analysisStates(jacketLimit,$TorL,disp) -1.0
    set analysisStates(concreteLimit,$TorL,disp) -1.0
    set analysisStates(stability,$TorL,disp) -1.0
    set analysisStates(postPeak,$TorL,disp) -1.0
    set analysisStates(maxLoad,$TorL,disp) -1.0
    
    global analysisProps
    set fractionMaxLoad [expr {$analysisProps(postPeak)/100.0}]
    
    SaveElementConnectivity $TorL
    SaveNodalDisplacements $TorL
    
    #DrawDisplacedShape [getCanvas] 0
    global viewProps

    set disp 0
    set ok 0
    set coords "[nodeDisp 3 1] 0"

    while {$stopAnalysis == 0 && $disp < $maxDisp} {

	$w.progress.update config -text "[format %.2f $disp] / [format %.2f $maxDisp] ft"
	
	update
	
	set ok [AnalyzeOneStep $TorL]
	if {$ok < 0} {
	    break
	}
	
	set load [getTime]
	set disp [nodeDisp 3 1]
	lappend coords $disp $load


	if {$load > $maxLoad} {
	    set maxLoad $load
	    set analysisStates(maxLoad,$TorL,disp) $disp
	    set analysisStates(maxLoad,$TorL,load) $load
	}

	# Check for loss of stability
	if {$analysisStates(stability,$TorL,disp) < 0 && $load < $lastLoad} {
	    set analysisStates(stability,$TorL,disp) $disp
	    set analysisStates(stability,$TorL,load) $load
	}
	set lastLoad $load

	# Terminate analysis at specified post-peak capacity
	if {$analysisStates(postPeak,$TorL,disp) < 0 && $load <= [expr {$fractionMaxLoad*$maxLoad}]} {
	    #set stopAnalysis 1
	    set analysisStates(postPeak,$TorL,disp) $disp
	    set analysisStates(postPeak,$TorL,load) $load
	}

	incr j
	
	SaveNodalDisplacements $TorL
	if {$viewProps(drawD) == 1} {
	    DrawDisplacedShape [getCanvas] $TorL 0
	}
	SaveMomentDiagram $TorL
	if {$viewProps(drawM) == 1} {
	    DrawMomentDiagram [getCanvas] $TorL 0
	}
	if {$viewProps(drawV) == 1} {
	    DrawShearDiagram [getCanvas] $TorL 0
	}

    }

    SaveFinalAxialLoads $TorL

    global OpenSeesPushoverResults
    set OpenSeesPushoverResults($TorL) $coords


    destroy $w

    global analyzeWindowOpen

    if {$analyzeWindowOpen == 1} {raise $w; return}

    if {$TorL == "T"} {
	createTopLevel $w "Transverse Pushover Analysis"
    } else {
	createTopLevel $w "Longitudinal Pushover Analysis"
    }

    # Create frame for menu options
    frame $w.mbar -borderwidth 1 -relief raised
    pack $w.mbar -fill x

    set height 600
    set width 600
    set graph foo
    set c [createEMUGraph $w $graph $width $height]

    # Create "File" pull down menu
    menubutton $w.mbar.file -text File -menu $w.mbar.file.menu
    pack $w.mbar.file -side left

    set m [menu $w.mbar.file.menu]
    $m add command -label "Print (Ctrl+P)" -command "PrintCanvas $c $w"
    bind $w <Control-Key-p> "PrintCanvas $c $w"
    $m add command -label "Export (Ctrl+X)" -command "ExportData [list $coords] $w"
    bind $w <Control-Key-x> "ExportData [list $coords] $w"
    $m add separator
    $m add command -label "Close (Ctrl+W)" -command "destroy $w"
    bind $w <Control-Key-w> "destroy $w"


    set analyzeWindowOpen 1
    bind $w <Destroy> {set analyzeWindowOpen 0}

    # Find max load-disp
    set emax 0
    set smax 0
    foreach {e s} $coords {
       if {$e > $emax} {set emax $e}
       if {$s > $smax} {set smax $s}
    }

    $graph data d2 -colour blue -points 0 -lines 1 -coords $coords

    axesEMUGraph $graph $emax $smax
    $graph redraw

    PlotLimitStates $graph $c $TorL

    if {$TorL == "T"} {
	set graphInfo(title) "Transverse Pushover Analysis"
    } else {
	set graphInfo(title) "Longitudinal Pushover Analysis"
    }
    set graphInfo(xlabel) "Displacement (ft)"
    set graphInfo(ylabel) "Load (kip)"
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

proc PlotLimitStates {graph canvas {TorL T}} {

   global analysisStates

   # Two loops so that text is always on top of lines
   foreach {state} {concreteLimit steelLimit jacketLimit steelYield postPeak} {
      if {$analysisStates($state,$TorL,disp) > 0} {
         $graph vmark $analysisStates($state,$TorL,disp) $state red
      }
   }

   foreach {state label} {concreteLimit "Concrete Limit" steelLimit "Steel Limit" jacketLimit "Shell Limit" steelYield "First Steel Yield" postPeak "Post-Peak"} {
      if {$analysisStates($state,$TorL,disp) > 0} {
         set xc [$graph x2canvas $analysisStates($state,$TorL,disp)]
         set yc [$graph y2canvas $analysisStates($state,$TorL,load)]
	
         $canvas create text $xc $yc -text " $label" -anchor nw
      }
   }

   # Only print maxLoad line if peak has been reached
   if {$analysisStates(maxLoad,$TorL,disp) > 0 && $analysisStates(stability,$TorL,disp) > 0} {
      $graph vmark $analysisStates(maxLoad,$TorL,disp) $state red
      set xc [$graph x2canvas $analysisStates(maxLoad,$TorL,disp)]
      set yc [$graph y2canvas $analysisStates(maxLoad,$TorL,load)]
	
      $canvas create text $xc $yc -text " Peak Capacity" -anchor nw
   }
}

proc DrawDisplacedShape {c {TorL T} {fromStorage 1} {mag 1.0}} {

    clearCanvas $c displacedShape

    set width [winfo reqwidth $c]
    set height [winfo reqheight $c]
    
    global units
    
    global soilProps
    set subgradeHeight $soilProps(subgradeHeight)

    set radius [expr {1.0*$units(in)}]
    
    global NodalDisplacements
    if {$fromStorage} {
	set nodeList $NodalDisplacements(tags,$TorL)
    } else {
	set nodeList [getNodeTags]
    }
    foreach node $nodeList {
	if {$fromStorage} {
	    set nodes($node,X) $NodalDisplacements($node,X,$TorL)
	    set nodes($node,Y) $NodalDisplacements($node,Y,$TorL)
	    set X [expr {$nodes($node,X) + $mag*$NodalDisplacements($node,UX,$TorL)}]
	    set Y [expr {$nodes($node,Y) + $mag*$NodalDisplacements($node,UY,$TorL)}]
	} else {
	    set nodes($node,X) [nodeCoord $node X]
	    set nodes($node,Y) [nodeCoord $node Y]	
	    set X [expr {$nodes($node,X) + $mag*[nodeDisp $node 1]}]
	    set Y [expr {$nodes($node,Y) + $mag*[nodeDisp $node 2]}]
	}
	$c create oval [expr {$X-$radius}] [expr {$Y-$radius}] [expr {$X+$radius}] [expr {$Y+$radius}] -fill black -tag displacedShape
	#$c create text $X $Y -text " $node" -tag displacedShape
    }
    
    global ElementConnectivity
    if {$fromStorage} {
	set elemList $ElementConnectivity(tags,$TorL)
    } else {
	set elemList [getEleTags]
    }
    foreach elem $elemList {
	if {$fromStorage} {
	    set eleNodes $ElementConnectivity($elem,$TorL)
	} else {
	    set eleNodes [eleNodes $elem]
	}
	
	set nodeI [lindex $eleNodes 0]
	set nodeJ [lindex $eleNodes 1]
	
	if {$fromStorage} {
	    set UXI $NodalDisplacements($nodeI,UX,$TorL)
	    set UYI $NodalDisplacements($nodeI,UY,$TorL)
	    set UXJ $NodalDisplacements($nodeJ,UX,$TorL)
	    set UYJ $NodalDisplacements($nodeJ,UY,$TorL)
	} else {
	    set UXI [nodeDisp $nodeI 1]
	    set UYI [nodeDisp $nodeI 2]
	    set UXJ [nodeDisp $nodeJ 1]
	    set UYJ [nodeDisp $nodeJ 2]
	}

	set XI [expr {$nodes($nodeI,X) + $mag*$UXI}]
	set YI [expr {$nodes($nodeI,Y) + $mag*$UYI}]
	set XJ [expr {$nodes($nodeJ,X) + $mag*$UXJ}]
	set YJ [expr {$nodes($nodeJ,Y) + $mag*$UYJ}]
	
	$c create line $XI $YI $XJ $YJ -tag displacedShape
	if {[expr $UXI*$UXJ] < 0} {
	    $c create text $XJ $YJ -text " [format %.1f [expr $YI-$subgradeHeight]] ft" -anchor w -tag displacedShape
	}
    }
    
    global pixelPerFeet
    
    $c scale displacedShape 0 0 [expr $pixelPerFeet(X)] [expr -$pixelPerFeet(Y)]
    $c move displacedShape 0 $height
}

proc DrawMomentDiagram {c {TorL T} {fromStorage 1} {mag 0.0002}} {

    clearCanvas $c momentDiagram

    set width [winfo reqwidth $c]
    set height [winfo reqheight $c]

    global units

    global BendingMoments

    global NodalDisplacements
    if {$fromStorage} {
       set nodeList $NodalDisplacements(tags,$TorL)
    } else {
       set nodeList [getNodeTags]
    }
    foreach node $nodeList {
       if {$fromStorage} {
	   set nodes($node,X) $NodalDisplacements($node,X,$TorL)
	   set nodes($node,Y) $NodalDisplacements($node,Y,$TorL)
       } else {
	   set nodes($node,X) [nodeCoord $node X]
	   set nodes($node,Y) [nodeCoord $node Y]	
       }
    }
        
    set elemList [getEleTags]

    global ElementConnectivity
    if {$fromStorage} {
	set elemList $ElementConnectivity(tags,$TorL)
    } else {
	
    }
    foreach elem $elemList {
	if {$fromStorage} {
	    set eleNodes $ElementConnectivity($elem,$TorL)
	} else {
	    set eleNodes [eleNodes $elem]
	}
	
	set nodeI [lindex $eleNodes 0]
	set nodeJ [lindex $eleNodes 1]
	
	set XI [expr $nodes($nodeI,X)]
	set YI [expr $nodes($nodeI,Y)]
	set XJ [expr $nodes($nodeJ,X)]
	set YJ [expr $nodes($nodeJ,Y)]
	
	#$c create line $XI $YI $XJ $YJ -tag momentDiagram
    }
    
    global columnProps
    set N $columnProps(N)

    global soilProps
    set H $soilProps(subgradeHeight)

    set radius [expr {2.0*$units(in)}]

    global columnElems
    global pileElems
    global analysisProps
    set SSIon $analysisProps(soilStructure)

    for {set i 1} {$i <= $N} {incr i} {

       set maxMPos 0; set maxMPosY 0; set maxMPosX 0
       set maxMNeg 0; set maxMNegY 0; set maxMNegX 0
       set zeroMPosY 0

       foreach elem $columnElems($i,$TorL) {
          if {$fromStorage} {
             set eleNodes $ElementConnectivity($elem,$TorL)
          } else {
             set eleNodes [eleNodes $elem]
          }
          set ndI [lindex $eleNodes 0]
          set ndJ [lindex $eleNodes 1]
	
          set XI [expr $nodes($ndI,X)]
          set YI [expr $nodes($ndI,Y)]
          set XJ [expr $nodes($ndJ,X)]
          set YJ [expr $nodes($ndJ,Y)]

          if {$fromStorage} {
              set MI [expr -$BendingMoments($elem,MI,$TorL)]
              set MJ $BendingMoments($elem,MJ,$TorL)
          } else {
              set MI [expr -[eleForce $elem 3]]
              set MJ [eleForce $elem 6]
          }

          if {$MJ > $maxMPos} {
             set maxMPos $MJ
             set maxMPosY $YJ
          }
          if {$MJ < $maxMNeg} {
             set maxMNeg $MJ
             set maxMNegY $YJ
          }
          if {$MI > $maxMPos} {
             set maxMPos $MI
             set maxMPosY $YI
          }
          if {$MI < $maxMNeg} {
             set maxMNeg $MI
             set maxMNegY $YI
          }

          if {[expr $MI*$MJ] < 0} {
             set YInfl [expr {$YI - $MI*($YJ-$YI)/($MJ-$MI)}]
             $c create oval [expr {$XI-$radius}] [expr {$YInfl-$radius}] [expr {$XI+$radius}] [expr {$YInfl+$radius}] -fill black -tag momentDiagram
             $c create text $XI $YInfl -text " [format %.1f [expr {$YInfl-$H}]] ft" -anchor nw -tag momentDiagram       
          }

          set MI [expr {$MI*$mag}]
          set MJ [expr {$MJ*$mag}]
          $c create line $XI $YI $XJ $YJ -tag momentDiagram
          $c create line [expr {$XI+$MI}] $YI [expr {$XJ+$MJ}] $YJ -tag momentDiagram
       }


       foreach elem $pileElems($i,$TorL) {
	   if {$fromStorage} {
	       set eleNodes $ElementConnectivity($elem,$TorL)
	   } else {
	       set eleNodes [eleNodes $elem]
	   }
	   set ndI [lindex $eleNodes 0]
	   set ndJ [lindex $eleNodes 1]
	   
	   set XI [expr $nodes($ndI,X)]
	   set YI [expr $nodes($ndI,Y)]
	   set XJ [expr $nodes($ndJ,X)]
	   set YJ [expr $nodes($ndJ,Y)]
	   
	   if {$fromStorage} {
	       set MI [expr -$BendingMoments($elem,MI,$TorL)]
	       set MJ $BendingMoments($elem,MJ,$TorL)
	   } else {
	       set MI [expr -[eleForce $elem 3]]
	       set MJ [eleForce $elem 6]
	   }
	   
	   if {$MJ > $maxMPos} {
	       set maxMPos $MJ
	       set maxMPosY $YJ
	   }
	   if {$MJ < $maxMNeg} {
	       set maxMNeg $MJ
	       set maxMNegY $YJ
	   }
	   if {$MI > $maxMPos} {
	       set maxMPos $MI
	       set maxMPosY $YI
	   }
	   if {$MI < $maxMNeg} {
	       set maxMNeg $MI
	       set maxMNegY $YI
	   }
	   
	   # Make sure we are only drawing first occurrence in pile region
	   if {$zeroMPosY == 0 && [expr {$MI*$MJ}] < 0 && $YI < $maxMPosY && $YI < $H} {
	       set zeroMPosY $YI
	   }
	   
	   set MI [expr {$MI*$mag}]
	   set MJ [expr {$MJ*$mag}]
	   $c create line $XI $YI $XJ $YJ -tag momentDiagram
	   $c create line [expr {$XI+$MI}] $YI [expr {$XJ+$MJ}] $YJ -tag momentDiagram
	   #$c create oval [expr {$XI + $MI-$radius}] [expr {$YI-$radius}] [expr {$XI + $MI+$radius}] [expr {$YI+$radius}] -fill black -tag momentDiagram
	   #$c create oval [expr {$XJ + $MJ-$radius}] [expr {$YJ-$radius}] [expr {$XJ + $MJ+$radius}] [expr {$YJ+$radius}] -fill black -tag momentDiagram
       }
       set X [expr {$XI + $maxMPos*$mag}]
       $c create text $X $maxMPosY -text " [format %.1f $maxMPos] kip-ft" -anchor sw -tag momentDiagram
       $c create text $X $maxMPosY -text " [format %.1f [expr {$maxMPosY-$H}]] ft" -anchor nw -tag momentDiagram
       $c create oval [expr {$X-$radius}] [expr {$maxMPosY-$radius}] [expr {$X+$radius}] [expr {$maxMPosY+$radius}] -fill black -tag momentDiagram
       
       set X [expr {$XI + $maxMNeg*$mag}]
       $c create text $X $maxMNegY -text " [format %.1f [expr -$maxMNeg]] kip-ft" -anchor sw -tag momentDiagram
       $c create text $X $maxMNegY -text " [format %.1f [expr {$maxMNegY-$H}]] ft" -anchor nw -tag momentDiagram
       $c create oval [expr {$X-$radius}] [expr {$maxMNegY-$radius}] [expr {$X+$radius}] [expr {$maxMNegY+$radius}] -fill black -tag momentDiagram
       
       if {$SSIon && $zeroMPosY >= [expr 1.0*$units(ft)]} {
	   #$c create text $XI $zeroMPosY -text " M = 0" -anchor sw -tag momentDiagram
	   $c create text $XI $zeroMPosY -text " [format %.1f [expr {$zeroMPosY-$H}]] ft" -anchor sw -tag momentDiagram       
	   $c create oval [expr {$XI-$radius}] [expr {$zeroMPosY-$radius}] [expr {$XI+$radius}] [expr {$zeroMPosY+$radius}] -fill black -tag momentDiagram
       }
   }
    global pixelPerFeet
    
    $c scale momentDiagram 0 0 [expr $pixelPerFeet(X)] [expr -$pixelPerFeet(Y)]
    $c move momentDiagram 0 $height
}

proc DrawShearDiagram {c {TorL T} {fromStorage 1} {mag 0.005}} {

    clearCanvas $c shearDiagram

    set width [winfo reqwidth $c]
    set height [winfo reqheight $c]

    global units

    global BendingMoments

    global NodalDisplacements
    if {$fromStorage} {
	set nodeList $NodalDisplacements(tags,$TorL)
    } else {
	set nodeList [getNodeTags]
    }
    foreach node $nodeList {
	if {$fromStorage} {
	    set nodes($node,X) $NodalDisplacements($node,X,$TorL)
	    set nodes($node,Y) $NodalDisplacements($node,Y,$TorL)
	} else {
	    set nodes($node,X) [nodeCoord $node X]
	    set nodes($node,Y) [nodeCoord $node Y]	
	}
    }
    
    global ElementConnectivity
    if {$fromStorage} {
	set elemList $ElementConnectivity(tags,$TorL)
    } else {
	set elemList [getEleTags]
    }
    foreach elem $elemList {
	if {$fromStorage} {
	    set eleNodes $ElementConnectivity($elem,$TorL)
	} else {
	    set eleNodes [eleNodes $elem]
	}
	
	set nodeI [lindex $eleNodes 0]
	set nodeJ [lindex $eleNodes 1]
	
	set XI [expr $nodes($nodeI,X)]
	set YI [expr $nodes($nodeI,Y)]
	set XJ [expr $nodes($nodeJ,X)]
	set YJ [expr $nodes($nodeJ,Y)]
	
	#$c create line $XI $YI $XJ $YJ -tag shearDiagram
    }

    global columnProps
    set N $columnProps(N)

    global soilProps
    set H $soilProps(subgradeHeight)

    set radius [expr {2.0*$units(in)}]

    global columnElems
    global pileElems
    global analysisProps
    set SSIon $analysisProps(soilStructure)

    for {set i 1} {$i <= $N} {incr i} {

       set maxMPos 0; set maxMPosY 0; set maxMPosX 0
       set maxMNeg 0; set maxMNegY 0; set maxMNegX 0
       set zeroMPosY 0
       
       foreach elem $columnElems($i,$TorL) {
	   if {$fromStorage} {
	       set eleNodes $ElementConnectivity($elem,$TorL)
	   } else {
	       set eleNodes [eleNodes $elem]
	   }
	   set ndI [lindex $eleNodes 0]
	   set ndJ [lindex $eleNodes 1]
	   
	   set XI [expr $nodes($ndI,X)]
	   set YI [expr $nodes($ndI,Y)]
	   set XJ [expr $nodes($ndJ,X)]
	   set YJ [expr $nodes($ndJ,Y)]
	   
	   if {$fromStorage} {
	       set MI [expr -$BendingMoments($elem,VI,$TorL)]
	       set MJ $BendingMoments($elem,VJ,$TorL)
	   } else {
	       set MI [expr -[eleForce $elem 1]]
	       set MJ [eleForce $elem 4]
	   }
	   
	   if {$MJ > $maxMPos} {
	       set maxMPos $MJ
	       set maxMPosY $YJ
	   }
	   if {$MJ < $maxMNeg} {
	       set maxMNeg $MJ
	       set maxMNegY $YJ
	   }
	   if {$MI > $maxMPos} {
	       set maxMPos $MI
	       set maxMPosY $YI
	   }
	   if {$MI < $maxMNeg} {
	       set maxMNeg $MI
	       set maxMNegY $YI
	   }
	   
	   if {[expr $MI*$MJ] < 0} {
	       set YInfl [expr {$YI - $MI*($YJ-$YI)/($MJ-$MI)}]
	       $c create oval [expr {$XI-$radius}] [expr {$YInfl-$radius}] [expr {$XI+$radius}] [expr {$YInfl+$radius}] -fill black -tag shearDiagram
	       $c create text $XI $YInfl -text " [format %.1f [expr {$YInfl-$H}]] ft" -anchor nw -tag shearDiagram       
	   }
	   
	   set MI [expr {$MI*$mag}]
	   set MJ [expr {$MJ*$mag}]
	   $c create line $XI $YI $XJ $YJ -tag shearDiagram
	   $c create line [expr {$XI+$MI}] $YI [expr {$XJ+$MJ}] $YJ -tag shearDiagram
       }
       
       
       foreach elem $pileElems($i,$TorL) {
	   if {$fromStorage} {
	       set eleNodes $ElementConnectivity($elem,$TorL)
	   } else {
	       set eleNodes [eleNodes $elem]
	   }
	   set ndI [lindex $eleNodes 0]
	   set ndJ [lindex $eleNodes 1]
	   
	   set XI [expr $nodes($ndI,X)]
	   set YI [expr $nodes($ndI,Y)]
	   set XJ [expr $nodes($ndJ,X)]
	   set YJ [expr $nodes($ndJ,Y)]
	   
	   if {$fromStorage} {
	       set MI [expr -$BendingMoments($elem,VI,$TorL)]
	       set MJ $BendingMoments($elem,VJ,$TorL)
	   } else {
	       set MI [expr -[eleForce $elem 1]]
	       set MJ [eleForce $elem 4]
	   }
	   
	   if {$MJ > $maxMPos} {
	       set maxMPos $MJ
	       set maxMPosY $YJ
	   }
	   if {$MJ < $maxMNeg} {
	       set maxMNeg $MJ
	       set maxMNegY $YJ
	   }
	   if {$MI > $maxMPos} {
	       set maxMPos $MI
	       set maxMPosY $YI
	   }
	   if {$MI < $maxMNeg} {
	       set maxMNeg $MI
	       set maxMNegY $YI
	   }
	   
	   # Make sure we are only drawing first occurrence in pile region
	   if {$zeroMPosY == 0 && [expr {$MI*$MJ}] < 0 && $YI < $maxMPosY && $YI < $H} {
	       set zeroMPosY $YI
	   }
	   
	   set MI [expr {$MI*$mag}]
	   set MJ [expr {$MJ*$mag}]
	   $c create line $XI $YI $XJ $YJ -tag shearDiagram
	   $c create line [expr {$XI+$MI}] $YI [expr {$XJ+$MJ}] $YJ -tag shearDiagram
	   #$c create oval [expr {$XI + $MI-$radius}] [expr {$YI-$radius}] [expr {$XI + $MI+$radius}] [expr {$YI+$radius}] -fill black -tag shearDiagram
	   #$c create oval [expr {$XJ + $MJ-$radius}] [expr {$YJ-$radius}] [expr {$XJ + $MJ+$radius}] [expr {$YJ+$radius}] -fill black -tag shearDiagram
       }
       set X [expr {$XI + $maxMPos*$mag}]
       $c create text $X $maxMPosY -text " [format %.1f $maxMPos] kip" -anchor sw -tag shearDiagram
       $c create text $X $maxMPosY -text " [format %.1f [expr {$maxMPosY-$H}]] ft" -anchor nw -tag shearDiagram
       $c create oval [expr {$X-$radius}] [expr {$maxMPosY-$radius}] [expr {$X+$radius}] [expr {$maxMPosY+$radius}] -fill black -tag shearDiagram
       
       set X [expr {$XI + $maxMNeg*$mag}]
       $c create text $X $maxMNegY -text " [format %.1f [expr -$maxMNeg]] kip" -anchor sw -tag shearDiagram
       $c create text $X $maxMNegY -text " [format %.1f [expr {$maxMNegY-$H}]] ft" -anchor nw -tag shearDiagram
       $c create oval [expr {$X-$radius}] [expr {$maxMNegY-$radius}] [expr {$X+$radius}] [expr {$maxMNegY+$radius}] -fill black -tag shearDiagram
       
       if {$SSIon && $zeroMPosY >= [expr 1.0*$units(ft)]} {
	   #$c create text $XI $zeroMPosY -text " V = 0" -anchor sw -tag shearDiagram
	   $c create text $XI $zeroMPosY -text " [format %.1f [expr {$zeroMPosY-$H}]] ft" -anchor sw -tag shearDiagram       
	   $c create oval [expr {$XI-$radius}] [expr {$zeroMPosY-$radius}] [expr {$XI+$radius}] [expr {$zeroMPosY+$radius}] -fill black -tag shearDiagram
       }
   }
    global pixelPerFeet
    
    $c scale shearDiagram 0 0 [expr $pixelPerFeet(X)] [expr -$pixelPerFeet(Y)]
    $c move shearDiagram 0 $height
}

# TorL indicates (L)ongitudinal or (T)ransverse model
proc DefineOpenSeesModel {{TorL T}} {

    global units
    foreach var [array names units] {
	set $var $units($var)
    }

    global hingePropsTMP
    set Gf $hingePropsTMP(Gf)
    set lp $hingePropsTMP(lp)

    global sectionPropsTMP
    set Dj [expr {$sectionPropsTMP(Dj,1)*$in}]
    set tj [expr {$sectionPropsTMP(tj,1)*$in}]

    global bentcapPropsTMP
    set bcap [expr {$bentcapPropsTMP(bcap)*$in}]
    set dcap [expr {$bentcapPropsTMP(dcap)*$in}]

    
    global soilProps
    global soilPropsTMP
    
    set subgradeHeight $soilProps(subgradeHeight)
    set waterTableDepth $soilProps(waterTableDepth)
    set Nlayers $soilProps(Nlayers)
    if {$Nlayers <= 0} {
	return
    }

    
    global columnProps
    global columnPropsTMP
    
    set L [expr {$columnProps(L)*$ft}]
    set N $columnProps(N)
    if {$N <= 0} {
	return
    }

    set arrowLength [expr {5*$ft}]
    set totalBentHeight [expr {$subgradeHeight + $L + $dcap + $arrowLength}]

    set Dc [expr {$Dj-2*$tj}]
    set Dc2 [expr {0.5*$Dc}]
    set Dj2 [expr {0.5*$Dj}]

    set bentCapOverhang [expr {1.0*$ft}]
    set capoverhangL [expr {$bentcapPropsTMP(leftoverhang)*$in}]
    set capoverhangR [expr {$bentcapPropsTMP(rightoverhang)*$in}]

    set columnWidth 0.0
    for {set i 1} {$i <= $N} {incr i} {
	set columnWidth [expr {$columnWidth + $columnProps(space,$i)}]
    }
    set totalBentWidth [expr {$columnWidth + $Dj2 + $capoverhangL + $capoverhangR}]


    #set x [expr {0.5*($totalBentHeight-$totalBentWidth)}]
    set x [expr {$Dj2 + $capoverhangL + 0.5*0.2*$L}]
    #set x 0
    set x0 $x

    set totalSoilDepth 0.0
    for {set i 1} {$i <= $soilProps(Nlayers)} {incr i} {
	set totalSoilDepth [expr {$totalSoilDepth + $soilProps(depth,$i)}]
    }

    wipe

    model basic -ndm 2 -ndf 3
    
    set linearTransfTag 1
    geomTransf Linear $linearTransfTag
    set corotTransfTag 2
    geomTransf PDelta $corotTransfTag
    
    global analysisProps
    # Turn off large-disp if specified by user
    if {$analysisProps(pDelta) == 0} {
       set corotTransfTag $linearTransfTag
    }

    set CISSSectionTag [CISSSection]
    set GapSectionTag [GapSection]
    set PileSectionTag [PileSection]
    DefineOpenSeesSection $CISSSectionTag $GapSectionTag $PileSectionTag

    set bentcapSectionTag 4
    global concretePropsTMP
    set fc $concretePropsTMP(fc)
    set eco $concretePropsTMP(eco)
    set fc [expr {$fc*$ksi}]
    set Ec [expr {57*sqrt($fc/$psi)*$ksi}]

    section Elastic $bentcapSectionTag [expr 1000*$Ec] [expr {$bcap*$dcap}] [expr {$bcap*pow($dcap,3)/12.0}]

    global gapSection ;# Int.Pt. of gap section to be monitored for strain limit states
    global jacketSection ;# Likewise for jacket section
    global sectionsOfInterest ;# Concatenation of two above

    global columnElems

    set totalHeight [expr {$subgradeHeight+$L}]
	
    if {$TorL == "T"} {
	node 0 [expr $x-$Dj2-$capoverhangL] [expr {$totalHeight+0.5*$dcap}]
    }

    set startElem [expr 2*$N+1]

    global bentCapElems
    set bentCapElems ""

    # Draw columns
    for {set i 1} {$i <= $N} {incr i} {

	set x [expr {$x + $columnProps(space,$i)}]
	
	set ndI [expr {5*$i-4}]
	set ndJ [expr {5*$i-3}]
	set ndK [expr {5*$i-2}]
	
	node $ndI $x $subgradeHeight ;# Ground
	node $ndJ $x $totalHeight ;# Top of column
	if {$TorL == "T"} {
	    node $ndK $x [expr {$totalHeight+0.5*$dcap}] ;# Middle of bent cap
	} else {
	    node $ndK $x [expr {$totalHeight+$dcap}] ;# Top of bent cap
	}
	
	# Turn off SSI if specified to do so by user
	if {$analysisProps(soilStructure) == 0} {
	    fix $ndI 1 1 1
	}
	
	# Column
	#element forceBeamColumn [expr {2*$i-1}] $ndI $ndJ $corotTransfTag RegularizedHinge Lobatto 5 $CISSSectionTag 0.0 0.0 $jacketlessSectionTag $lp [expr {0.015*$L*$ft}] $jacketSectionTag; set gapSection 5; set jacketSection 1
	element forceBeamColumn [expr {2*$i-1}] $ndI $ndJ $corotTransfTag HingeRadau $CISSSectionTag $Dc $GapSectionTag $lp $CISSSectionTag; set gapSection 6; set jacketSection 1
	#element forceBeamColumn [expr {2*$i-1}] $ndI $ndJ $corotTransfTag Lobatto $CISSSectionTag 5; set gapSection 5; set jacketSection 1
	
	# Bent rigid link (vertical)
	element elasticForceBeamColumn [expr {2*$i}] $ndJ $ndK $linearTransfTag Legendre $bentcapSectionTag 2

	#set columnElems($i,$TorL) "[expr {2*$i}] [expr {2*$i-1}]" ;# Includes bent cap
	set columnElems($i,$TorL) "[expr {2*$i-1}]"

	set ndLeft  [expr {5*$i-1}]
	set ndRight [expr {5*$i-0}]
	if {$TorL == "T"} {
	    node $ndLeft  [expr {$x-$Dj2}] [expr {$totalHeight+0.5*$dcap}]
	    node $ndRight [expr {$x+$Dj2}] [expr {$totalHeight+0.5*$dcap}]
	    incr startElem
	    element forceBeamColumn $startElem [expr 5*$i-5] [expr 5*$i-1] $linearTransfTag Legendre $bentcapSectionTag 2
	    lappend bentCapElems $startElem
	    incr startElem
	    element forceBeamColumn $startElem [expr 5*$i-1] [expr 5*$i-2] $linearTransfTag Legendre $bentcapSectionTag 2
	    lappend bentCapElems $startElem
	    incr startElem
	    element forceBeamColumn $startElem [expr 5*$i-2] [expr 5*$i-0] $linearTransfTag Legendre $bentcapSectionTag 2
	    lappend bentCapElems $startElem
	}
    }

    set sectionsOfInterest "$gapSection $jacketSection"

    # Filler trusses for longitudinal analysis
    for {set i 2} {$i <= $N} {incr i} {
	set ndI [expr {5*($i-1)-2}]
	set ndJ [expr {5*$i-2}]
	if {$TorL == "L"} {
	    incr startElem
	    element truss $startElem $ndI $ndJ $bentcapSectionTag
	}
    }
    
    # Create last node and element for overhang
    if {$TorL == "T"} {
	node [expr 5*$N+1] [expr $x+$Dj2+$capoverhangR] [expr {$totalHeight+0.5*$dcap}]
	incr startElem
	element forceBeamColumn $startElem [expr 5*$N] [expr 5*$N+1] $linearTransfTag Legendre $bentcapSectionTag 2
	lappend bentCapElems $startElem
    }

    # Apply gravity loads
    pattern Plain 1 Constant {
	for {set i 1} {$i <= $N} {incr i} {
	    load [expr {5*$i-2}] 0.0 [expr -abs($columnProps(axialLoad,$i))] 0.0
	}
    }

    # Define mass
    set g [expr 32.2*$ft/$sec**2]
    set massDL $analysisProps(massDL)
    for {set i 1} {$i <= $N} {incr i} {
	set P $columnProps(axialLoad,$i)
	set m [expr $P*$massDL/$g]
	mass [expr {5*$i-2}] $m $m 0.0
    }


    set x $x0

    set matTag 10

    global pileElems

    set startElem [lindex [lsort -real [getEleTags]] end]
    set startNode [lindex [lsort -real [getNodeTags]] end]
    # Loop over column/piles
    for {set i 1} {$i <= $N} {incr i} {

	# p-multiplier for shadowing
	set pmult $columnProps(pMult$TorL,$i)

	set x [expr {$x + $columnProps(space,$i)}]
	
	set pileElems($i,$TorL) " "
	
	# Don't create any soil elements if they're not needed
	if {$analysisProps(soilStructure) == 0} {
	    continue
	}
	
	set ndI [expr {5*$i-4}]
	
	set depthPile $sectionPropsTMP(changePile) ;# depth where section changes from column to pile
	set startDepth 0.0
	for {set j 1} {$j <= $soilProps(Nlayers)} {incr j} {
	    set layerDepth $soilPropsTMP(depth,$j)
	    set endDepth [expr {$startDepth + $layerDepth}]
	    set spacing $soilProps(space,$j)
	    
	    set NpyLayers [expr int($layerDepth/$spacing)]
	    set r [expr {0.5*($layerDepth-$spacing*$NpyLayers)}]
	    
	    set numSprings 0
	    
	    # Do first spring if there is a remainder layer
	    if {$r > 0.0} {
		set depth [expr {$startDepth + 0.5*$r}]
		set secTag $PileSectionTag
		if {$depth < $depthPile} {
		    set secTag $CISSSectionTag
		}

		OpenSeesSoilSpring $matTag $depth $r $pmult
		
		set ndJ [incr startNode]
		node $ndJ $x [expr {$subgradeHeight-$depth}]
		
		set ndJ2 [incr startNode]
		node $ndJ2 $x [expr {$subgradeHeight-$depth}]
		fix $ndJ2 1 1 1
		
		element dispBeamColumn [incr startElem] $ndJ $ndI $linearTransfTag Legendre $secTag 2
		lappend pileElems($i,$TorL) $startElem
		element zeroLength [incr startElem] $ndJ $ndJ2 -mat $matTag -dir 1
		
		set ndI $ndJ
		
		incr matTag
		
		incr numSprings
	    }
	    
	    # Do springs in main p-y layers
	    for {set k 1} {$k <= $NpyLayers} {incr k} {
		set depth [expr {$startDepth + $r + ($k-0.5)*$spacing}]
		set secTag $PileSectionTag
		if {$depth < $depthPile} {
		    set secTag $CISSSectionTag
		}

		OpenSeesSoilSpring $matTag $depth $spacing $pmult
		
		set ndJ [incr startNode]
		node $ndJ $x [expr {$subgradeHeight-$depth}]
		
		set ndJ2 [incr startNode]
		node $ndJ2 $x [expr {$subgradeHeight-$depth}]
		fix $ndJ2 1 1 1
		
		element dispBeamColumn [incr startElem] $ndJ $ndI $linearTransfTag Legendre $secTag 2
		lappend pileElems($i,$TorL) $startElem
		element zeroLength [incr startElem] $ndJ $ndJ2 -mat $matTag -dir 1
		
		set ndI $ndJ
		
		incr matTag
		
		incr numSprings
	    }
	    
	    # Do last spring if there is a remainder layer
	    if {$r > 0.0} {
		set depth [expr {$endDepth - 0.5*$r}]
		set secTag $PileSectionTag
		if {$depth < $depthPile} {
		    set secTag $CISSSectionTag
		}
		
		OpenSeesSoilSpring $matTag $depth $r $pmult
		
		set ndJ [incr startNode]
		node $ndJ $x [expr {$subgradeHeight-$depth}]
		
		set ndJ2 [incr startNode]
		node $ndJ2 $x [expr {$subgradeHeight-$depth}]
		fix $ndJ2 1 1 1
		
		element dispBeamColumn [incr startElem] $ndJ $ndI $linearTransfTag Legendre $secTag 2
		lappend pileElems($i,$TorL) $startElem
		element zeroLength [incr startElem] $ndJ $ndJ2 -mat $matTag -dir 1
		
		set ndI $ndJ
		
		incr matTag
		
		incr numSprings
	    }
	    
	    set startDepth $endDepth
	}
	
	fix $ndJ 0 1 0
    }


    foreach nd [getNodeTags] {
	#puts $nd
    }

    set x $x0
}


DefineModel [getCanvas]
