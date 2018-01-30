proc CenterText {p text {xOffset 0}} {
   global pageInfo
   set xCenter $pageInfo(xCenter)
   if {$xOffset != 0} {
      set xCenter $xOffset
   }
   $p text $text -x [expr ($xCenter - [$p getStringWidth $text]/2.0)]
}
proc LeftText {p text {xOffset 0}} {
   global pageInfo
   set xLeft $pageInfo(xLeft)
   $p text $text -x [expr $xLeft + $xOffset]
}
proc RightText {p text {xOffset 0}} {
   global pageInfo
   set xRight $pageInfo(xRight)
   if {$xOffset != 0} {
      set xRight $xOffset
   }
   $p text $text -x [expr ($xRight - [$p getStringWidth $text])]
}
proc WriteHeader {p} {
   global pageInfo
   $p setFont 10 "Times-Roman"
   $p setTextPosition 0 [expr -0.5*$pageInfo(margin)]
   RightText $p "[BridgeName]"; $p newLine
   RightText $p "[EngineerName]"
}
proc WritePageNumber {p num} {
   global pageInfo
   $p setFont 10 "Times-Roman"
   $p setTextPosition 0 [expr $pageInfo(ySize)-1.5*$pageInfo(margin)]
   CenterText $p $num 
}
proc ReportDataLine {p label value {units ""}} {
   global pageInfo
   set xCenter $pageInfo(xCenter)
   LeftText $p $label; RightText $p $value $xCenter; LeftText $p " $units" $xCenter
}

proc CreateReport {w} {

    global PDFtypelist
    global wd
    global pageInfo

    if {[IsModelDirty] || [IsSectionDirty]} {
       set answer [tk_messageBox -message "Bridge model has changed since last save.  Save now?" \
           -icon question -type yesnocancel -default yes -title "Save Bridge file"]

       switch $answer {
          yes {
             SaveFile
             set tmp [tk_messageBox -message "Bridge file saved, click OK to create report" \
                 -icon info -type ok -default ok -title "Create Report"]
          }
          no {
             set tmp [tk_messageBox -message "Bridge file not saved, click OK to create report" \
                 -icon info -type ok -default ok -title "Create Report"]
          }
          cancel return
       }
    }

    set openFile [tk_getSaveFile -filetypes $PDFtypelist -initialdir $wd -initialfile [BridgeName] \
        -defaultextension .pdf -parent . -title "Create Report"]
    
    if {[llength $openFile] <= 0} {
	return
    }

    set wd [file dirname $openFile]

    set iReport 0
    set NreportSteps 18

    global stopReport
    set stopReport 0

    createTopLevel $w "Summary Report"

    set reportWindowOpen 1
    bind $w <Destroy> {set reportWindowOpen 0; set stopReport 1}

    frame $w.report
    label $w.report.label
    grid $w.report.label -row 0 -column 0 -sticky ew
    label $w.report.update
    grid $w.report.update -row 1 -column 0 -sticky ew

    set stopReport 0
    button $w.report.cancel -text "Stop" -command "set stopReport 1"
    grid $w.report.cancel -row 2 -column 0 -sticky ew

    label $w.report.warning -text "Please disregard changes to main screen \n during report generation"
    grid $w.report.warning -row 3 -column 0 -sticky ew

    pack $w.report -side top



# A canvas/graph for drawing plots in report
set gWidth 600
set gHeight 600
set graph foo
set gCanvas [createEMUGraph "" $graph $gWidth $gHeight]

set size [pdf4tcl::getPaperSize a4]
set xSize [lindex $size 0]
set ySize [lindex $size 1]
set pageInfo(xSize) $xSize
set pageInfo(ySize) $ySize

set margin 72.0
set pageInfo(margin) $margin
set pageInfo(xLeft) 0
set xCenter [expr $xSize/2.0 - $margin]
set pageInfo(xCenter) $xCenter
set pageInfo(xRight) [expr $xSize - 2.0*$margin]

pdf4tcl::new p1 -paper a4 -margin $margin

set pageNumber 0

#----- Title Page -----

$w.report.label config -text "Creating Title Page"
$w.report.update config -text "Step: [incr iReport]/$NreportSteps"
update

if {$stopReport == 1} {
   set c [getCanvas]
   DefineModel $c

   destroy $w

    
   destroy $gCanvas

   p1 write -file $openFile
   p1 finish
   p1 destroy

   return
}

p1 startPage 
p1 setTextPosition 0 0

p1 setFont 18 "Times-Roman"
CenterText p1 [BridgeName]; p1 newLine
p1 newLine
CenterText p1 "Bridge Analysis Summary"; p1 newLine

for {set i 1} {$i <= 12} {incr i} {
    p1 newLine
}
p1 setFont 14
CenterText p1 [EngineerName]; p1 newLine
CenterText p1 [DateTime]; p1 newLine


for {set i 1} {$i <= 6} {incr i} {
   p1 newLine
}
p1 setFont 12
CenterText p1 "[SoftwareTitle], v[VersionNumber]"; p1 newLine
for {set i 1} {$i <= 6} {incr i} {
   p1 newLine
}

p1 setFont 14
CenterText p1 "Contents"; p1 newLine
p1 setFont 12
LeftText p1 "Bent Overview"; RightText p1 2; p1 newLine
LeftText p1 "Transverse Pushover Analysis"; RightText p1 3; p1 newLine
LeftText p1 "Longitudinal Pushover Analysis"; RightText p1 7; p1 newLine
global sectionProps
if {$sectionProps(pileSameAsColumn) == 1} {
    LeftText p1 "CISS/Pile Section Details"; RightText p1 11; p1 newLine
    set page 11
} else {
    LeftText p1 "CISS Section Details"; RightText p1 11; p1 newLine
    LeftText p1 "Pile Section Details"; RightText p1 12; p1 newLine
    set page 12
}
LeftText p1 "Gap Section Moment-Curvature"; RightText p1 [incr page]; p1 newLine
LeftText p1 "Section Axial-Moment Interaction"; RightText p1 [incr page]; p1 newLine
LeftText p1 "Longitudinal Reinforcing Steel"; RightText p1 [incr page]; p1 newLine
LeftText p1 "Steel Shell"; RightText p1 [incr page]; p1 newLine
LeftText p1 "Concrete"; RightText p1 [incr page]; p1 newLine
LeftText p1 "Soil Layers"; RightText p1 [incr page]; p1 newLine

incr pageNumber

#----- Picture of Bent -----

$w.report.label config -text "Creating Bent Image"
$w.report.update config -text "Step: [incr iReport]/$NreportSteps"
update

if {$stopReport == 1} {
   set c [getCanvas]
   DefineModel $c

   destroy $w

    
   destroy $gCanvas

   p1 write -file $openFile
   p1 finish
   p1 destroy

   return
}

p1 startPage
WriteHeader p1
p1 setTextPosition 0 0

p1 setFont 18 "Times-Roman"
CenterText p1 "Bent Overview"; p1 newLine
p1 newLine
p1 setFont 12

global columnProps
ReportDataLine p1 "Column Height"            [format %.1f $columnProps(L)] "ft above grade"; p1 newLine
global bentcapProps
ReportDataLine p1 "Cap Beam depth"           [format %.1f $bentcapProps(dcap)] "in"; p1 newLine
ReportDataLine p1 "Cap Beam width"           [format %.1f $bentcapProps(bcap)] "in"; p1 newLine
ReportDataLine p1 "Cap Overhang left"        [format %.1f $bentcapProps(leftoverhang)] "in (clear from column face)"; p1 newLine
ReportDataLine p1 "Cap Overhang right"       [format %.1f $bentcapProps(rightoverhang)] "in (clear from column face)"; p1 newLine
global hingeProps
ReportDataLine p1 "Shell Gap, Gf"            [format %.1f $hingeProps(Gf)] in; p1 newLine
ReportDataLine p1 "Plastic Hinge Length, lp" [format %.2f $hingeProps(lp)] ft; p1 newLine
global soilProps
ReportDataLine p1 "Water Table"              [format %.1f $soilProps(waterTableDepth)] "ft below grade"; p1 newLine
#ReportDataLine p1 "Frozen Soil"              [format %.1f $soilProps(frozenSoilDepth)] "ft below grade"; p1 newLine
ReportDataLine p1 "Total Soil Depth"         [format %.1f $soilProps(subgradeHeight)]  "ft below grade"; p1 newLine

set c .tmpc
destroy $c
canvas $c -height 400 -width 400
set width [winfo reqwidth $c]
set height [winfo reqheight $c]

DefineModel $c off

p1 canvas $c -y [expr 2*18 + 7*12 + 72/2] -bbox "0 0 $width $height" -width [expr $xSize-2.0*$margin] -height [expr $ySize-2.0*$margin]

clearCanvas $c

WritePageNumber p1 [incr pageNumber]

#----- Transverse Pushover Analysis -----

$w.report.label config -text "Transverse Pushover Analysis"
$w.report.update config -text "Step: [incr iReport]/$NreportSteps (may take a few seconds)"
update

if {$stopReport == 1} {
   set c [getCanvas]
   DefineModel $c

   destroy $w

    
   destroy $gCanvas

   p1 write -file $openFile
   p1 finish
   p1 destroy

   return
}

p1 startPage
WriteHeader p1
p1 setTextPosition 0 0

p1 setFont 18 "Times-Roman"
CenterText p1 "Transverse Pushover Analysis"; p1 newLine
p1 newLine
p1 setFont 12

set coords [OpenSeesPushoverAnalysis T]

global units
set ft $units(ft)
set in $units(in)

global analysisProps
global columnProps
set maxLateralDisp [expr $analysisProps(lateralDisp,T)*$ft]
set whichLimit ""

global sectionProps
set Dj [expr $sectionProps(Dj,1)*$in]

if {$Dj > $maxLateralDisp} {
   #set maxLateralDisp $Dj
   #set whichLimit "(Column Diameter)"
}

ReportDataLine p1 "Max Displacement $whichLimit" $maxLateralDisp ft
p1 newLine

set PDyn On
if {$analysisProps(pDelta) == 0} {
   set PDyn Off
}
ReportDataLine p1 "P-Delta Analysis" $PDyn
p1 newLine

set SSIyn On
if {$analysisProps(soilStructure) == 0} {
   set SSIyn Off
}
ReportDataLine p1 "Soil-Structure Interaction" $SSIyn
p1 newLine

set H2Oyn On
if {$analysisProps(subtractWater) == 0} {
   set H2Oyn Off
}
ReportDataLine p1 "Soil Buoyant Weight below GWT" $H2Oyn
p1 newLine

ReportDataLine p1 "Overstrength Factor" $analysisProps(overstrength)
p1 newLine
p1 newLine

global analysisStates

ReportDataLine p1 "R/F Steel Yield"  " "
if {$analysisStates(steelYield,T,disp) > 0} {
   RightText p1 "[format %.2f $analysisStates(steelYield,T,disp)] ft, [format %.1f $analysisStates(steelYield,T,load)] kip"
} else {
   RightText p1 "N/A"
}
p1 newLine

ReportDataLine p1 "R/F Steel Strain Limit ($analysisStates(steelLimit,val) in/in)"  " "
if {$analysisStates(steelLimit,T,disp) > 0} {
   RightText p1 "[format %.2f $analysisStates(steelLimit,T,disp)] ft, [format %.1f $analysisStates(steelLimit,T,load)] kip"
} else {
   RightText p1 "N/A"
}
p1 newLine

ReportDataLine p1 "Steel Shell Strain Limit ($analysisStates(jacketLimit,val) in/in)"  " "
if {$analysisStates(jacketLimit,T,disp) > 0} {
   RightText p1 "[format %.2f $analysisStates(jacketLimit,T,disp)] ft, [format %.1f $analysisStates(jacketLimit,T,load)] kip"
} else {
   RightText p1 "N/A"
}
p1 newLine

ReportDataLine p1 "Concrete Strain Limit ($analysisStates(concreteLimit,val) in/in)"  " "
if {$analysisStates(concreteLimit,T,disp) > 0} {
   RightText p1 "[format %.2f $analysisStates(concreteLimit,T,disp)] ft, [format %.1f $analysisStates(concreteLimit,T,load)] kip"
} else {
   RightText p1 "N/A"
}
p1 newLine

ReportDataLine p1 "Peak Capacity" " "
if {$analysisStates(maxLoad,T,disp) > 0} {
   RightText p1 "[format %.2f $analysisStates(maxLoad,T,disp)] ft, [format %.1f $analysisStates(maxLoad,T,load)] kip"
} else {
   RightText p1 "N/A"
}
p1 newLine

global analysisProps

ReportDataLine p1 "Post Peak Limit ($analysisProps(postPeak)\% Peak)"  " "
if {$analysisStates(postPeak,T,disp) > 0} {
   RightText p1 "@ [format %.1f $analysisStates(postPeak,T,load)] kip"
} else {
   RightText p1 "N/A"
}
p1 newLine
p1 newLine

set gCanvas [createEMUGraph "" $graph $gWidth $gHeight]

$graph data d2 -colour blue -points 0 -lines 1 -coords $coords

set emax 0
set smax 0
foreach {e s} $coords {
   if {$e > $emax} {set emax $e}
   if {$s > $smax} {set smax $s}
}

axesEMUGraph $graph $emax $smax
$graph redraw

PlotLimitStates $graph $gCanvas T

set graphInfo(title) ""
set graphInfo(xlabel) "Displacement (ft)"
set graphInfo(ylabel) "Load (kip)"
labelEMUGraph $gCanvas $graph graphInfo $gWidth $gHeight

p1 canvas $gCanvas -y [expr 2*18 + 9*12 + 72/2] -width [expr $xSize-2.0*$margin] -height [expr $ySize-2.0*$margin]

destroy $gCanvas

WritePageNumber p1 [incr pageNumber]


#----- Final Displaced Shape -----

$w.report.label config -text "Drawing Final Displaced Shape"
$w.report.update config -text "Step: [incr iReport]/$NreportSteps"
update

if {$stopReport == 1} {
   set c [getCanvas]
   DefineModel $c

   destroy $w

    
   destroy $gCanvas

   p1 write -file $openFile
   p1 finish
   p1 destroy

   return
}

p1 startPage
WriteHeader p1
p1 setTextPosition 0 0

p1 setFont 18 "Times-Roman"
CenterText p1 "Final Transverse Displaced Shape"; p1 newLine
p1 newLine
p1 setFont 12

global extremeAxialLoad

set TorL T

ReportDataLine p1 "Extreme Axial Loads" " "
global columnProps
global extremeAxialLoad
for {set i 1} {$i <= $columnProps(N)} {incr i} {
    puts $extremeAxialLoad($i,max,$TorL)
    puts $extremeAxialLoad($i,min,$TorL)
    set tmpMin [format %.1f $extremeAxialLoad($i,min,$TorL)]
    set tmpMax [format %.1f $extremeAxialLoad($i,max,$TorL)]
    set outputString "Column $i:"
    if {$tmpMin >= 0} {
       set outputString "$outputString $tmpMin kip (C)"
    } else {
       set outputString "$outputString [expr abs($tmpMin)] kip (T)"
    }
    if {$tmpMax >= 0} {
       set outputString "$outputString to $tmpMax kip (C)"
    } else {
       set outputString "$outputString to [expr abs($tmpMax)] kip (T)"
    }
    RightText p1 $outputString
    p1 newLine
}

p1 newLine

set N $columnProps(N)

global bentCapForces
# Make sure there is more than one column
if {$N > 1} {
    ReportDataLine p1 "Bent Cap Forces" " "
    set N [format %.1f $bentCapForces(trail,N)]
    set TorC T
    if {$N < 0} {set TorC C}
    set V [format %.1f $bentCapForces(trail,V)]
    set M [format %.1f $bentCapForces(trail,M)]
    RightText p1 "Inside trailing pile: N=[expr abs($N)] kip ($TorC), V=$V kip, M=$M kip-ft"
    p1 newLine
    set N [format %.1f $bentCapForces(lead,N)]
    set TorC T
    if {$N < 0} {set TorC C}
    set V [format %.1f $bentCapForces(lead,V)]
    set M [format %.1f $bentCapForces(lead,M)]
    RightText p1 "Inside leading pile: N=[expr abs($N)] kip ($TorC), V=$V kip, M=$M kip-ft"
    p1 newLine
    if {$analysisProps(lateralLoad) == 1} {
	RightText p1 "Lateral load DISTRIBUTED across bent cap"
    } else {
	RightText p1 "Lateral load CONCENTRATED at bent cap-column joints"	
    }
}


set c [getCanvas]

clearCanvas $c
DefineModel $c off
DrawDisplacedShape $c T

set width [winfo reqwidth $c]
set height [winfo reqheight $c]
p1 canvas $c -y [expr 2*18 + 5*12 + 72/2] -bbox "0 0 $width $height" -width [expr $xSize-2.0*$margin] -height [expr $ySize-2.0*$margin]
#clearCanvas $c

WritePageNumber p1 [incr pageNumber]


#----- Final Moment Diagram -----

$w.report.label config -text "Drawing Final Moment Diagram"
$w.report.update config -text "Step: [incr iReport]/$NreportSteps"
update

if {$stopReport == 1} {
   set c [getCanvas]
   DefineModel $c

   destroy $w

    
   destroy $gCanvas

   p1 write -file $openFile
   p1 finish
   p1 destroy

   return
}

p1 startPage
WriteHeader p1
p1 setTextPosition 0 0

p1 setFont 18 "Times-Roman"
CenterText p1 "Final Transverse Moment Diagram"; p1 newLine
p1 newLine
p1 setFont 12


set c [getCanvas]

clearCanvas $c
DefineModel $c off
DrawMomentDiagram $c T

set width [winfo reqwidth $c]
set height [winfo reqheight $c]
p1 canvas $c -y [expr 2*18 + 5*12 + 72/2] -bbox "0 0 $width $height" -width [expr $xSize-2.0*$margin] -height [expr $ySize-2.0*$margin]
#clearCanvas $c

WritePageNumber p1 [incr pageNumber]



#----- Final Shear Diagram -----

$w.report.label config -text "Drawing Final Shear Diagram"
$w.report.update config -text "Step: [incr iReport]/$NreportSteps"
update

if {$stopReport == 1} {
   set c [getCanvas]
   DefineModel $c

   destroy $w

    
   destroy $gCanvas

   p1 write -file $openFile
   p1 finish
   p1 destroy

   return
}

p1 startPage
WriteHeader p1
p1 setTextPosition 0 0

p1 setFont 18 "Times-Roman"
CenterText p1 "Final Transverse Shear Diagram"; p1 newLine
p1 newLine
p1 setFont 12


set c [getCanvas]

clearCanvas $c
DefineModel $c off
DrawShearDiagram $c T

set width [winfo reqwidth $c]
set height [winfo reqheight $c]
p1 canvas $c -y [expr 2*18 + 5*12 + 72/2] -bbox "0 0 $width $height" -width [expr $xSize-2.0*$margin] -height [expr $ySize-2.0*$margin]
#clearCanvas $c

WritePageNumber p1 [incr pageNumber]


#----- Longitudinal Pushover Analysis -----

$w.report.label config -text "Longitudinal Pushover Analysis"
$w.report.update config -text "Step: [incr iReport]/$NreportSteps (may take a few seconds)"
update

if {$stopReport == 1} {
   set c [getCanvas]
   DefineModel $c

   destroy $w

    
   destroy $gCanvas

   p1 write -file $openFile
   p1 finish
   p1 destroy

   return
}

p1 startPage
WriteHeader p1
p1 setTextPosition 0 0

p1 setFont 18 "Times-Roman"
CenterText p1 "Longitudinal Pushover Analysis"; p1 newLine
p1 newLine
p1 setFont 12

set coords [OpenSeesPushoverAnalysis L]

global units
set ft $units(ft)
set in $units(in)

global analysisProps
global columnProps
set maxLateralDisp [expr $analysisProps(lateralDisp,L)*$ft]
set whichLimit ""

global sectionProps
set Dj [expr $sectionProps(Dj,1)*$in]

if {$Dj > $maxLateralDisp} {
   #set maxLateralDisp $Dj
   #set whichLimit "(Column Diameter)"
}

ReportDataLine p1 "Max Lateral Disp $whichLimit" $maxLateralDisp ft
p1 newLine

set PDyn On
if {$analysisProps(pDelta) == 0} {
   set PDyn Off
}
ReportDataLine p1 "P-Delta Analysis" $PDyn
p1 newLine

set SSIyn On
if {$analysisProps(soilStructure) == 0} {
   set SSIyn Off
}
ReportDataLine p1 "Soil-Structure Interaction" $SSIyn
p1 newLine

set H2Oyn On
if {$analysisProps(subtractWater) == 0} {
   set H2Oyn Off
}
ReportDataLine p1 "Soil Buoyant Weight below GWT" $H2Oyn
p1 newLine

ReportDataLine p1 "Overstrength Factor" $analysisProps(overstrength)
p1 newLine
p1 newLine

global analysisStates

ReportDataLine p1 "R/F Steel Yield"  " "
if {$analysisStates(steelYield,L,disp) > 0} {
   RightText p1 "[format %.2f $analysisStates(steelYield,L,disp)] ft, [format %.1f $analysisStates(steelYield,L,load)] kip"
} else {
   RightText p1 "N/A"
}
p1 newLine

ReportDataLine p1 "R/F Steel Strain Limit ($analysisStates(steelLimit,val) in/in)"  " "
if {$analysisStates(steelLimit,L,disp) > 0} {
   RightText p1 "[format %.2f $analysisStates(steelLimit,L,disp)] ft, [format %.1f $analysisStates(steelLimit,L,load)] kip"
} else {
   RightText p1 "N/A"
}
p1 newLine

ReportDataLine p1 "Steel Shell Strain Limit ($analysisStates(jacketLimit,val) in/in)"  " "
if {$analysisStates(jacketLimit,L,disp) > 0} {
   RightText p1 "[format %.2f $analysisStates(jacketLimit,L,disp)] ft, [format %.1f $analysisStates(jacketLimit,L,load)] kip"
} else {
   RightText p1 "N/A"
}
p1 newLine

ReportDataLine p1 "Concrete Strain Limit ($analysisStates(concreteLimit,val) in/in)"  " "
if {$analysisStates(concreteLimit,L,disp) > 0} {
   RightText p1 "[format %.2f $analysisStates(concreteLimit,L,disp)] ft, [format %.1f $analysisStates(concreteLimit,L,load)] kip"
} else {
   RightText p1 "N/A"
}
p1 newLine

ReportDataLine p1 "Peak Capacity" " "
if {$analysisStates(maxLoad,L,disp) > 0} {
   RightText p1 "[format %.2f $analysisStates(maxLoad,L,disp)] ft, [format %.1f $analysisStates(maxLoad,L,load)] kip"
} else {
   RightText p1 "N/A"
}
p1 newLine

global analysisProps

ReportDataLine p1 "Post Peak Limit ($analysisProps(postPeak)\% Peak)"  " "
if {$analysisStates(postPeak,L,disp) > 0} {
   RightText p1 "@ [format %.1f $analysisStates(postPeak,L,load)] kip"
} else {
   RightText p1 "N/A"
}
p1 newLine
p1 newLine

set gCanvas [createEMUGraph "" $graph $gWidth $gHeight]

$graph data d2 -colour blue -points 0 -lines 1 -coords $coords

set emax 0
set smax 0
foreach {e s} $coords {
   if {$e > $emax} {set emax $e}
   if {$s > $smax} {set smax $s}
}

axesEMUGraph $graph $emax $smax
$graph redraw

PlotLimitStates $graph $gCanvas L

set graphInfo(title) ""
set graphInfo(xlabel) "Displacement (ft)"
set graphInfo(ylabel) "Load (kip)"
labelEMUGraph $gCanvas $graph graphInfo $gWidth $gHeight

p1 canvas $gCanvas -y [expr 2*18 + 9*12 + 72/2] -width [expr $xSize-2.0*$margin] -height [expr $ySize-2.0*$margin]

destroy $gCanvas

WritePageNumber p1 [incr pageNumber]


#----- Final Displaced Shape -----

$w.report.label config -text "Drawing Final Displaced Shape"
$w.report.update config -text "Step: [incr iReport]/$NreportSteps"
update

if {$stopReport == 1} {
   set c [getCanvas]
   DefineModel $c

   destroy $w

    
   destroy $gCanvas

   p1 write -file $openFile
   p1 finish
   p1 destroy

   return
}

p1 startPage
WriteHeader p1
p1 setTextPosition 0 0

p1 setFont 18 "Times-Roman"
CenterText p1 "Final Longitudinal Displaced Shape"; p1 newLine
p1 newLine
p1 setFont 12


set c [getCanvas]

clearCanvas $c
DefineModel $c off
DrawDisplacedShape $c L

set width [winfo reqwidth $c]
set height [winfo reqheight $c]
p1 canvas $c -y [expr 2*18 + 5*12 + 72/2] -bbox "0 0 $width $height" -width [expr $xSize-2.0*$margin] -height [expr $ySize-2.0*$margin]
#clearCanvas $c

WritePageNumber p1 [incr pageNumber]


#----- Final Moment Diagram -----

$w.report.label config -text "Drawing Final Moment Diagram"
$w.report.update config -text "Step: [incr iReport]/$NreportSteps"
update

if {$stopReport == 1} {
   set c [getCanvas]
   DefineModel $c

   destroy $w

    
   destroy $gCanvas

   p1 write -file $openFile
   p1 finish
   p1 destroy

   return
}

p1 startPage
WriteHeader p1
p1 setTextPosition 0 0

p1 setFont 18 "Times-Roman"
CenterText p1 "Final Longitudinal Moment Diagram"; p1 newLine
p1 newLine
p1 setFont 12


set c [getCanvas]

clearCanvas $c
DefineModel $c off
DrawMomentDiagram $c L

set width [winfo reqwidth $c]
set height [winfo reqheight $c]
p1 canvas $c -y [expr 2*18 + 5*12 + 72/2] -bbox "0 0 $width $height" -width [expr $xSize-2.0*$margin] -height [expr $ySize-2.0*$margin]
#clearCanvas $c

WritePageNumber p1 [incr pageNumber]



#----- Final Shear Diagram -----

$w.report.label config -text "Drawing Final Shear Diagram"
$w.report.update config -text "Step: [incr iReport]/$NreportSteps"
update

if {$stopReport == 1} {
   set c [getCanvas]
   DefineModel $c

   destroy $w

    
   destroy $gCanvas

   p1 write -file $openFile
   p1 finish
   p1 destroy

   return
}

p1 startPage
WriteHeader p1
p1 setTextPosition 0 0

p1 setFont 18 "Times-Roman"
CenterText p1 "Final Longitudinal Shear Diagram"; p1 newLine
p1 newLine
p1 setFont 12


set c [getCanvas]

clearCanvas $c
DefineModel $c off
DrawShearDiagram $c L

set width [winfo reqwidth $c]
set height [winfo reqheight $c]
p1 canvas $c -y [expr 2*18 + 5*12 + 72/2] -bbox "0 0 $width $height" -width [expr $xSize-2.0*$margin] -height [expr $ySize-2.0*$margin]
#clearCanvas $c

WritePageNumber p1 [incr pageNumber]


#----- Section Details -----

$w.report.label config -text "Drawing Section Details"
$w.report.update config -text "Step: [incr iReport]/$NreportSteps"
update

if {$stopReport == 1} {
   set c [getCanvas]
   DefineModel $c

   destroy $w

    
   destroy $gCanvas

   p1 write -file $openFile
   p1 finish
   p1 destroy

   return
}



p1 startPage
WriteHeader p1
p1 setTextPosition 0 0

p1 setFont 18 "Times-Roman"
if {$sectionProps(pileSameAsColumn) == 1} {
    CenterText p1 "CISS/Pile Section Details"; p1 newLine
} else {
    CenterText p1 "CISS Section Details"; p1 newLine
}
p1 newLine
p1 setFont 12

global sectionProps

global units
set in $units(in)

set ds $sectionProps(ds,1)
set barArea [BarArea $ds]
set Nbars $sectionProps(Nbars,1)
set steelArea [expr {$Nbars*$barArea}]

set pi [expr {2*asin(1.0)}]
set Dj [expr {$sectionProps(Dj,1)*$in}]
set tj [expr {$sectionProps(tj,1)*$in}]
set Dc [expr {$Dj-2*$tj}]

set cover [expr {$sectionProps(cover,1)*$in}]

set sectionArea [expr {0.25*$pi*$Dc*$Dc}]
set rhol [expr {$steelArea/$sectionArea}]

# Now go back to original units for report
set Dj $sectionProps(Dj,1)
set tj $sectionProps(tj,1)
set cover $sectionProps(cover,1)

ReportDataLine p1 "Outer Shell Diamter, Dj" [format %.2f $Dj]    in; p1 newLine
ReportDataLine p1 "Shell Thickness, tj"     [format %.2f $tj]    in; p1 newLine
p1 newLine

if {$sectionProps(noConcrete,1) == 0} {
    ReportDataLine p1 "Clear Cover"             [format %.2f $cover] in; p1 newLine
    ReportDataLine p1 "Number of Bars"          $Nbars                 ; p1 newLine
    ReportDataLine p1 "Bar Size"                $ds                    ; p1 newLine
    ReportDataLine p1 "Long. R/F Ratio"         [format %.5f $rhol]    ; p1 newLine
    p1 newLine
    
    ReportDataLine p1 "Spiral Size"  $sectionProps(dshoop,1)    ; p1 newLine
    ReportDataLine p1 "Hoop Spacing" [format %.2f $sectionProps(sphoop,1)]  in; p1 newLine
    ReportDataLine p1 "Yield Stress" [format %.2f $sectionProps(fyhoop,1)] ksi; p1 newLine
} else {
    ReportDataLine p1 "No concrete in column" ""
}

global whichSection
set whichSection 1
DrawSection $c off

p1 canvas $c -y [expr 2*18 + 10*12 + 72/2] -width [expr $xSize-2.0*$margin] -height [expr $ySize-2.0*$margin]

clearCanvas $c

WritePageNumber p1 [incr pageNumber]



if {$sectionProps(pileSameAsColumn) == 0} {
    p1 startPage
    WriteHeader p1
    p1 setTextPosition 0 0
    
    p1 setFont 18 "Times-Roman"
    CenterText p1 "Pile Section Details"; p1 newLine
    p1 newLine
    p1 setFont 12
    
    global sectionProps
    
    global units
    set in $units(in)
    
    set ds $sectionProps(ds,2)
    set barArea [BarArea $ds]
    set Nbars $sectionProps(Nbars,2)
    set steelArea [expr {$Nbars*$barArea}]
    
    set pi [expr {2*asin(1.0)}]
    set Dj [expr {$sectionProps(Dj,2)*$in}]
    set tj [expr {$sectionProps(tj,2)*$in}]
    set Dc [expr {$Dj-2*$tj}]
    
    set cover [expr {$sectionProps(cover,2)*$in}]
    
    set sectionArea [expr {0.25*$pi*$Dc*$Dc}]
    set rhol [expr {$steelArea/$sectionArea}]
    
    # Now go back to original units for report
    set Dj $sectionProps(Dj,2)
    set tj $sectionProps(tj,2)
    set cover $sectionProps(cover,2)
    
    ReportDataLine p1 "Outer Shell Diamter, Dj" [format %.2f $Dj]    in; p1 newLine
    ReportDataLine p1 "Shell Thickness, tj"     [format %.2f $tj]    in; p1 newLine
    p1 newLine

    if {$sectionProps(noConcrete,2) == 0} {
	ReportDataLine p1 "Clear Cover"             [format %.2f $cover] in; p1 newLine
	ReportDataLine p1 "Number of Bars"          $Nbars                 ; p1 newLine
	ReportDataLine p1 "Bar Size"                $ds                    ; p1 newLine
	ReportDataLine p1 "Long. R/F Ratio"         [format %.5f $rhol]    ; p1 newLine
	p1 newLine
	
	ReportDataLine p1 "Spiral Size"  $sectionProps(dshoop,2)    ; p1 newLine
	ReportDataLine p1 "Hoop Spacing" [format %.2f $sectionProps(sphoop,2)]  in; p1 newLine
	ReportDataLine p1 "Yield Stress" [format %.2f $sectionProps(fyhoop,2)] ksi; p1 newLine
    } else {
	ReportDataLine p1 "No concrete in pile" ""
    }

    global whichSection
    set whichSection 2
    DrawSection $c off
    
    p1 canvas $c -y [expr 2*18 + 10*12 + 72/2] -width [expr $xSize-2.0*$margin] -height [expr $ySize-2.0*$margin]
    
    clearCanvas $c
    
    WritePageNumber p1 [incr pageNumber]
}


#----- Moment-Curvature @ Maximum Axial Load -----

$w.report.label config -text "Calculating M-phi at Max. Axial Load"
$w.report.update config -text "Step: [incr iReport]/$NreportSteps"
update

if {$stopReport == 1} {
   set c [getCanvas]
   DefineModel $c

   destroy $w

    
   destroy $gCanvas

   p1 write -file $openFile
   p1 finish
   p1 destroy

   return
}

p1 startPage
WriteHeader p1
p1 setTextPosition 0 0

global bentCapForces
global extremeAxialLoad
set maxAxialLoadT $extremeAxialLoad(1,max,T)
set whichMax 1
set whenMax $extremeAxialLoad(1,max,when,T)
set minAxialLoadT $extremeAxialLoad(1,min,T)
set whichMin 1
set whenMin $extremeAxialLoad(1,min,when,T)
for {set i 2} {$i <= $columnProps(N)} {incr i} {
   set axialLoad $extremeAxialLoad($i,max,T)
   if {$axialLoad > $maxAxialLoadT} {
      set maxAxialLoadT $axialLoad
      set whichMax $i
   }
   set axialLoad $extremeAxialLoad($i,min,T)
   if {$axialLoad < $minAxialLoadT} {
      set minAxialLoadT $axialLoad
      set whichMin $i
   }
}

set coords1 [OpenSeesMKAnalysis [GapSection] $maxAxialLoadT]
set coords2 [OpenSeesMKAnalysis [GapSection] $minAxialLoadT]
set maxAxialLoadL $extremeAxialLoad(1,max,L) ;# same for all columns in Long. direction and will not change much wrt time
set coords3 [OpenSeesMKAnalysis [GapSection] $maxAxialLoadL]

p1 setFont 18 "Times-Roman"
CenterText p1 "Gap Section Moment-Curvature"; p1 newLine
p1 newLine
p1 setFont 12

if {$hingeProps(coreConfined) == 1} {
   ReportDataLine p1 "Core Confinement" "Steel Shell"; p1 newLine
} else {
   ReportDataLine p1 "Core Confinement" "Spiral Hoop"; p1 newLine
}
if {$hingeProps(coverConfined) == 1} {
   ReportDataLine p1 "Cover Confinement" "Same as Core"; p1 newLine
} else {
   ReportDataLine p1 "Cover Confinement" "None"; p1 newLine
}
p1 newLine
ReportDataLine p1 "Target Ductility" $sectionProps(MKductility); p1 newLine
p1 newLine

set minAxialLoadT [format %.1f $minAxialLoadT]
set maxAxialLoadT [format %.1f $maxAxialLoadT]
set maxAxialLoadL [format %.1f $maxAxialLoadL]

set gCanvas [createEMUGraph "" $graph $gWidth $gHeight]

ReportDataLine p1 "Extreme Axial Loads" " "
if {$minAxialLoadT >= 0} {
   RightText p1 "Transverse Min: $minAxialLoadT kip (C) in Column $whichMin @ [format %.1f $extremeAxialLoad($whichMin,min,when,T)] kip"
} else {
   RightText p1 "Transverse Min: [expr abs($minAxialLoadT)] kip (T) in Column $whichMin @ [format %.1f $extremeAxialLoad($whichMin,min,when,T)] kip"
}
p1 newLine
if {[llength $coords2] <= 4} {
   RightText p1 "WARNING: Axial load ($minAxialLoadT kip) too large to produce moment-curvature plot"; p1 newLine
} else {
   $graph data d2 -colour red  -points 0 -lines 1 -coords $coords2
}

if {$maxAxialLoadT >= 0} {
   RightText p1 "Transverse Max: $maxAxialLoadT kip (C) in Column $whichMax @ [format %.1f $extremeAxialLoad($whichMax,max,when,T)] kip"
} else {
   RightText p1 "Transverse Max: [expr abs($maxAxialLoadT)] kip (T) in Column $whichMax @ [format %.1f $extremeAxialLoad($whichMax,max,when,T)] kip"
}
p1 newLine
if {[llength $coords1] <= 4} {
   RightText p1 "WARNING: Axial load ($maxAxialLoadT kip) too large to produce moment-curvature plot"; p1 newLine
} else {
   $graph data d1 -colour blue -points 0 -lines 1 -coords $coords1
}

if {$maxAxialLoadL >= 0} {
   RightText p1 "Longitudinal: $maxAxialLoadL kip (C)"
} else {
   RightText p1 "Longitudinal: [expr abs($maxAxialLoadL)] kip (T)"
}
p1 newLine
if {[llength $coords1] <= 4} {
   RightText p1 "WARNING: Axial load ($maxAxialLoadL kip) too large to produce moment-curvature plot"; p1 newLine
} else {
   $graph data d3 -colour black -points 0 -lines 1 -coords $coords3
}

set emax 0
set smax 0
foreach {e s} "$coords1 $coords2 $coords3" {
   if {$e > $emax} {set emax $e}
   if {$s > $smax} {set smax $s}
}

axesEMUGraph $graph $emax $smax
$graph redraw

set xC [lindex $coords1 end-1]
set yC [lindex $coords1 end]
$gCanvas create text [$graph x2canvas $xC] [$graph y2canvas $yC] \
	-text "Trans. Max" -anchor ne
set xC [lindex $coords2 end-1]
set yC [lindex $coords2 end]
$gCanvas create text [$graph x2canvas $xC] [$graph y2canvas $yC] \
	-text "Trans. Min" -anchor ne
set xC [lindex $coords3 end-1]
set yC [lindex $coords3 end]
$gCanvas create text [$graph x2canvas $xC] [$graph y2canvas $yC] \
	-text "Long." -anchor ne

set graphInfo(title) ""
set graphInfo(xlabel) "Curvature (1/ft)"
set graphInfo(ylabel) "Moment (kip-ft)"
labelEMUGraph $gCanvas $graph graphInfo $gWidth $gHeight

p1 canvas $gCanvas -y [expr 2*18 + 7*12 + 72/2] -width [expr $xSize-2.0*$margin] -height [expr $ySize-2.0*$margin]

destroy $gCanvas

WritePageNumber p1 [incr pageNumber]


#----- Section N-M Interaction -----

p1 startPage
WriteHeader p1
p1 setTextPosition 0 0

p1 setFont 18 "Times-Roman"
CenterText p1 "Section Axial-Moment Interaction"; p1 newLine
p1 newLine
p1 setFont 12

global hingeProps

ReportDataLine p1 "Gap Region" ""; p1 newLine
if {$hingeProps(coreConfined) == 1} {
   ReportDataLine p1 "Core Confinement" "Steel Shell"; p1 newLine
} else {
   ReportDataLine p1 "Core Confinement" "Spiral Hoop"; p1 newLine
}
if {$hingeProps(coverConfined) == 1} {
   ReportDataLine p1 "Cover Confinement" "Same as Core"; p1 newLine
} else {
   ReportDataLine p1 "Cover Confinement" "None"; p1 newLine
}
p1 newLine

set gCanvas [createEMUGraph "" $graph $gWidth $gHeight]

$w.report.label config -text "Gap Section N-M Interaction"
$w.report.update config -text "Step: [incr iReport]/$NreportSteps (may take a few seconds)"
update

set coordsGap [OpenSeesNMAnalysis 2]

if {$stopReport == 1} {
   set c [getCanvas]
   DefineModel $c

   destroy $w

    
   destroy $gCanvas

   p1 write -file $openFile
   p1 finish
   p1 destroy

   return
}

$w.report.label config -text "CISS Section N-M Interaction"
$w.report.update config -text "Step: [incr iReport]/$NreportSteps (may take a few seconds)"
update

set coordsCISS [OpenSeesNMAnalysis 1]


set coordsPile ""
if {$sectionProps(pileSameAsColumn) == 0} {
    $w.report.label config -text "Pile Section N-M Interaction"
    #$w.report.update config -text "Step: [incr iReport]/$NreportSteps (may take a few seconds)"
    update
    
    set coordsPile [OpenSeesNMAnalysis 3]
}

set Nmin 0
set Nmax 0
set Mmax 0

# Figure out clean axis labels
foreach {M N} "$coordsCISS $coordsGap $coordsPile" {
   if {$N < $Nmin} {set Nmin $N}
   if {$N > $Nmax} {set Nmax $N}
   if {$M > $Mmax} {set Mmax $M}
}

axesEMUGraph $graph $Mmax $Nmax 0 $Nmin

if {$stopReport == 1} {
   set c [getCanvas]
   DefineModel $c

   destroy $w

    
   destroy $gCanvas

   p1 write -file $openFile
   p1 finish
   p1 destroy

   return
}

$graph data d2 -colour blue -points 0 -lines 1 -coords $coordsGap
$graph data d1 -colour red -points 0 -lines 1 -coords $coordsCISS
if {$sectionProps(pileSameAsColumn) == 0} {
    $graph data d3 -colour blue -points 0 -lines 1 -coords $coordsPile
}
$graph redraw

$graph hmark 0.0 tagZero black

if {$sectionProps(pileSameAsColumn) == 1} {
    set xC [lindex $coordsCISS 2]
    set yC [lindex $coordsCISS 3]
    $gCanvas create text [$graph x2canvas $xC] [$graph y2canvas $yC] \
	-text "CISS/Pile Section" -anchor sw
} else {
    set xC [lindex $coordsCISS 2]
    set yC [lindex $coordsCISS 3]
    $gCanvas create text [$graph x2canvas $xC] [$graph y2canvas $yC] \
	-text "CISS Section" -anchor sw

    set xC [lindex $coordsPile 2]
    set yC [lindex $coordsPile 3]
    $gCanvas create text [$graph x2canvas $xC] [$graph y2canvas $yC] \
	-text "Pile Section" -anchor sw
}

set xC [lindex $coordsGap 2]
set yC [lindex $coordsGap 3]
$gCanvas create text [$graph x2canvas $xC] [$graph y2canvas $yC] \
	-text "Gap Section" -anchor sw

set Mmax 0; set NatMmax 0
if {$sectionProps(pileSameAsColumn) == 1} {
    ReportDataLine p1 "CISS/Pile Section" ""
} else {
    ReportDataLine p1 "CISS Section" ""
}
p1 newLine
foreach {M N} $coordsCISS {
   if {$N == 0} {
      set xC [format %.1f $M]
      set yC [format %.1f $N]
      $gCanvas create text [$graph x2canvas $xC] [$graph y2canvas $yC] \
         -text "($xC,$yC)" -anchor nw
      ReportDataLine p1 "Capacity @ N = 0" "$xC kip-ft"
      p1 newLine
   }
   if {$M > $Mmax} {
      set Mmax $M
      set NatMmax $N
   }
}
set xC [format %.1f $Mmax]
set yC [format %.1f $NatMmax]
$gCanvas create text [$graph x2canvas $xC] [$graph y2canvas $yC] \
   -text "($xC,$yC)" -anchor w
ReportDataLine p1 "Balance Point @ N = $yC kip" "$xC kip-ft"
p1 newLine
p1 newLine


set Mmax 0; set NatMmax 0
if {$sectionProps(pileSameAsColumn) == 0} {
    ReportDataLine p1 "Pile Section" ""
    p1 newLine
    foreach {M N} $coordsPile {
	if {$N == 0} {
	    set xC [format %.1f $M]
	    set yC [format %.1f $N]
	    $gCanvas create text [$graph x2canvas $xC] [$graph y2canvas $yC] \
		-text "($xC,$yC)" -anchor nw
	    ReportDataLine p1 "Capacity @ N = 0" "$xC kip-ft"
	    p1 newLine
	}
	if {$M > $Mmax} {
	    set Mmax $M
	    set NatMmax $N
	}
    }
    set xC [format %.1f $Mmax]
    set yC [format %.1f $NatMmax]
    $gCanvas create text [$graph x2canvas $xC] [$graph y2canvas $yC] \
	-text "($xC,$yC)" -anchor w
    ReportDataLine p1 "Balance Point @ N = $yC kip" "$xC kip-ft"
    p1 newLine
    p1 newLine
}


set Mmax 0; set NatMmax 0
ReportDataLine p1 "Gap Section" ""
p1 newLine
foreach {M N} $coordsGap {
   if {$N == 0} {
      set xC [format %.1f $M]
      set yC [format %.1f $N]
      $gCanvas create text [$graph x2canvas $xC] [$graph y2canvas $yC] \
         -text "($xC,$yC)" -anchor nw
      ReportDataLine p1 "Capacity @ N = 0" "$xC kip-ft"
      p1 newLine
   }
   if {$M > $Mmax} {
      set Mmax $M
      set NatMmax $N
   }
}
set xC [format %.1f $Mmax]
set yC [format %.1f $NatMmax]
$gCanvas create text [$graph x2canvas $xC] [$graph y2canvas $yC] \
   -text "($xC,$yC)" -anchor w
ReportDataLine p1 "Balance Point @ N = $yC kip" "$xC kip-ft"
p1 newLine
p1 newLine


global columnProps
set Ncol $columnProps(N)
for {set i 1} {$i <= $Ncol} {incr i} {
   #$graph hmark $columnProps(axialLoad,$i) tagZero$i red
}

set graphInfo(xlabel) "\nM (kip-ft)"
set graphInfo(ylabel) "N (kip)"
labelEMUGraph $gCanvas $graph graphInfo $gWidth $gHeight

p1 canvas $gCanvas -y [expr 2*18 + 9*12 + 72/2] -width [expr $xSize-2.0*$margin] -height [expr $ySize-2.0*$margin]

destroy $gCanvas

WritePageNumber p1 [incr pageNumber]


#----- Long. R/F Steel -----

$w.report.label config -text "Drawing Long. R/F Steel Stress-Strain"
$w.report.update config -text "Step: [incr iReport]/$NreportSteps"
update

if {$stopReport == 1} {
   set c [getCanvas]
   DefineModel $c

   destroy $w

    
   destroy $gCanvas

   p1 write -file $openFile
   p1 finish
   p1 destroy

   return
}

p1 startPage
WriteHeader p1
p1 setTextPosition 0 0

p1 setFont 18 "Times-Roman"
CenterText p1 "Longitudinal Reinforcing Steel"; p1 newLine
p1 newLine
p1 setFont 12

global rfsteelProps
ReportDataLine p1 "Elastic Modulus, E"               $rfsteelProps(E)      ksi; p1 newLine
ReportDataLine p1 "Yield Stress, fye"                $rfsteelProps(fye)    ksi; p1 newLine
ReportDataLine p1 "Ultimate Stress, fue"             $rfsteelProps(fue)    ksi; p1 newLine
ReportDataLine p1 "Strain-Hardening Onset, epssh"    $rfsteelProps(epssh)  in/in; p1 newLine
ReportDataLine p1 "Strain-Hardening Coefficient, C1" $rfsteelProps(C1)        ; p1 newLine
ReportDataLine p1 "Rupture Strain, epssuR"           $rfsteelProps(epssuR) in/in; p1 newLine
ReportDataLine p1 "Ultimate Strain, epssu"           $rfsteelProps(epssu)  in/in; p1 newLine

set gCanvas [createEMUGraph "" $graph $gWidth $gHeight]

set coords [OpenSeesRebarAnalysis]
$graph data d2 -colour blue -points 0 -lines 1 -coords $coords

set emax 0
set smax 0
foreach {e s} $coords {
   if {$e > $emax} {set emax $e}
   if {$s > $smax} {set smax $s}
}

axesEMUGraph $graph $emax $smax
$graph redraw

set graphInfo(xlabel) "Strain (in/in)"
set graphInfo(ylabel) "Stress (ksi)"
labelEMUGraph $gCanvas $graph graphInfo $gWidth $gHeight

p1 canvas $gCanvas -y [expr 2*18 + 7*12 + 72/2] -width [expr $xSize-2.0*$margin] -height [expr $ySize-2.0*$margin]

destroy $gCanvas

WritePageNumber p1 [incr pageNumber]


#----- Jacket Steel -----

$w.report.label config -text "Drawing Steel Shell Stress-Strain"
$w.report.update config -text "Step: [incr iReport]/$NreportSteps"
update

if {$stopReport == 1} {
   set c [getCanvas]
   DefineModel $c

   destroy $w

    
   destroy $gCanvas

   p1 write -file $openFile
   p1 finish
   p1 destroy

   return
}

p1 startPage
WriteHeader p1
p1 setTextPosition 0 0

p1 setFont 18 "Times-Roman"
CenterText p1 "Steel Shell"; p1 newLine
p1 newLine
p1 setFont 12

global jacketProps
ReportDataLine p1 "Elastic Modulus, E"               $jacketProps(E)      ksi; p1 newLine
ReportDataLine p1 "Yield Stress, fye"                $jacketProps(fye)    ksi; p1 newLine
ReportDataLine p1 "Ultimate Stress, fue"             $jacketProps(fue)    ksi; p1 newLine
ReportDataLine p1 "Strain-Hardening Onset, epssh"    $jacketProps(epssh)  in/in; p1 newLine
ReportDataLine p1 "Strain-Hardening Coefficient, C1" $jacketProps(C1)        ; p1 newLine
ReportDataLine p1 "Rupture Strain, epssuR"           $jacketProps(epssuR) in/in; p1 newLine
ReportDataLine p1 "Ultimate Strain, epssu"           $jacketProps(epssu)  in/in; p1 newLine

set gCanvas [createEMUGraph "" $graph $gWidth $gHeight]

set coords [OpenSeesJacketAnalysis]
$graph data d2 -colour blue -points 0 -lines 1 -coords $coords

set emax 0
set smax 0
foreach {e s} $coords {
   if {$e > $emax} {set emax $e}
   if {$s > $smax} {set smax $s}
}

axesEMUGraph $graph $emax $smax
$graph redraw

set graphInfo(xlabel) "Strain (in/in)"
set graphInfo(ylabel) "Stress (ksi)"
labelEMUGraph $gCanvas $graph graphInfo $gWidth $gHeight

p1 canvas $gCanvas -y [expr 2*18 + 7*12 + 72/2] -width [expr $xSize-2.0*$margin] -height [expr $ySize-2.0*$margin]

destroy $gCanvas

WritePageNumber p1 [incr pageNumber]


#----- Concrete -----

$w.report.label config -text "Drawing Concrete Stress-Strain"
$w.report.update config -text "Step: [incr iReport]/$NreportSteps"
update

if {$stopReport == 1} {
   set c [getCanvas]
   DefineModel $c

   destroy $w

    
   destroy $gCanvas

   p1 write -file $openFile
   p1 finish
   p1 destroy

   return
}

p1 startPage
WriteHeader p1
p1 setTextPosition 0 0

p1 setFont 18 "Times-Roman"
CenterText p1 "Concrete"; p1 newLine
p1 newLine
p1 setFont 12

global concreteProps
set fc $concreteProps(fc)
set eco $concreteProps(eco)
set Ec [expr {2*$fc/$eco}]
ReportDataLine p1 "Expected Concrete Strength, f\'ce" [format %.2f $fc]  ksi  ; p1 newLine
ReportDataLine p1 "Cracking Strain, eco"              [format %.4f $eco] in/in; p1 newLine
ReportDataLine p1 "Tangent Modulus, Ec"               [format %.2f $Ec]  ksi; p1 newLine
ReportDataLine p1 "Spalling Strain, esp"              [format %.4f $concreteProps(esp)] in/in; p1 newLine
p1 newLine

set fcc $concreteProps(fcc,1)
set ecc $concreteProps(ecc,1)
set fcchoop $concreteProps(fcchoop,1)
set ecchoop $concreteProps(ecchoop,1)

CenterText p1 "Steel Shell (CISS)";
RightText  p1 "Spiral Hoop"; p1 newLine

LeftText   p1 "Compressive Strength, f\'cc"
CenterText p1 "[format %.2f $fcc] ksi"
RightText  p1 "[format %.2f $fcchoop] ksi"; p1 newLine

LeftText   p1 "Peak Strain, ecc"
CenterText p1 "[format %.5f $ecc] in/in"
RightText  p1 "[format %.5f $ecchoop] in/in"; p1 newLine

LeftText   p1 "Secant Modulus, Esec"
CenterText p1 "[format %.2f [expr $fcc/$ecc]] ksi"
RightText  p1 "[format %.2f [expr $fcchoop/$ecchoop]] ksi"; p1 newLine

LeftText   p1 "Mander Coefficient, r"
CenterText p1 "[format %.2f [expr $Ec/($Ec-$fcc/$ecc)]]"
RightText  p1 "[format %.2f [expr $Ec/($Ec-$fcchoop/$ecchoop)]]"; p1 newLine


set gCanvas [createEMUGraph "" $graph $gWidth $gHeight]

set emax 0
set smax 0

set coords1 [OpenSeesConcreteAnalysis CISS]
$graph data d1 -colour blue -points 0 -lines 1 -coords $coords1
set coords2 [OpenSeesConcreteAnalysis Unconfined]
$graph data d2 -colour red -points 0 -lines 1 -coords $coords2
set coords3 [OpenSeesConcreteAnalysis Hoop]
$graph data d3 -colour black -points 0 -lines 1 -coords $coords3

foreach {e s} "$coords1 $coords2 $coords3" {
   if {$e > $emax} {set emax $e}
   if {$s > $smax} {set smax $s}
}

axesEMUGraph $graph $emax $smax
$graph redraw






set xC [lindex $coords1 end-1]
set yC [lindex $coords1 end]
$gCanvas create text [$graph x2canvas $xC] [$graph y2canvas $yC] \
	-text "CISS" -anchor ne
set xC [lindex $coords2 end-1]
set yC [lindex $coords2 end]
$gCanvas create text [$graph x2canvas $xC] [$graph y2canvas $yC] \
	-text "Unconfined" -anchor sw
set xC [lindex $coords3 end-1]
set yC [lindex $coords3 end]
$gCanvas create text [$graph x2canvas $xC] [$graph y2canvas $yC] \
	-text "Hoop" -anchor ne


set graphInfo(xlabel) "Strain (in/in)"
set graphInfo(ylabel) "Stress (ksi)"
labelEMUGraph $gCanvas $graph graphInfo $gWidth $gHeight

p1 canvas $gCanvas -y [expr 2*18 + 12*12 + 72/2] -width [expr $xSize-2.0*$margin] -height [expr $ySize-2.0*$margin]

destroy $gCanvas

WritePageNumber p1 [incr pageNumber]


#----- Soil Layers -----

$w.report.label config -text "Drawing Soil p-y Curves"
$w.report.update config -text "Step: [incr iReport]/$NreportSteps"
update

if {$stopReport == 1} {
   set c [getCanvas]
   DefineModel $c

   destroy $w

    
   destroy $gCanvas

   p1 write -file $openFile
   p1 finish
   p1 destroy

   return
}

global soilProps
set Nlayers $soilProps(Nlayers)

for {set layer 1} {$layer <= $Nlayers} {incr layer} {

   p1 startPage
   WriteHeader p1
   p1 setTextPosition 0 0

   set soilType $soilProps(type,$layer)

   p1 setFont 18 "Times-Roman"
   CenterText p1 "Soil Layer $layer: $soilType"; p1 newLine
   p1 newLine
   p1 setFont 12

   set startDepth 0.0
   for {set i 1} {$i < $layer} {incr i} {
      set startDepth [expr $startDepth + $soilProps(depth,$i)]
   }
   set endDepth [expr {$startDepth + $soilProps(depth,$layer)}]

   ReportDataLine p1 "Depth range" "$startDepth - $endDepth" ft; p1 newLine
   ReportDataLine p1 "p-Multiplier" $soilProps(multiplier,$layer) ""; p1 newLine
   p1 newLine

   if {$soilType == "Sand"} {
      ReportDataLine p1 "Unit weight, gamma"               $soilProps(gamma,$layer) pcf; p1 newLine
      ReportDataLine p1 "Angle of internal friction, phi"  $soilProps(phi,$layer)   deg; p1 newLine
      ReportDataLine p1 "Stiffness increase wrt depth, nh" $soilProps(nh,$layer)    pci; p1 newLine
   }
   if {$soilType == "N.C. Clay"} {
      ReportDataLine p1 "Unit weight, gamma"           $soilProps(gamma,$layer) pcf  ; p1 newLine
      ReportDataLine p1 "Shear strength, c"            $soilProps(c,$layer)     psi  ; p1 newLine
      ReportDataLine p1 "50\% principal strain, eps50" $soilProps(eps50,$layer) in/in; p1 newLine
   }
   if {$soilType == "O.C. Clay"} {
      ReportDataLine p1 "Unit weight, gamma"               $soilProps(gamma,$layer) pcf  ; p1 newLine
      ReportDataLine p1 "Shear strength, c"                $soilProps(c,$layer)     psi  ; p1 newLine
      ReportDataLine p1 "Stiffness increase wrt depth, nh" $soilProps(nh,$layer)    pci  ; p1 newLine
      ReportDataLine p1 "50\% principal strain, eps50"     $soilProps(eps50,$layer) in/in; p1 newLine
   }


   p1 newLine

   set spacing $soilProps(space,$layer)
     
   ReportDataLine p1 "p-y spring spacing"    $spacing ft; p1 newLine

   set gCanvas [createEMUGraph "" $graph $gWidth $gHeight]

    set startDepth 0.0
    for {set i 1} {$i < $layer} {incr i} {
	set startDepth [expr $startDepth + $soilProps(depth,$i)]
    }
    set layerDepth $soilProps(depth,$layer)
    set endDepth [expr $startDepth + $layerDepth]

    set numSprings 0

    set NpyLayers [expr int($layerDepth/$spacing)]
    set r [expr 0.5*($layerDepth-$spacing*$NpyLayers)]

    # Do first spring if there is a remainder layer
    if {$r > 0.0} {
       set depth [expr {$startDepth + 0.5*$r}]
       set py($numSprings) [OpenSeesSoilAnalysis $depth $r]
       set py($numSprings,depth) $depth
       incr numSprings
    }

    # Do springs in main p-y layes
    for {set i 1} {$i <= $NpyLayers} {incr i} {
       set depth [expr {$startDepth + $r + ($i-0.5)*$spacing}]
       set py($numSprings) [OpenSeesSoilAnalysis $depth $spacing]
       set py($numSprings,depth) $depth
       incr numSprings
    }

    # Do last spring if there is a remainder layer
    if {$r > 0.0} {
       set depth [expr {$endDepth - 0.5*$r}]
       set py($numSprings) [OpenSeesSoilAnalysis $depth $r]
       set py($numSprings,depth) $depth
       incr numSprings
    }


    set emax 0
    set smax 0
    for {set i 0} {$i < $numSprings} {incr i} {
       foreach {e s} $py($i) {
          if {$e > $emax} {set emax $e}
          if {$s > $smax} {set smax $s}
       }
    }

   ReportDataLine p1 "Number of p-y springs" $numSprings; p1 newLine

   for {set k 0} {$k < $numSprings} {incr k} {
       set color blue
       set depth $py($k,depth)
       if {$depth < $soilProps(waterTableDepth)} {
	   set color black
       }
       $graph data d$k -colour $color -points 0 -lines 1 -coords $py($k)
   }

   axesEMUGraph $graph $emax $smax
   $graph redraw

   for {set k 0} {$k < $numSprings} {incr k} {
       set x [lindex $py($k) end-1]
       set y [lindex $py($k) end]
       set depth [format %.1f $py($k,depth)]
       $gCanvas create text [$graph x2canvas $x] [$graph y2canvas $y] -text "x = $depth ft" -anchor w
   }
    
   set graphInfo(xlabel) "y (ft)"
   set graphInfo(ylabel) "p (kip/ft)"
   labelEMUGraph $gCanvas $graph graphInfo $gWidth $gHeight

   p1 canvas $gCanvas -y [expr 2*18 + 7*12 + 72/2] -width [expr $xSize-2.0*$margin] -height [expr $ySize-2.0*$margin]

   for {set k 0} {$k < $numSprings} {incr k} {
      $graph clearmark d$k
   }

   destroy $gCanvas

   WritePageNumber p1 [incr pageNumber]
}

#-----

set c [getCanvas]
DefineModel $c

if {0} {
DrawDisplacedShape $c
}

destroy $w

destroy $gCanvas

p1 write -file $openFile
p1 finish
p1 destroy

}
