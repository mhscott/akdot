proc ReadFile {filename} {
    global outFile

    if {[file exists $filename]} {
      set inFile [open $filename r]
    } else {
      return -1
    }

    while {[gets $inFile line] >= 0} {

      # Trim out whitespace for processing
      set line2 [string trim $line]

	if {[lindex [split $line2] 0] == "source"} {
	    if {[ReadFile [lindex [split $line2] 1]] < 0} {
            puts $outFile $line ;# Output line anyway, could be a legit "source" command
          }
	} else {
          # Need to check if there is a line continuation (\)
          set line2 [string trimright $line] ;# Trim white space on right
          if {[string index $line2 end] == "\\"} {
            set line2 [string trimright $line2 "\\"] ;# Remove \ at end of line
    	      puts -nonewline $outFile "$line2 "
          } else {
            puts $outFile $line
          }
	}

    }

    close $inFile

    return 0
}

set outFile [open zCISS_Feb02_2018.tcl w]

ReadFile main.tcl

puts $outFile ""
puts $outFile "\# pdf4tcl license -- reproduced by MHS"
puts $outFile ""
if {$tcl_platform(platform) == "unix" || $tcl_platform(platform) == "linux"} {
   set inFile [open /nfs/depot/cce_u1/scott/mhscott/pdf4tcl05/licence.terms r]
} else {
   set inFile [open m:\\pdf4tcl05\\licence.terms r]
}
while {[gets $inFile line] >= 0} {
   puts $outFile "# $line"    
}
puts $outFile ""


puts $outFile ""
puts $outFile "\# snit license -- reproduced by MHS"
puts $outFile ""
if {$tcl_platform(platform) == "unix" || $tcl_platform(platform) == "linux"} {
   set inFile [open /nfs/depot/cce_u1/scott/mhscott/snit1.0/license.txt r]
} else {
   set inFile [open m:\\snit1.0\\license.txt r]
}
while {[gets $inFile line] >= 0} {
   puts $outFile "# $line"    
}
puts $outFile ""


close $outFile
