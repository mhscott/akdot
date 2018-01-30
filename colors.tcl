proc Assign_colors {} {
    global colors
    global colorsTMP

    foreach var [array names colorsTMP] {
	set colors($var) $colorsTMP($var)
    }
}

proc AssignTMP_colors {} {
    global colors
    global colorsTMP

    foreach var [array names colors] {
	set colorsTMP($var) $colors($var)
    }
}

set colors(steel,default) #834b43
set colors(concrete,default) grey
set colors(ground,default) brown
set colors(pile,default) #aa4b43

set colors(steel) $colors(steel,default)
set colors(concrete) $colors(concrete,default)
set colors(ground) $colors(ground,default)
set colors(pile) $colors(pile,default)
AssignTMP_colors

proc ChooseColor {whichColor} {

    upvar $whichColor color

    set tmpColor [tk_chooseColor -initialcolor $color]

    if {[string length $tmpColor] > 0} {
       set color $tmpColor
    }
}

proc RestoreDefaultColors {} {
    global colors
    global colorsTMP    

    set colorsTMP(steel) $colors(steel,default)
    set colorsTMP(concrete) $colors(concrete,default)
    set colorsTMP(ground) $colors(ground,default)
    set colorsTMP(pile) $colors(pile,default)

    Assign_colors
}
