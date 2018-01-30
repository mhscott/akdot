wipe
model basic -ndm 2 -ndf 3

node 1000 0.0 0.0; fix 1000 1 1 1
node 2000 10.0 0.0; fix 2000 1 1 1

node 0 0.0 12.0

set Nele 100
set DX [expr 10.0/$Nele]

section Elastic 1 3000.0 20.0 1000.0

geomTransf Linear 1

element forceBeamColumn 1000 1000 0 1 "Lobatto 1 4"
for {set i 1} {$i <= $Nele} {incr i} {
    node $i [expr $i*$DX] 12.0
    element forceBeamColumn $i [expr $i-1] $i 1 "Lobatto 1 4"
}
element forceBeamColumn 2000 2000 $Nele 1 "Lobatto 1 4"

fix 0 0 1 1
fix $Nele 0 1 1

pattern Plain 1 Linear {
    for {set i 1} {$i <= $Nele} {incr i} {
	eleLoad -ele $i -type beamUniform 0.0 1.0
    }
}

integrator DisplacementControl $Nele 1 0.02

analysis Static

analyze 1

foreach ele [getEleTags] {
    puts [basicForce $ele]
}

reactions

puts [nodeReaction 1000 1]
puts [nodeReaction 2000 1]

puts [expr 0.02 * 2.0*12*3000*1000/12**3]
