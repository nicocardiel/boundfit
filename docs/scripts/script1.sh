#!/bin/bash
if [ -e script1.out ] \rm -f script1.out
touch .running_BoundFit
boundfit << end_boundfit
example.dat
0
0
1
2
0
y
y
1
n
5
1000.0
2.0
0.0
0.0
1
1E-5
2000
n
1
0.0095
0.3028
1000
script1.out
0
end_boundfit
\rm -f .running_BoundFit
