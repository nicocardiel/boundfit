#!/bin/csh
if (-e script4.out) \rm -f script4.out
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
3
n
15
y
1000.0
2.0
0.0
0.0
2
1E-5
1000
1111
n
r
40
0
1
0.0094
0.3028
1000
script4.out
0
end_boundfit
\rm -f .running_BoundFit
