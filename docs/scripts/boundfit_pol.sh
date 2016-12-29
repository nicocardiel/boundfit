#!/bin/bash
#
# $1: input data file
# $2: polynomial degree
# $3: asymmetry coefficient (xi)
# $4: power for distances (alpha)
# $5: power for errors (beta)
# $6: cut-off parameter for errors (tau)
# $7: side: 1=upper, 2=lower
# $8: Nmaxiter
# $9: output file name
if [ -e $9 ]
then
  \rm -f $9
fi
touch .running_BoundFit
boundfit << end_boundfit
$1
0
0
1
2
0
y
y
1
n
$2
$3
$4
$5
$6
$7
1E-5
$8
n
1


1000
$9
0
end_boundfit
\rm -f .running_BoundFit
