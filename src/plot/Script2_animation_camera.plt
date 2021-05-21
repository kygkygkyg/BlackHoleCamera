reset
set terminal gif animate delay 12
set output 'iwanaga.gif'

offset = 0
imin = 0
imax = 90
delta_i = 1

load 'iwanaga.py'

imin = 90
imax = 0
delta_i = -1

load 'iwanaga2.py'

##imin = 180
##imax = 90
##delta_i = -1

#load 'iwanaga.py'

#imin = 90
#imax = 180
#delta_i = 1

#load 'iwanaga.py'