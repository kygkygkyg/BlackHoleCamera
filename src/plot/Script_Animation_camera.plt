reset
set terminal gif animate
set output 'Animation_PhotonRing.gif'


imin = 0
imax = 90
delta_i = 1

load 'Script_BH_animation.tex'

imin = 90
imax = 0
delta_i = -1

load 'Script_BH_animation.tex'

imin = 180
imax = 90
delta_i = -1

load 'Script_BH_animation.tex'

imin = 90
imax = 180
delta_i = 1

load 'Script_BH_animation.tex'