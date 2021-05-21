#===========================================
if(exist('i')==0 || i<0) i=imin
filename=sprintf("BlackHoleCamera_%05d.dat",i)

timelabel=sprintf("test  degree = %3f",i)
unset label
#===========================================



set pm3d
set pm3d map
set logscale cb
set format cb "%.1f"
set palette rgbformulae 22,13,-31
set cblabel 'Log(Intencity)'
set palette define (0 'black', 1 'navy', 2 'blue', 3 'cyan', 4 'white')

#
set title 'Black Hole Shot'
set colorbox vertical user origin 0.01,0.15 size 0.04,0.6

#ブラックホールのカメラ映像

splot filename using 1:2:3 #pm3d title ''

#=========================================================

i = i + delta_i
if(i<imax) reread
undefine i