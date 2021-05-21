reset
#出力先の指定
# set terminal png
# set output 'Black_hall_shot_uniform.png'

#=============これ以降がアニメーションで必要な奴=============

#===========================================
if(exist('i')==0 || i<0) i=imin
filename_cm   = sprintf("BlackHoleCamera_%05d.dat",i)
filename_it   = sprintf("Intencity_%05d.dat",i)
filename_it_y = sprintf("Intencity_elv_%05d.dat",i)

phase_label=sprintf("Black Hole \n Shot degree = %3f",i)
unset label
#===========================================

#余白を０にする
set lmargin 0
set rmargin 0
set tmargin 0
set bmargin 0
#複数描写モード
set multiplot

#強度のグラフ (横方向) ==================================================================
set notitle # 'Intencity of x=0 - line'
set size 0.45 , 0.2
set origin 0.139 , 0.16
set ylabel 'r'
set xlabel 'Intencity ; y=0 - line'
set xrange[-10:10]
#set yrange[-10:10]
plot filename_it using 1:2 w d title ''

#強度のグラフ (縦方向) ==================================================================
set title 'Intencity ; x=0 - line'
set size 0.34 , 0.394
set origin 0.642 , 0.44
set ytics ('' -3,'' -2,'' -1,'' 0,'' 1,'' 2,'' 3)
set ylabel ''  # 'r'
set xlabel ''  #'Intencity'
#set xrange[0:5]
unset xrange
set yrange[-10:10]
plot filename_it_y using 2:1 w d title ''

#ブラックホールのカメラ映像 ==============================================================
set size 0.6 , 0.6
set origin 0.08 , 0.395
set ytics ('' -3,'' -2,'' -1,'' 0,'' 1,'' 2,'' 3)
set xlabel ''
set ylabel ''
set xrange[-10:10]
set yrange[-10:10]
set pm3d
set pm3d map
set logscale zcb
set format cb "%.1f"

# set palette rgbformulae 22,13,-31
# set cblabel 'Log(Intencity)'
set palette define (0 'black', 1 'navy', 2 'blue', 3 'cyan', 4 'white')

set mapping cylindrical
set title phase_label
set colorbox vertical user origin 0.01,0.45 size 0.04,0.4
splot filename_cm using 1:2:3 with pm3d title ''
set nomultiplot
# pause -1

#=========================================================

i = i + delta_i
if(i<imax) reread
undefine i