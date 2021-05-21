reset
#出力先の指定
set terminal png
set output 'Black_hall_shot_uniform.png'
#余白を０にする
set lmargin 0
set rmargin 0
set tmargin 0
set bmargin 0
#複数描写モード
set multiplot

#強度のグラフ
set size 0.32 , 0.57
set origin 0.65 , 0.165
set ylabel 'r'
set xlabel 'Intencity'
#set xrange[0:5]
set yrange[-10:10]
set title 'Intencity independence of r'
plot "Intencity_00070.dat" using 2:1 w d title ''

#ブラックホールのカメラ映像
set size 0.6 , 0.8
set origin 0.08 , 0.1
set ytics ('' -3,'' -2,'' -1,'' 0,'' 1,'' 2,'' 3)
set xlabel ''
set ylabel ''
set xrange[-10:10]
set yrange[-10:10]
set pm3d
set pm3d map
set logscale cb
set format cb "%.1f"

# set palette rgbformulae 22,13,-31
# set cblabel 'Log(Intencity)'
set palette define (0 'black', 1 'navy', 2 'blue', 3 'cyan', 4 'white')

set mapping cylindrical
set title 'Black Hole Shot'
set colorbox vertical user origin 0.01,0.15 size 0.04,0.6
splot "BlackHoleCamera_00070.dat" using 1:2:3 with pm3d title ''
set nomultiplot
pause -1