reset
set terminal png
set output 'Schwarzschild_light_path.png'
set  polar
set size square
set xrange[-20:20]
set yrange[-20:20]
set title 'Light paths in Schwarzschild metric'
plot 'Schwarzschild_LightPath.dat' using 1:3 w d t ''

set obj circle at 0,0 fc rgb 'blue' size 1 fs transparent solid 0.4 noborder

set output 'Schwarzschild_light_path.png'
replot 1 lc black with filledcurve closed t ''
