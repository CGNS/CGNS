#!/sw/bin/gnuplot -persist
#
#    
#    	G N U P L O T
#    	Version 5.0 patchlevel 7    last modified 2017-08-16
#    
#    	Copyright (C) 1986-1993, 1998, 2004, 2007-2017
#    	Thomas Williams, Colin Kelley and many others
#    
#    	gnuplot home:     http://www.gnuplot.info
#    	faq, bugs, etc:   type "help FAQ"
#    	immediate help:   type "help"  (plot window: hit 'h')
# set terminal wxt 0 enhanced
# set output
unset clip points
set clip one
unset clip two
set bar 1.000000 front
set border 31 front lt black linewidth 1.000 dashtype solid
set zdata 
set ydata 
set xdata 
set y2data 
set x2data 
set boxwidth
set style fill  empty border
set style rectangle back fc  bgnd fillstyle   solid 1.00 border lt -1
set style circle radius graph 0.02, first 0.00000, 0.00000 
set style ellipse size graph 0.05, 0.03, first 0.00000 angle 0 units xy
set dummy x, y
set format x "% h" 
set format y "% h" 
set format x2 "% h" 
set format y2 "% h" 
set format z "% h" 
set format cb "% h" 
set format r "% h" 
set timefmt "%d/%m/%y,%H:%M"
set angles radians
set tics back
unset grid
set raxis
set style parallel front  lt black linewidth 2.000 dashtype solid
set key title "" center
set key inside right top vertical Left reverse enhanced autotitle nobox
set key noinvert samplen 4 spacing 1 width 0 height 0 
set key maxcolumns 0 maxrows 0
set key noopaque
unset label
unset arrow
set style increment userstyles
unset style line
set style line 1  linecolor rgb "#377eb8"  linewidth 2.000 dashtype solid pointtype 6 pointsize default pointinterval 0
set style line 2  linecolor rgb "#e41a1c"  linewidth 2.000 dashtype solid pointtype 4 pointsize default pointinterval 0
set style line 3  linecolor rgb "grey40"  linewidth 2.000 dashtype solid pointtype 8 pointsize default pointinterval 0
set style line 4  linecolor rgb "#4daf4a"  linewidth 2.000 dashtype solid pointtype 10 pointsize default pointinterval 0
set style line 5  linecolor rgb "#377eb8"  linewidth 2.000 dashtype 3 pointtype 12 pointsize default pointinterval 0
set style line 6  linecolor rgb "#e41a1c"  linewidth 2.000 dashtype 3 pointtype 14 pointsize default pointinterval 0
set style line 7  linecolor rgb "grey40"  linewidth 2.000 dashtype 3 pointtype 7 pointsize default pointinterval 0
set style line 8  linecolor rgb "#4daf4a"  linewidth 2.000 dashtype 3 pointtype 5 pointsize default pointinterval 0
set style line 9  linecolor rgb "#db4dff"  linewidth 2.000 dashtype solid pointtype 9 pointsize default pointinterval 0
set style line 10  linecolor rgb "#db4dff"  linewidth 2.000 dashtype 3 pointtype 11 pointsize default pointinterval 0
set style line 11  linecolor rgb "#33cccc"  linewidth 2.000 dashtype solid pointtype 13 pointsize default pointinterval 0
set style line 12  linecolor rgb "#33cccc"  linewidth 2.000 dashtype 3 pointtype 6 pointsize default pointinterval 0
set style line 13  linecolor rgb "#ff6600"  linewidth 3.000 dashtype solid pointtype 4 pointsize default pointinterval 0
set style line 14  linecolor rgb "#ff6600"  linewidth 2.000 dashtype solid pointtype 8 pointsize default pointinterval 0
set style line 23  linecolor rgb "#00857d"  linewidth 2.000 dashtype solid pointtype 10 pointsize default pointinterval 0
set style line 24  linecolor rgb "#006298"  linewidth 2.000 dashtype solid pointtype 12 pointsize default pointinterval 0
set style line 25  linecolor rgb "#298fc2"  linewidth 2.000 dashtype solid pointtype 14 pointsize default pointinterval 0
set style line 26  linecolor rgb "#563d82"  linewidth 2.000 dashtype solid pointtype 7 pointsize default pointinterval 0
set style line 27  linecolor rgb "#6f5091"  linewidth 2.000 dashtype solid pointtype 5 pointsize default pointinterval 0
set style line 28  linecolor rgb "#e35205"  linewidth 2.000 dashtype solid pointtype 9 pointsize default pointinterval 0
set style line 29  linecolor rgb "#ff7f41"  linewidth 2.000 dashtype solid pointtype 11 pointsize default pointinterval 0
set style line 30  linecolor rgb "#89813d"  linewidth 2.000 dashtype solid pointtype 13 pointsize default pointinterval 0
set style line 31  linecolor rgb "#afa96e"  linewidth 2.000 dashtype solid pointtype 15 pointsize default pointinterval 0
set style line 32  linecolor rgb "#7c878e"  linewidth 2.000 dashtype solid pointtype 14 pointsize default pointinterval 0
set style line 33  linecolor rgb "#c1c6c8"  linewidth 2.000 dashtype solid pointtype 16 pointsize default pointinterval 0
unset style arrow
set style histogram clustered gap 2 title textcolor lt -1
unset object
set style textbox transparent margins  1.0,  1.0 border
unset logscale
set logscale x 10
set offsets 0, 0, 0, 0
set pointsize 1
set pointintervalbox 1
set encoding default
unset polar
unset parametric
unset decimalsign
unset micro
unset minussign
set view 60, 30, 1, 1
set samples 100, 100
set isosamples 10, 10
set surface 
unset contour
set cntrlabel  format '%8.3g' font '' start 5 interval 20
set mapping cartesian
set datafile separator whitespace
unset hidden3d
set cntrparam order 4
set cntrparam linear
set cntrparam levels auto 5
set cntrparam points 5
set size ratio 0 1,1
set origin 0,0
set style data points
set style function lines
unset xzeroaxis
unset yzeroaxis
unset zzeroaxis
unset x2zeroaxis
unset y2zeroaxis
set xyplane relative 0.5
set tics scale  1, 0.5, 1, 1, 1
set mxtics default
set mytics default
set mztics default
set mx2tics default
set my2tics default
set mcbtics default
set mrtics default
set xtics border in scale 1,0.5 mirror norotate  autojustify
set xtics  norangelimit autofreq  font ", 20"
set ytics border in scale 1,0.5 mirror norotate  autojustify
set ytics  norangelimit autofreq  font ", 20"
set ztics border in scale 1,0.5 nomirror norotate  autojustify
set ztics  norangelimit autofreq 
unset x2tics
unset y2tics
set cbtics border in scale 1,0.5 mirror norotate  autojustify
set cbtics  norangelimit autofreq 
set rtics axis in scale 1,0.5 nomirror norotate  autojustify
set rtics  norangelimit autofreq 
unset paxis 1 tics
unset paxis 2 tics
unset paxis 3 tics
unset paxis 4 tics
unset paxis 5 tics
unset paxis 6 tics
unset paxis 7 tics
set title "" 
set title  font "" norotate
set timestamp bottom 
set timestamp "" 
set timestamp  font "" norotate
set rrange [ * : * ] noreverse nowriteback
set trange [ * : * ] noreverse nowriteback
set urange [ * : * ] noreverse nowriteback
set vrange [ * : * ] noreverse nowriteback
set xlabel "Number of Processes" 
set xlabel  font "Times Bold,24" textcolor lt -1 norotate
set x2label "" 
set x2label  font "" textcolor lt -1 norotate
set xrange [ * : * ] noreverse nowriteback
set x2range [ * : * ] noreverse nowriteback
set ylabel "Time (sec.)" 
set ylabel  font "Times Bold,24" textcolor lt -1 rotate by -270
set y2label "" 
set y2label  font "" textcolor lt -1 rotate by -270
set yrange [ * : * ] noreverse nowriteback
set y2range [ * : * ] noreverse nowriteback
set zlabel "" 
set zlabel  font "" textcolor lt -1 norotate
set zrange [ * : * ] noreverse nowriteback
set cblabel "" 
set cblabel  font "" textcolor lt -1 rotate by -270
set cbrange [ * : * ] noreverse nowriteback
set paxis 1 range [ * : * ] noreverse nowriteback
set paxis 2 range [ * : * ] noreverse nowriteback
set paxis 3 range [ * : * ] noreverse nowriteback
set paxis 4 range [ * : * ] noreverse nowriteback
set paxis 5 range [ * : * ] noreverse nowriteback
set paxis 6 range [ * : * ] noreverse nowriteback
set paxis 7 range [ * : * ] noreverse nowriteback
set zero 1e-08
set lmargin  -1
set bmargin  -1
set rmargin  -1
set tmargin  -1
set locale "en_US.UTF-8"
set pm3d explicit at s
set pm3d scansautomatic
set pm3d interpolate 1,1 flush begin noftriangles noborder corners2color mean
set palette positive nops_allcF maxcolors 0 gamma 1.5 color model RGB 
set palette rgbformulae 7, 5, 15
set colorbox default
set colorbox vertical origin screen 0.9, 0.2, 0 size screen 0.05, 0.6, 0 front  noinvert bdefault
set style boxplot candles range  1.50 outliers pt 7 separation 1 labels auto unsorted
set loadpath 
set fontpath 
set psdir
set fit brief errorvariables nocovariancevariables errorscaling prescale nowrap v5
GNUTERM = "aqua"
x = 0.0
set xtics (900,1800,3600, 7200)
set xrange [225:7200]
set key top left
#set log y
plot 'timing_eclipse.dat' us 1:5:6:7 ti 'Write Coordinates' wi errorlines lt 1,\
'' us 1:8:9:10 ti 'Write Elements' wi errorlines lt 2,\
'' us 1:11:12:13 ti 'Write Field Data' wi errorlines lt 3,\
'' us 1:14:15:16 ti 'Write Array Data' wi errorlines lt 4,\
'' us 1:17:18:19 ti 'Read Coordinates' wi errorlines  lt 5,\
'' us 1:20:21:22 ti 'Read Elements' wi errorlines lt 6,\
'' us 1:23:24:25 ti 'Read Field Data' wi errorlines lt 7,\
'' us 1:26:27:28 ti 'Read Array Data' wi errorlines lt 8,\
'' us 1:29:30:31 ti 'cgp\_open, WRITE MODE'  wi errorlines lt 9,\
'' us 1:32:33:34 ti 'cgp\_open, MODIFY MODE' wi errorlines lt 10,\
'' us 1:35:36:37 ti  'cgp\_close, AFTER WRITE'  wi errorlines lt 11,\
'' us 1:38:39:40 ti 'cgp\_close, AFTER READ' wi errorlines lt 12,\
'' us 1:2 ti 'Total Time' wi linespo lt 13
#    EOF
