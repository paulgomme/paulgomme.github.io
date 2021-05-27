set style line 1 dt 1 lt 1 lw 3 pt 11 ps 3 linecolor rgb 'red'
set style line 2 dt (5,2) lt 1 lw 3 pt 11 ps 3 linecolor rgb 'blue'
set style line 3 dt (20,3) lt 1 lw 3 pt 11 ps 3 linecolor rgb 'black'
set style line 4 dt (10,2) lt 1 lw 3 pt 11 ps 3 linecolor rgb 'purple'
set style line 5 dt (15,2,5,2) lt 1 lw 6 pt 11 ps 3 linecolor rgb 'dark-green'
set style line 6 dt (5,1) lw 4 pt 11 ps 3 linecolor rgb 'dark-gray'
set style line 7 dt (15,2,10,2,5,2) lw 2 pt 11 ps 3 linecolor rgb 'orange'
set style line 8 lt 1 lw 3 pt 11 ps 3 linecolor rgb 'dark-red'

set style increment user
set xzeroaxis lw 2 lt 1 linecolor rgb 'grey70'
set xtics nomirror
set ytics nomirror
set nox2tics
set border 3
set xtics out
set noy2tics
set lmargin 5
set key
set term pdfcairo solid font 'Times-New-Roman,12' size 5.9cm,2.7cm

unset key
set xrange [2020:2040]

set ytics 3
set out "figs/" . ARG1 . "-y.pdf"
plot "data/" . ARG2 . ".pct" using 1:2 with line,\
	"data/" . ARG3 . ".pct" using 1:2 with line,\
	"data/" . ARG4 . ".pct" using 1:2 with line

set ytics 2
set out "figs/" . ARG1 . "-c.pdf"
plot "data/" . ARG2 . ".pct" using 1:3 with line,\
	"data/" . ARG3 . ".pct" using 1:3 with line,\
	"data/" . ARG4 . ".pct" using 1:3 with line

set ytics auto
set out "figs/" . ARG1 . "-x.pdf"
plot "data/" . ARG2 . ".pct" using 1:16 with line,\
	"data/" . ARG3 . ".pct" using 1:16 with line,\
	"data/" . ARG4 . ".pct" using 1:16 with line

set ytics 2
set out "figs/" . ARG1 . "-h.pdf"
plot "data/" . ARG2 . ".pct" using 1:4 with line,\
	"data/" . ARG3 . ".pct" using 1:4 with line,\
	"data/" . ARG4 . ".pct" using 1:4 with line

set ytics 4
set out "figs/" . ARG1 . "-k.pdf"
plot "data/" . ARG2 . ".pct" using 1:5 with line,\
	"data/" . ARG3 . ".pct" using 1:5 with line,\
	"data/" . ARG4 . ".pct" using 1:5 with line

set ytics auto
set out "figs/" . ARG1 . "-w.pdf"
plot "data/" . ARG2 . ".pct" using 1:8 with line,\
	"data/" . ARG3 . ".pct" using 1:8 with line,\
	"data/" . ARG4 . ".pct" using 1:8 with line

set ytics .1
set out "figs/" . ARG1 . "-dy.pdf"
plot "data/" . ARG2 . ".txt" using 1:13 with line,\
	"data/" . ARG3 . ".txt" using 1:13 with line,\
	"data/" . ARG4 . ".txt" using 1:13 with line

set ytics .05
set out "figs/" . ARG1 . "-defy.pdf"
plot "data/" . ARG2 . ".txt" using 1:15 with line,\
	"data/" . ARG3 . ".txt" using 1:15 with line,\
	"data/" . ARG4 . ".txt" using 1:15 with line

set ytics 1
set out "figs/" . ARG1 . "-rd.pdf"
plot "data/" . ARG2 . ".txt" using 1:18 with line,\
	"data/" . ARG3 . ".txt" using 1:18 with line,\
	"data/" . ARG4 . ".txt" using 1:18 with line

set ytics 2
set out "figs/" . ARG1 . "-rk.pdf"
plot "data/" . ARG2 . ".txt" using 1:19 with line,\
	"data/" . ARG3 . ".txt" using 1:19 with line,\
	"data/" . ARG4 . ".txt" using 1:19 with line

set ytics 3
set out "figs/" . ARG1 . "-taxh.pdf"
plot "data/" . ARG2 . ".txt" using 1:9 with line,\
	"data/" . ARG3 . ".txt" using 1:9 with line,\
	"data/" . ARG4 . ".txt" using 1:9 with line

set ytics 10
set out "figs/" . ARG1 . "-taxk.pdf"
plot "data/" . ARG2 . ".txt" using 1:10 with line,\
	"data/" . ARG3 . ".txt" using 1:10 with line,\
	"data/" . ARG4 . ".txt" using 1:10 with line

set ytics auto
set out "figs/" . ARG1 . "-g.pdf"
plot "data/" . ARG2 . ".pct" using 1:11 with line,\
	"data/" . ARG3 . ".pct" using 1:11 with line,\
	"data/" . ARG4 . ".pct" using 1:11 with line

set ytics 20
set out "figs/" . ARG1 . "-fp.pdf"
plot "data/" . ARG2 . ".pct" using 1:11 with line,\
	"data/" . ARG3 . ".pct" using 1:9 with line,\
	"data/" . ARG4 . ".pct" using 1:10 with line

set out "figs/" . ARG1 . "-u.pdf"
plot "data/" . ARG2 . ".pct" using 1:20 with line,\
	"data/" . ARG3 . ".pct" using 1:20 with line,\
	"data/" . ARG4 . ".pct" using 1:20 with line

#############################################################################

set term pdf size 9cm,4cm
set xrange [2020:2024.75]

set ytics auto
set xtics 1

set out "figs/Figure_02-shocks.pdf"
plot "data/" . ARG2 . ".pct" using 1:11 with line,\
     "" using 1:21 with line,\
     "" using 1:17 with line


