#load "graphics.cma";;
#load "unix.cma";;

#use "src.ml";;

let init:t_param = {size_x = 6; size_y = 12; dilat = 50; tick = ref 2.};;

let pointTest:t_point = {x = 0; y = 0; color = black};;

open_graph(init);;

draw_frame(init);;

draw_point(pointTest, init.dilat);;
