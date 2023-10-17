#use "src_v2.ml";;

let init:t_param = {size_x = 10; size_y = 20; dilat = 25; tick = ref 2.};;


let horizontal_bar:t_shape = {position = (0, 0),(1, 0),(2, 0),(3, 0)};;

let vertical_bar:t_shape = {position = (0, 0),(0, 1),(0, 2),(0, 3)};;

let square:t_shape = {position = (0, 0),(1, 0),(0, 1),(1, 1)};;


let cur_square:t_cur_shape = {position=(0,18); color=magenta; shape=square};;

let cur_horizontal_bar:t_cur_shape = {position=(0,0); color=red; shape=horizontal_bar};;

let cur_vertical_bar:t_cur_shape = {position=(9,4); color=red; shape=vertical_bar};;


open_graph(init);;

draw_frame(init);;

draw_shape(cur_square, init.dilat);;

draw_shape(cur_horizontal_bar, init.dilat);;

draw_shape(cur_vertical_bar, init.dilat);;

wait 60;;
