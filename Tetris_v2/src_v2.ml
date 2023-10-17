#use "AP1inter.ml";;

type t_param = {
                size_x:int; 
                size_y:int; 
                dilat:int; 
                tick:float ref
                }
;;

type t_point = {
                x:int; 
                y:int; 
                }
;;

type t_shape = {
                position:(int*int)*(int*int)*(int*int)*(int*int);
                height: int;
                width: int
                }
;;

type t_cur_shape = {
                    position:int*int;
                    color:t_color;
                    shape:t_shape
                    }
;;

let draw_frame(p_param:t_param):unit =
  fill_rect(10,10,
           (p_param.size_x+2)*p_param.dilat + (p_param.size_x + 1),
           (p_param.size_y+1)*p_param.dilat + (p_param.size_y + 1));
  set_color(white);
  fill_rect(p_param.dilat + 10,p_param.dilat + 10,
           (p_param.size_x)*p_param.dilat + (p_param.size_x + 1),
           (p_param.size_y)*p_param.dilat + (p_param.size_y + 1))
;;

let draw_point(p_point, p_param, color: t_point*t_param*t_color):unit =
  set_color(color);
  fill_rect((p_point.x + 1) * p_param.dilat + (p_point.x + 11),
            (p_point.y + 1) * p_param.dilat + (p_point.y + 11),
            p_param.dilat, p_param.dilat);
  set_color(black);
  draw_rect((p_point.x + 1) * p_param.dilat + (p_point.x + 11),
            (p_point.y + 1) * p_param.dilat + (p_point.y + 11),
            p_param.dilat, p_param.dilat)
;;

let open_graph(p_param:t_param):unit =
  open_graph ((p_param.size_x+2)*p_param.dilat + (p_param.size_x + 21),
           (p_param.size_y+1)*p_param.dilat + (p_param.size_y + 11))
;;

let add_two_tuple(p_tuple, p_second_tuple:(int*int)*(int*int)):int*int = 
  (fst p_tuple + fst p_second_tuple, snd p_tuple + snd p_second_tuple)
;;

let draw_shape(p_cur_shape, p_param:t_cur_shape*t_param):unit = 
  let draw_single_square((x, y): (int*int)):unit = 
    let point:t_point = {x = x; y = y} in
    draw_point(point, p_param, p_cur_shape.color)
  in
  match p_cur_shape.shape.position with | (square_1, square_2, square_3, square_4) ->
  draw_single_square(add_two_tuple(square_1, p_cur_shape.position));
  draw_single_square(add_two_tuple(square_2, p_cur_shape.position));
  draw_single_square(add_two_tuple(square_3, p_cur_shape.position));
  draw_single_square(add_two_tuple(square_4, p_cur_shape.position))
;;

let tetris_v2():unit = 

  let init:t_param = {size_x = 10; size_y = 20; dilat = 25; tick = ref 2.} 
  and horizontal_bar:t_shape = {position = (0, 0),(1, 0),(2, 0),(3, 0); height = 1; width = 4};
  and vertical_bar:t_shape = {position = (0, 0),(0, 1),(0, 2),(0, 3); height = 4; width = 1};
  and square:t_shape = {position = (0, 0),(1, 0),(0, 1),(1, 1); height = 2; width = 2};
  in
      let cur_square:t_cur_shape = {position=(0,18); color=magenta; shape=square};
      and cur_horizontal_bar:t_cur_shape = {position=(0,0); color=red; shape=horizontal_bar};
      and cur_vertical_bar:t_cur_shape = {position=(9,4); color=red; shape=vertical_bar} 

  in
      open_graph(init);
      draw_frame(init);
      draw_shape(cur_square, init);
      draw_shape(cur_horizontal_bar, init);
      draw_shape(cur_vertical_bar, init);
      wait 60
;;



