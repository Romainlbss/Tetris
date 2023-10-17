#use "topfind";;
#require "graphics";;
#require "unix";;
#use "AP1graphics.ml";;


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
                color:t_color
                }
;;

type t_shape = {
                position:(int*int)*(int*int)*(int*int)*(int*int)
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

let draw_point(p_point, p_dilat : t_point*int):unit =
  set_color(p_point.color);
  fill_rect((p_point.x + 1) * p_dilat + (p_point.x + 11),
            (p_point.y + 1) * p_dilat + (p_point.y + 11),
            p_dilat, p_dilat);
  set_color(black);
  draw_rect((p_point.x + 1) * p_dilat + (p_point.x + 11),
            (p_point.y + 1) * p_dilat + (p_point.y + 11),
            p_dilat, p_dilat)
;;

let open_graph(p_param:t_param):unit =
  open_graph ((p_param.size_x+2)*p_param.dilat + (p_param.size_x + 21),
           (p_param.size_y+1)*p_param.dilat + (p_param.size_y + 11))
;;

let add_two_tuple(p_tuple, p_second_tuple:(int*int)*(int*int)):int*int = 
  (fst p_tuple + fst p_second_tuple, snd p_tuple + snd p_second_tuple)
;;

let draw_shape(p_cur_shape, p_dilat:t_cur_shape*int):unit = 
  let draw_single_square((x, y), color : (int*int) * t_color):unit = 
    let point:t_point = {x = x; y = y; color = color} in
    draw_point(point, p_dilat)
  in
  match p_cur_shape.shape.position with | (square_1, square_2, square_3, square_4) ->
  draw_single_square(add_two_tuple(square_1, p_cur_shape.position),p_cur_shape.color);
  draw_single_square(add_two_tuple(square_2, p_cur_shape.position),p_cur_shape.color);
  draw_single_square(add_two_tuple(square_3, p_cur_shape.position),p_cur_shape.color);
  draw_single_square(add_two_tuple(square_4, p_cur_shape.position),p_cur_shape.color)
;;