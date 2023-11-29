let horizontal_bar : t_shape =  {position = [|(0, 0); (1, 0); (2, 0); (3, 0)|];
                                 height = 1;
                                 width = 4
                                }
;;
let cur_horizontal_bar : t_cur_shape = { position =(3,19);
                                         color = red;
                                         shape = horizontal_bar
                                       }
;;
let test_param : t_param = { size_x = 10;
                             size_y = 20;
                             dilat = 25;
                             tick = ref 2.;
                             border = 10
                           }
;;

let test_play : t_play = { main_shape = ref cur_horizontal_bar }
;;

(*ouvre une fenetre pendant 5 secondes*)
let test_open_graph(p_param:t_param) : unit =
  open_graph(p_param);
  wait(5);
  close_graph()
;;
test_open_graph(test_param);;

(*fait apparaitre l'espace de jeu pendant 5 secondes*)
let test_draw_frame(p_param:t_param) : unit =
  open_graph(p_param);
  draw_frame(p_param);
  wait(5);
  close_graph()
;;
test_draw_frame(test_param);;

(*dessine un point en position (3,19) de couleur rouge pendant 5 secondes*)
let test_draw_point((p_point_x,p_point_y),p_param,p_color: (int*int)*t_param*t_color) : unit =
  open_graph(p_param);
  draw_frame(p_param);
  draw_point((p_point_x,p_point_y),p_param,p_color);
  wait(5);
  close_graph()
;;
test_draw_point((0,0),test_param,red);;

(*doit renvoyer le tuple (1,1)*)
add_two_tuple((0,0),(1,1));;

(*dessine une barre horizontal rouge en (3,19) pendant 5 secondes*)
let test_draw_shape(p_cur_shape,p_param: t_cur_shape*t_param) : unit =
  open_graph(p_param);
  draw_frame(p_param);
  draw_shape(p_cur_shape,p_param);
  wait(5);
  close_graph()
;;
test_draw_shape(cur_horizontal_bar, test_param);;

(*dessine une barre horizontal rouge au hasard sur le haut de la fenetre*)
let test_cur_shape_choice(p_shape,p_color,p_param: t_shape*t_color*t_param) : unit =
  open_graph(p_param);
  p_param.main_shape := cur_shape_choice(p_shape,p_color,p_param);
  draw_frame(p_param);
  wait(5);
  close_graph()
;;
test_cur_shape_choice(horizontal_bar,red,test_param);;

(*doit enlever toutes les formes de la fenetre*)
let test_clean(p_cur_shape,p_param: t_cur_shape*t_param) : unit =
  open_graph(p_param);
  draw_frame(p_param);
  draw_shape(p_cur_shape,p_param);
  wait(1);
  clean(p_param);
  wait(1)
;;
test_clean(cur_horizontal_bar, test_param);;

(*la barre horizontal rouge bouge de 1 carreau a gauche*)
let test_move_left(p_cur_shape,p_param: t_cur_shape*t_param) : unit =
  open_graph(p_param);
  draw_frame(p_param);
  draw_shape(p_cur_shape,p_param);
  wait(3);
  draw_frame(p_param);
  draw_shape(move_left(p_cur_shape), p_param);
  wait(3);
  close_graph()
;;
test_move_left(cur_horizontal_bar, test_param);;

(*la barre horizontal rouge bouge de 1 carreau a droite*)
let test_move_right(p_cur_shape,p_param: t_cur_shape*t_param) : unit =
  open_graph(p_param);
  draw_frame(p_param);
  draw_shape(p_cur_shape,p_param);
  wait(3);
  draw_frame(p_param);
  draw_shape(move_right(p_cur_shape,p_param), p_param);
  wait(3);
  close_graph()
;;
test_move_right(cur_horizontal_bar, test_param);;

(*la barre horizontal rouge bouge de 1 carreau en bas*)
let test_move_down(p_cur_shape,p_param: t_cur_shape*t_param) : unit =
  open_graph(p_param);
  draw_frame(p_param);
  draw_shape(p_cur_shape,p_param);
  wait(3);
  draw_frame(p_param);
  draw_shape(move_down(p_cur_shape), p_param);
  wait(3);
  close_graph()
;;
test_move_down(cur_horizontal_bar, test_param);;

(*la barre horizontal rouge bouge en bas de la fenetre de jeu*)
let test_move_at_bottom(p_cur_shape,p_param: t_cur_shape*t_param) : unit =
  open_graph(p_param);
  draw_frame(p_param);
  draw_shape(p_cur_shape,p_param);
  wait(3);
  draw_frame(p_param);
  draw_shape(move_at_bottom(p_cur_shape), p_param);
  wait(3);
  close_graph()
;;
test_move_at_bottom(cur_horizontal_bar, test_param);;

(*on peut bouger la piece avec les touches d,h,v jusqu'a ce que la piece soit en bas de la fenetre*)
let test_move_shape(p_param,p_play: t_param*t_play):unit =
  open_graph(p_param);
  let l_start_time : float ref = ref (get_time_of_day())
  in
   while not(is_on_floor(!(p_play.main_shape)))
   do
     let l_time = get_time_of_day() in
      if l_time -. !(l_start_time) >= !(p_param.tick) then
        (
         draw_frame(p_param);
         draw_shape(move_down(!(p_play.main_shape)), p_param);
         p_play.main_shape := move_down(!(p_play.main_shape)); 
         l_start_time := get_time_of_day();
         );
      if key_pressed() then
       let l_key : char = read_key() in 
       move_shape(l_key, p_param,p_play);
   done;
   close_graph()
;;
test_move_shape(test_param,test_play);;

(*fait apparaitre 5 barres horizontales que l'on peut bouger*)
let test_drop_changer(p_shape,p_color,p_nb,p_play,p_param: t_shape*t_color*int*t_play*t_param) : unit =
  open_graph(p_param);
  drop_shape(p_shape, p_color, p_nb,p_param,p_play)
;;
test_drop_changer(horizontal_bar, red, 5, test_play, test_param);;

(*lance le test demander*)
let tetris_v4():unit = 
  let l_vertical_bar:t_shape = {position = [|(0, 0); (0, 1); (0, 2); (0, 3)|]; height = 4; width = 1};
  and l_square:t_shape = {position = [|(0, 0); (1, 0); (0, 1); (1, 1)|]; height = 2; width = 2}
  in
  let l_init:t_param = {size_x = 10; size_y = 20; dilat = 25; tick = ref 0.5; border = 20};
  and l_play:t_play = {main_shape = ref {position = (0,1); color = red; shape = l_square}}
  in
      open_graph(l_init);
      drop_shape(l_square, green, 5, l_init, l_play);
      drop_shape(l_vertical_bar, green, 5, l_init, l_play);
      close_graph()

      
;;

tetris_v4();;
