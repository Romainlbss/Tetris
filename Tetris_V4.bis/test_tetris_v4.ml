let test_point : t_point = { x = 0;
                             y = 0
                           }
;;
let horizontal_bar : t_shape =  {position = [|(0, 0); (1, 0); (2, 0); (3, 0)|];
                                 height = 1;
                                 width = 4
                                }
;;
let cur_horizontal_bar : t_cur_shape = { position =(0,0);
                                         color = red;
                                         shape = horizontal_bar
                                       }
;;
let test_param : t_param = { size_x = 10;
                             size_y = 20;
                             dilat = 25;
                             tick = ref 2.;
                             border = 10;
                             is_on_floor = ref false;
                             main_shape = ref {position = (0,1); color = red; shape = horizontal_bar}
                           }
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

(*dessine un point en position (0,0) de couleur rouge pendant 5 secondes*)
let test_draw_point(p_point,p_param,p_color: t_point*t_param*t_color) : unit =
  open_graph(p_param);
  draw_frame(p_param);
  draw_point(p_point,p_param,p_color);
  wait(5);
  close_graph()
;;
test_draw_point(test_point,test_param,red);;

(*doit renvoyer le tuple (1,1)*)
add_two_tuple((0,0),(1,1));;

(*dessine une barre horizontal rouge en (0,0) pendant 5 secondes*)
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
(*dessine 3 barres horizontales rouges au-dessus de la fenetre de jeu*)
spawn_shape(horizontal_bar, red, 3, test_param);;


let tetris_v4():unit = 
  
  let horizontal_bar:t_shape = {position = [|(0, 0); (1, 0); (2, 0); (3, 0)|]; height = 1; width = 4};
  and vertical_bar:t_shape = {position = [|(0, 0); (0, 1); (0, 2); (0, 3)|]; height = 4; width = 1};
  and square:t_shape = {position = [|(0, 0); (1, 0); (0, 1); (1, 1)|]; height = 2; width = 2} in
  let init:t_param = {size_x = 10; size_y = 20; dilat = 25; tick = ref 0.5; is_on_floor = ref false; main_shape = ref {position = (0,1); color = red; shape = square}; border = 20};

  in
      open_graph(init);
      drop_shape(square, green, 5, init);
      drop_shape(vertical_bar, green, 5, init)

      
;;

tetris_v4();;
