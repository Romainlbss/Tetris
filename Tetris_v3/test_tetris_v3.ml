#use "src_v3.ml";;

let test_point : t_point = { x = 0;
                             y = 0
                           }
;;
let horizontal_bar : t_shape =  {position = (0, 0),(1, 0),(2, 0),(3, 0);
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
(*doit ouvrir une fenetre*)
open_graph(test_param);;

(*fait apparaitre l'espace de jeu*)
draw_frame(test_param);;

(*dessine un point en position (0,0) de couleur rouge*)
draw_point(test_point,test_param,red);;

(*doit renvoyer le tuple (1,1)*)
add_two_tuple((0,0),(1,1));;

(*dessine une barre horizontal rouge en (0,0)*)
draw_shape(cur_horizontal_bar, test_param);;

(*dessine une barre horizontal rouge au hasard sur le haut de la fenetre*)
cur_shape_choice(horizontal_bar,red,test_param);;

(*doit enlever toutes les formes de la fenetre*)
clean(test_param);;

(*dessine 3 barres horizontales rouges au-dessus de la fenetre de jeu*)
spawn_shape(horizontal_bar, red, 3, test_param);;

(*dessine 5 barres horizontales rouges et 5 barres verticales vertes dans la fenetre*) 
let tetris_v3():unit = 
  
  let horizontal_bar:t_shape = {position = (0, 0),(1, 0),(2, 0),(3, 0); height = 1; width = 4};
  and vertical_bar:t_shape = {position = (0, 0),(0, 1),(0, 2),(0, 3); height = 4; width = 1};
  and square:t_shape = {position = (0, 0),(1, 0),(0, 1),(1, 1); height = 2; width = 2} in
  let init:t_param = {size_x = 10; size_y = 20; dilat = 25; tick = ref 0.5; is_on_floor = ref false; main_shape = ref {position = (0,1); color = red; shape = square}; border = 10};

  in
      open_graph(init);
      spawn_shape(horizontal_bar, red, 5, init);
      spawn_shape(vertical_bar, green, 5, init)
;;      
tetris_v3();;
