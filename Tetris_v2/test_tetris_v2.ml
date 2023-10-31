#use "tetris_v2.ml";;
let test_param : t_param = { size_x = 10;
                             size_y = 20;
                             dilat = 25;
                             tick = ref 2.;
                             border = 10
                           }
;;
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

(*
auteur : Romain
dessine une barre horizontal rouge en (0,0), une barre vertical verte en (9,4)
et un carre jaune en (0,18)
 *)
let tetris_v2():unit = 

  let init:t_param = {size_x = 10; size_y = 20; dilat = 25; tick = ref 2.; border = 50} 
  and horizontal_bar:t_shape = {position = (0, 0),(1, 0),(2, 0),(3, 0); height = 1; width = 4};
  and vertical_bar:t_shape = {position = (0, 0),(0, 1),(0, 2),(0, 3); height = 4; width = 1};
  and square:t_shape = {position = (0, 0),(1, 0),(0, 1),(1, 1); height = 2; width = 2};
  in
      let cur_square:t_cur_shape = {position=(0,18); color=yellow; shape=square};
      and cur_horizontal_bar:t_cur_shape = {position=(0,0); color=red; shape=horizontal_bar};
      and cur_vertical_bar:t_cur_shape = {position=(9,4); color=green; shape=vertical_bar} 

  in
      open_graph(init);
      draw_frame(init);
      draw_shape(cur_square, init);
      draw_shape(cur_horizontal_bar, init);
      draw_shape(cur_vertical_bar, init);
      wait 60
;;
tetris_v2();;
