#use "AP1inter.ml";;

(*
type structure qui definit les parametres du jeu
size_x: largeur de l'espace de jeu
size_y: hauteur de l'espace de jeu
dilat: taille en pixels d'un carre
tick: temps de rafraichissement
*)
type t_param = {
                size_x:int; 
                size_y:int; 
                dilat:int; 
                tick:float ref
                }
;;

(*
type structure pour definir un point
x: position horizontale
y: position verticale
 *)
type t_point = {
                x:int; 
                y:int; 
                }
;;

(*
type structure pour definir la forme abstraite d'un objet
position : consitute d'un quadruplet representant les quatre points de l'objet
height : hauteur de l'objet
width : largeur de l'objet
 *)
type t_shape = {
                position:(int*int)*(int*int)*(int*int)*(int*int);
                height: int;
                width: int
                }
;;

(*
type structure pour definir la forme concrete d'un objet
position : position de l'objet dans l'espace de jeu
color : couleur de l'objet
shape : forme abstraite de l'objet
 *)
type t_cur_shape = {
                    position:int*int;
                    color:t_color;
                    shape:t_shape
                    }
;;

(*
auteurs : Melodie, Nathan
fait apparaitre l'espace de jeu en fonction des parametres donnees
 *)
let draw_frame(p_param:t_param):unit =
  fill_rect(10,10,
           (p_param.size_x+2)*p_param.dilat + (p_param.size_x + 1),
           (p_param.size_y+1)*p_param.dilat + (p_param.size_y + 1));
  set_color(white);
  fill_rect(p_param.dilat + 10,p_param.dilat + 10,
           (p_param.size_x)*p_param.dilat + (p_param.size_x + 1),
           (p_param.size_y)*p_param.dilat + (p_param.size_y + 1))
;;

(*
auteurs : Romain, Lucas
colorie un carre en un point  en fonction des parametres et de la couleur donnees
 *) 
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

(*
auteurs : Lucas, Romain
ouvre une fenetre en fonction des parametres donnees
 *)
let open_graph(p_param:t_param):unit =
  open_graph ((p_param.size_x+2)*p_param.dilat + (p_param.size_x + 21),
           (p_param.size_y+3)*p_param.dilat + (p_param.size_y + 11))
;;

(* 
auteurs : Melodie, Nathan
fait l'addition de deux tuples donnes
 *)
let add_two_tuple(p_tuple, p_second_tuple:(int*int)*(int*int)):int*int = 
  (fst p_tuple + fst p_second_tuple, snd p_tuple + snd p_second_tuple)
;;

(*
auteur : Romain
dessine une forme concrete avec les parametres donnes
 *)
let draw_shape(p_cur_shape, p_param:t_cur_shape*t_param):unit =
  (* fait en sorte de mettre la position d'un point sous le type t_point
     afin de pouvoir utiliser la fonction draw_point *)
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
(*
auteur : Romain
dessine une barre horizontal rouge en (0,0), une barre vertical verte en (9,4)
et un carre jaune en (0,18)
 *)
let tetris_v2():unit = 

  let init:t_param = {size_x = 10; size_y = 20; dilat = 25; tick = ref 2.} 
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
