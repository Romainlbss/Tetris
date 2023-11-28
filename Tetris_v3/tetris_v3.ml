#use "AP1inter.ml";;

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
type structure qui definit les parametres du jeu
size_x: largeur de l'espace de jeu
size_y: hauteur de l'espace de jeu
dilat: taille en pixels d'un carre
tick: temps de rafraichissement
border: taille de la bordure
is_on_floor: verifie si la piece est en bas de l'espace de jeu
main_shape: la piece qui est a l'ecran
*)
type t_param = {
                size_x:int; 
                size_y:int; 
                dilat:int; 
                tick:float ref;
                border:int;
                is_on_floor:bool ref;
                main_shape: t_cur_shape ref
                }
;;

(*
auteurs : Melodie, Nathan
fait apparaitre l'espace de jeu en fonction des parametres donnees
p_param : paramètre
 *)
let draw_frame(p_param:t_param):unit =
  fill_rect(p_param.border,p_param.border,
           (p_param.size_x+2)*p_param.dilat + (p_param.size_x + 1),
           (p_param.size_y+1)*p_param.dilat + (p_param.size_y + 1));
  set_color(white);
  fill_rect(p_param.dilat + p_param.border,p_param.dilat + p_param.border,
           (p_param.size_x)*p_param.dilat + (p_param.size_x + 1),
           (p_param.size_y)*p_param.dilat + (p_param.size_y + 1))
;;

(*
auteurs : Romain, Lucas
colorie un carre en un point  en fonction des parametres et de la couleur donnees
p_point : un point
p_param : parametre
color : couleur
 *) 
let draw_point(p_point, p_param, color: t_point*t_param*t_color):unit =
  set_color(color);
  fill_rect((p_point.x + 1) * p_param.dilat + (p_point.x + 1 + p_param.border),
            (p_point.y + 1) * p_param.dilat + (p_point.y + 1 + p_param.border),
            p_param.dilat, p_param.dilat);
  set_color(black);
  draw_rect((p_point.x + 1) * p_param.dilat + (p_point.x + 1 + p_param.border),
            (p_point.y + 1) * p_param.dilat + (p_point.y + 1 + p_param.border),
            p_param.dilat, p_param.dilat)

(*
auteurs : Lucas, Romain
ouvre une fenetre en fonction des parametres donnees
p_param : parametre
 *)
let open_graph(p_param:t_param):unit =
  open_graph ((p_param.size_x+2)*p_param.dilat + (p_param.size_x + 1 + p_param.border + 1),
           (p_param.size_y+1)*p_param.dilat + (p_param.size_y + 1 + p_param.border))
;;

(* 
auteurs : Melodie, Nathan
fait l'addition de deux tuples donnes
p_tuple : premier tuple
p_second_tuple : deuxieme tuple
 *)
let add_two_tuple(p_tuple, p_second_tuple:(int*int)*(int*int)):int*int = 
  (fst p_tuple + fst p_second_tuple, snd p_tuple + snd p_second_tuple)
;;

(*
auteur : Romain
dessine une forme concrete avec les parametres donnes
p_cur_shape : forme concrete
p_param : parametre
 *)
let draw_shape(p_cur_shape, p_param:t_cur_shape*t_param):unit =
  (* fait en sorte de mettre la position d'un point sous le type t_point
     afin de pouvoir utiliser la fonction draw_point *)
  let draw_single_square((x, y): (int*int)):unit = 
    let l_point:t_point = {x = x; y = y} in
    draw_point(l_point, p_param, p_cur_shape.color)
  in
  match p_cur_shape.shape.position with | (square_1, square_2, square_3, square_4) ->
  draw_single_square(add_two_tuple(square_1, p_cur_shape.position));
  draw_single_square(add_two_tuple(square_2, p_cur_shape.position));
  draw_single_square(add_two_tuple(square_3, p_cur_shape.position));
  draw_single_square(add_two_tuple(square_4, p_cur_shape.position))
;;

(*
auteur : Nathan, Melodie
pose une forme d'une couleur donnee au hasard dans le haut de la fenetre
 *)
let cur_shape_choice(p_shape, p_color, p_param:t_shape*t_color*t_param):t_cur_shape = 
  let l_random_x : int = Random.int((p_param.size_x - p_shape.width) + 1) in
    {position = (l_random_x, p_param.size_y - p_shape.height); color = p_color; shape = p_shape}
;;

(*
auteur : Lucas,Romain
permet de faire disparaitre les formes presentes dans la fenetre 
*)
let clean(p_param:t_param):unit = 
  set_color(white);
  fill_rect(p_param.dilat + p_param.border,p_param.dilat + p_param.border,
           (p_param.size_x)*p_param.dilat + (p_param.size_x + 1),
           (p_param.size_y)*p_param.dilat + (p_param.size_y + 1))
;;

(*
auteur : Romain
dessine la forme choisis un nombre de fois donnees dans la fenetre
*)
let spawn_shape(p_shape, p_color, p_nb, p_param: t_shape*t_color*int*t_param) = 
  for i = 1 to p_nb do
    let l_cur_shape : t_cur_shape = cur_shape_choice(p_shape, p_color, p_param) in
      draw_frame(p_param);
      p_param.main_shape := l_cur_shape;
      draw_shape(!(p_param.main_shape), p_param);
      wait 1;
  done;
;;
