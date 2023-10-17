#load "graphics.cma";;
#load "unix.cma";;

#use "AP1graphics.ml";;

(*
x: position horizontale
y: position verticale
color: couleur du carre
*)
type t_point = {x:int; y:int; color:t_color};;

(*
size_x: largeur de l'espace de jeu
size_y: hauteur de l'espace de jeu
dilat: taille en pixels d'un carre
tick: temps de rafraichissement
*)
type t_param = {size_x:int; size_y:int; dilat:int; tick:float ref};;

(*
ouvre la fenêtre en fonction des parametres donnes
*)
let open_graph(p_param:t_param):unit =
  open_graph((p_param.size_x+2)*p_param.dilat + (p_param.size_x + 21),
           (p_param.size_y+1)*p_param.dilat + (p_param.size_y + 11))
;;

(*
dessine l'aire de jeu en fonction des parametres donnes
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
dessine un point en fonction de la couleur et la taille donnees dans l'aire de jeu 
*)
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
