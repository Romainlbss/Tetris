#use "AP1inter.ml";;

(*retourne le temps en micro-seconde depuis le 1er janvier 1970*)
let get_time_of_day() : float =
  Unix.gettimeofday()
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
                position: (int * int) array;
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
is_on_floor: verifie si la piece est en bas de l'espace de jeu
main_shape: la piece qui est a l'ecran
border: taille de la bordure
*)
type t_param = {
                size_x:int; 
                size_y:int; 
                dilat:int; 
                tick:float ref;
                is_on_floor:bool ref;
                main_shape: t_cur_shape ref;
                border: int
                }
;;

(*
auteurs : Melodie, Nathan
fait apparaitre l'espace de jeu en fonction des parametres donnees
p_param : paramètre
 *)
let draw_frame(p_param:t_param):unit =
  fill_rect(p_param.border, p_param.border,
           (p_param.size_x+2)*p_param.dilat + (p_param.size_x + 1),
           (p_param.size_y+1)*p_param.dilat + (p_param.size_y + 1));
  set_color(white);
  fill_rect(p_param.dilat + p_param.border, p_param.dilat + p_param.border,
           (p_param.size_x)*p_param.dilat + (p_param.size_x + 1),
           (p_param.size_y)*p_param.dilat + (p_param.size_y + 1))
;;

(*
auteurs : Romain, Lucas
colorie un carre en un point  en fonction des parametres et de la couleur donnees
p_point : un point
p_param : parametres
color : couleur
 *) 
let draw_point(p_point, p_param, color: t_point*t_param*t_color):unit =
  set_color(color);
  fill_rect((p_point.x + 1) * p_param.dilat + (p_point.x + p_param.border + 1),
            (p_point.y + 1) * p_param.dilat + (p_point.y + p_param.border + 1),
            p_param.dilat, p_param.dilat);
  set_color(black);
  draw_rect((p_point.x + 1) * p_param.dilat + (p_point.x + p_param.border + 1),
            (p_point.y + 1) * p_param.dilat + (p_point.y + p_param.border + 1),
            p_param.dilat, p_param.dilat)
;;

(*
auteurs : Lucas, Romain
ouvre une fenetre en fonction des parametres donnees
p_param : parametres
 *)
let open_graph(p_param:t_param):unit =
  open_graph ((p_param.size_x+2)*p_param.dilat + (p_param.size_x + 2 * p_param.border + 1),
           (p_param.size_y+1)*p_param.dilat + (p_param.size_y + p_param.border + 1))
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
    let point:t_point = {x = x; y = y} in
    draw_point(point, p_param, p_cur_shape.color)
  in
    for i = 0 to Array.length p_cur_shape.shape.position - 1
    do draw_single_square(add_two_tuple(p_cur_shape.shape.position.(i), p_cur_shape.position));
    done;
;;

(*
auteurs : Nathan, Melodie
 pose une forme d'une couleur donnee au hasard en fonction des parametres dans le haut de la fenetre
 *)
let cur_shape_choice(p_shape, p_color, p_param:t_shape*t_color*t_param):t_cur_shape = 
  let random_x : int = Random.int((p_param.size_x - p_shape.width) + 1) in
    {position = (random_x, p_param.size_y - p_shape.height); color = p_color; shape = p_shape}
;;

(*
auteurs : Lucas,Romain
permet de faire disparaitre les formes presentes dans la fenetre 
*)
let clean(p_param:t_param):unit = 
  set_color(white);
  fill_rect(p_param.dilat + p_param.border ,p_param.dilat + p_param.border,
           (p_param.size_x)*p_param.dilat + (p_param.size_x + 1),
           (p_param.size_y)*p_param.dilat + (p_param.size_y + 1))
;;

(*
auteurs : Nathan, Melodie
permet de decaler la forme vers la gauche en fonction de la forme concrete
*)
let move_left(p_cur_shape:t_cur_shape): t_cur_shape =
  match p_cur_shape.position with | (x, y) ->
  if x = 0 then p_cur_shape
  else {position = (x - 1, y); color = p_cur_shape.color; shape = p_cur_shape.shape}
;;

(*
auteurs : Nathan, Melodie
permet de decaler la forme vers la droite en fonction de la forme concrete
*)
let move_right(p_cur_shape, p_param:t_cur_shape*t_param): t_cur_shape =
  match p_cur_shape.position with | (x, y) ->
  if x + p_cur_shape.shape.width = p_param.size_x then p_cur_shape
  else {position = (x + 1, y); color = p_cur_shape.color; shape = p_cur_shape.shape}
;;

(*
auteurs : Nathan, Melodie
permet de decaler la forme vers le bas en fonction de la forme concrete
*)
let move_down(p_cur_shape:t_cur_shape): t_cur_shape =
  match p_cur_shape.position with | (x, y) ->
  {position = (x, y - 1); color = p_cur_shape.color; shape = p_cur_shape.shape}
;;

(*
auteurs : Nathan, Melodie
permet de decaler la forme tout en bas de la fenetre en fonction de la forme concrete
*)
let move_at_bottom(p_cur_shape:t_cur_shape): t_cur_shape = 
  match p_cur_shape.position with | (x, y) ->
  {position = (x, 0); color = p_cur_shape.color; shape = p_cur_shape.shape}
;;

(*
auteur : Lucas
verifie que la forme concrete se situe en bas de la fenetre
 *)
let is_on_floor(p_cur_shape:t_cur_shape): bool =
  if snd(p_cur_shape.position) = 0 then
    true 
  else false
;;

(*
auteur : Romain
permet de deplacer les formes en fonction de la touche appuyee et des parametres donnees
 *)
let move_shape(key, p_param:char*t_param):unit = 
  match key with 
    |'d' -> 
      begin
        draw_frame(p_param);
        draw_shape(move_left(!(p_param.main_shape)), p_param);
        p_param.main_shape := move_left(!(p_param.main_shape));
      end;
    |'h' -> 
      begin
        draw_frame(p_param);
        draw_shape(move_right(!(p_param.main_shape), p_param), p_param);
        p_param.main_shape := move_right(!(p_param.main_shape), p_param);
      end;
    |'v' -> 
      begin
        draw_frame(p_param);
        draw_shape(move_at_bottom(!(p_param.main_shape)), p_param);
        p_param.main_shape := move_at_bottom(!(p_param.main_shape));
        p_param.is_on_floor := true;
      end;
    |_ -> ();
;;

(*
auteurs : Romain, Lucas, Nathan, Melodie
fonction de base du jeu, fait descendre un nombre de fois donnnes une forme donnee,
avec une couleur donnee et les parametres du jeu
 *)
let drop_shape(p_shape, p_color, p_nb, p_param: t_shape*t_color*int*t_param) = 
  for i = 1 to p_nb do
    (*variable qui stock la position aleatoire d'une forme concrete en haut de la fenetre*)
    let l_cur_shape : t_cur_shape = cur_shape_choice(p_shape, p_color, p_param) in
    wait 1;
      draw_frame(p_param);
      p_param.is_on_floor := false;
      p_param.main_shape := l_cur_shape;
      (*variable qui stock le temps au debut du placement de la forme concrete*)
        let l_start_time : float ref = ref (get_time_of_day())in
          draw_shape(!(p_param.main_shape), p_param);
          while not !(p_param.is_on_floor) do
            (*variable qui stock le temps tant que la forme n'est pas en bas de la fenetre*)
            let l_time = get_time_of_day() in
              if l_time -. !(l_start_time) >= !(p_param.tick) then
              (
                draw_frame(p_param);
                draw_shape(move_down(!(p_param.main_shape)), p_param);
                p_param.main_shape := move_down(!(p_param.main_shape));
                p_param.is_on_floor := is_on_floor(!(p_param.main_shape)); 
                l_start_time := get_time_of_day();
              );
              if key_pressed() then
                (*variable qui stock la touche pressee*)
                let l_key : char = read_key() in 
                move_shape(l_key, p_param);
            done;
      done;
;;

