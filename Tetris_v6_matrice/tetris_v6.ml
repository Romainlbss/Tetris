(*retourne le temps en micro-seconde depuis le 1er janvier 1970*)
let get_time_of_day() : float =
  Unix.gettimeofday()
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
border: taille de la bordure
shapes: stock toutes les formes possibles
colors: stock toutes les couleurs possibles
*)
type t_param = {
                size_x : int; 
                size_y : int; 
                dilat : int; 
                tick : float ref;
                border : int;
                shapes : t_shape array;
                colors : t_color array
                }
;;

(*
type structure qui definit la piece qui est a l'ecran
main_shape : piece qui est a l'ecran
score : score qui augmente a chaque piece poser
game_space : definit l'espace de jeu en une matrice
 *)
type t_play = {
               game_space : int array array;
               score : int ref;
               main_shape : t_cur_shape ref
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
let draw_point((p_point_x,p_point_y), p_param, color: (int*int)*t_param*t_color):unit =
  set_color(color);
  fill_rect((p_point_x + 1) * p_param.dilat + (p_point_x + p_param.border + 1),
            (p_point_y + 1) * p_param.dilat + (p_point_y + p_param.border + 1),
            p_param.dilat, p_param.dilat);
  set_color(black);
  draw_rect((p_point_x + 1) * p_param.dilat + (p_point_x + p_param.border + 1),
            (p_point_y + 1) * p_param.dilat + (p_point_y + p_param.border + 1),
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
auteurs : Melodie, Romain
change un entier en une couleur
*)
let color_of_int(p_nb : int) : t_color = 
  if p_nb = 1
    then Graphics.green
    else
      if p_nb = 2
      then Graphics.yellow
      else
        if p_nb = 7
        then Graphics.cyan
        else
          if p_nb = 4 
          then Graphics.blue
          else
            if p_nb = 5
            then Graphics.red
            else
              if p_nb = 6
              then Graphics.magenta
              else 
                if p_nb = 3
                then (255 * 256 * 256) + (140 * 256) + 0
                else -1;
;;

(*
auteurs : Melodie, Romain
change une couleur en un nombre
*)
let int_of_color(p_color:t_color) : int = 
  if p_color = Graphics.green
    then 1
    else
      if p_color = Graphics.yellow
      then 2
      else
        if p_color = Graphics.cyan
        then 7
        else
          if p_color = Graphics.blue
          then 4
          else
            if p_color = Graphics.red
            then 5
            else
              if p_color = Graphics.magenta
              then 6
              else 
                if p_color = (255 * 256 * 256) + (140 * 256) + 0
                then 3
                else -1;
;;

(*
auteurs : Nathan, Lucas
affiche la matrice en fonction des parametres et de la matrice du jeu*)
let print_matrix(p_param, p_play : t_param*t_play) : unit =
  draw_frame(p_param);
  for i = 0 to p_param.size_y - 1
  do
    (
      for j = 0 to p_param.size_x - 1
      do
        (
          let color = color_of_int(p_play.game_space.(p_param.size_y - 1 - i).(j)) in
            if color <> -1
            then draw_point(j, i, p_param, p_play, color);
        )
      done;
    )
  done;
;;

(*
auteurs : Romain, Melodie
affiche la forme concrete en fonction des parametres et de la matrice du jeu
 *)
let draw_shape(p_cur_shape, p_param, p_game : t_cur_shape*t_param*t_play):unit = 
  for i = 0 to Array.length p_cur_shape.shape.position - 1
  do p_game.game_space.(p_cur_shape.position.(1) + snd(p_cur_shape.shape.position.(i))).(p_cur_shape.position.(0) + fst(p_cur_shape.shape.position.(i))) <- int_of_color(p_cur_shape.color);
  done;
  affiche_matrice(p_param, p_game);
;;

(*
auteurs : Nathan, Melodie
pose une forme au hasard en fonction des parametres dans le haut de la fenetre
 *)
let cur_shape_choice(p_param:t_param):t_cur_shape = 
  let shape = p_param.shapes.(Random.int(Array.length p_param.shapes));
  and color = p_param.colors.(Random.int(Array.length p_param.colors));
  in
   let random_x : int = Random.int((p_param.size_x - shape.width) + 1)
   in
     {position = [|random_x; 0|]; color = color; shape = shape}
;;

(*
auteurs : Nathan, Melodie
permet de decaler la forme vers la gauche en fonction de la forme concrete, des parametres et de la matrice
*)
let move_left(p_cur_shape, p_param, p_game : t_cur_shape*t_param*t_play): t_cur_shape =
  let l_x = p_cur_shape.position.(0) 
  and l_y = p_cur_shape.position.(1) 
  and l_left_colision : bool ref = ref false in
    if l_x = 0 
    then p_cur_shape
    else
      (
        for i = 0 to p_cur_shape.shape.height - 1
          do 
            (
              if p_game.game_space.(p_cur_shape.position.(1) + i).(p_cur_shape.position.(0) - 1) <> 0
              then 
                (
                 l_left_colision := true;
                  print_int(p_cur_shape.position.(1) + i);
                  print_int(p_cur_shape.position.(0) - 1);
                )
              
            )
          done;
          if !l_left_colision
          then
            (
              p_cur_shape;
            )
            
          else {position = [|l_x - 1; l_y|]; color = p_cur_shape.color; shape = p_cur_shape.shape}
      )
;;

(*
auteurs : Nathan, Melodie
permet de decaler la forme vers la droite en fonction de la forme concrete, des parametres et de la matrice de jeu
*)
let move_right(p_cur_shape, p_param, p_game:t_cur_shape*t_param*t_play): t_cur_shape =
  let l_x = p_cur_shape.position.(0) 
  and l_y = p_cur_shape.position.(1) 
  and l_right_colision : bool ref = ref false
  in
    if p_cur_shape.position.(0) + p_cur_shape.shape.width = p_param.size_x
    then p_cur_shape
    else
      (
        for i = 0 to p_cur_shape.shape.height - 1
        do 
        (
          if p_game.game_space.(p_cur_shape.position.(1) + i).(p_cur_shape.position.(0) + p_cur_shape.shape.width) <> 0
          then l_right_colision := true;
        )
        done;
        if !l_right_colision
        then p_cur_shape
        else {position = [|l_x + 1; l_y|]; color = p_cur_shape.color; shape = p_cur_shape.shape}
      )
;;

(*
auteurs : Nathan, Melodie
permet de decaler la forme vers le bas en fonction de la forme concrete
*)
let move_down(p_cur_shape : t_cur_shape): t_cur_shape =
  let l_x = p_cur_shape.position.(0) 
  and l_y = p_cur_shape.position.(1)
  in
    {position = [|l_x; l_y + 1|]; color = p_cur_shape.color; shape = p_cur_shape.shape}

(*
auteurs : Nathan, Melodie
permet de decaler la forme tout en bas de la fenetre en fonction de la forme concrete
 *)
let move_at_bottom(p_cur_shape, p_param, p_game : t_cur_shape*t_param*t_play): t_cur_shape = 
  let l_x = p_cur_shape.position.(0) 
  and l_y = ref (p_param.size_y) in
    for i = p_cur_shape.position.(0) to p_cur_shape.shape.width + p_cur_shape.position.(0) - 1
    do 
      (
        for j = p_cur_shape.position.(1) to p_param.size_y - 1
        do 
          (
            if p_game.game_space.(j).(i) <> 0
            then 
              (
                l_y := min !l_y j;
              )
            
          )
        done;
      )
      done;
    {position = [|l_x; !l_y - p_cur_shape.shape.height|]; color = p_cur_shape.color; shape = p_cur_shape.shape}
;;

(*
auteurs :
detecte si une forme touche une autre forme ou le bas de l'espace de jeu en fonction de la forme concrete, des parametres et de la matrice de jeu
 *)
let colision(p_cur_shape, p_param, p_game : t_cur_shape*t_param*t_play): bool =
  let l_colision : bool ref = ref false in
    if p_cur_shape.position.(1) = p_param.size_y - p_cur_shape.shape.height;
    then l_colision := true
    else 
      for i = 0 to p_cur_shape.shape.width - 1
      do 
        if p_game.game_space.(p_cur_shape.position.(1) + snd(p_cur_shape.shape.position.(i)) + p_cur_shape.shape.height).(p_cur_shape.position.(0) + fst(p_cur_shape.shape.position.(i))) <> 0
        then 
        (
          l_colision := true; 
        );
     done;
    !l_colision
;;
(*
auteur : Romain
permet de deplacer les formes en fonction de la touche appuyee et des parametres donnees
 *)
let move_shape(key, p_param,p_play:char*t_param*t_play):unit = 
  match key with 
    |'d' -> 
      begin
        draw_frame(p_param);
        draw_shape(move_left(!(p_play.main_shape)), p_param);
        p_play.main_shape := move_left(!(p_play.main_shape));
      end;
    |'h' -> 
      begin
        draw_frame(p_param);
        draw_shape(move_right(!(p_play.main_shape), p_param), p_param);
        p_play.main_shape := move_right(!(p_play.main_shape), p_param);
      end;
    |'v' -> 
      begin
        draw_frame(p_param);
        draw_shape(move_at_bottom(!(p_play.main_shape)), p_param);
        p_play.main_shape := move_at_bottom(!(p_play.main_shape));
      end;
    |_ -> ();
;;

(*
auteurs : Romain, Lucas, Nathan, Melodie
fonction de base du jeu, fait descendre un nombre de fois donnnes des formes et les parametres du jeu
 *)
let drop_shape(p_nb, p_param,p_play: int*t_param*t_play) = 
  for i = 1 to p_nb do
    (*variable qui stock la position aleatoire d'une forme concrete en haut de la fenetre*)
    let l_cur_shape : t_cur_shape = cur_shape_choice(p_param) in
    wait 1;
      draw_frame(p_param);
      p_play.main_shape := l_cur_shape;
      (*variable qui stock le temps au debut du placement de la forme concrete*)
        let l_start_time : float ref = ref (get_time_of_day())in
          draw_shape(!(p_play.main_shape), p_param);
          while not(is_on_floor(!(p_play.main_shape))) do
            (*variable qui stock le temps tant que la forme n'est pas en bas de la fenetre*)
            let l_time = get_time_of_day() in
              if l_time -. !(l_start_time) >= !(p_param.tick) then
              (
                draw_frame(p_param);
                draw_shape(move_down(!(p_play.main_shape)), p_param);
                p_play.main_shape := move_down(!(p_play.main_shape)); 
                l_start_time := get_time_of_day();
              );
              if key_pressed() then
                (*variable qui stock la touche pressee*)
                let l_key : char = read_key() in 
                move_shape(l_key, p_param,p_play);
            done;
      done
;;

(*
auteurs : Lucas, Romain
supprime une forme donnee en parametres de la fenetre et de la matrice de jeu*)
*)
let supp_shape(p_cur_shape, p_param, p_game : t_cur_shape*t_param*t_play) : unit =
  for i = 0 to Array.length (p_cur_shape.shape.position) - 1
  do p_game.game_space.(p_cur_shape.position.(1) + snd(p_cur_shape.shape.position.(i))).(p_cur_shape.position.(0) + fst(p_cur_shape.shape.position.(i))) <- 0;
  done;
  affiche_matrice(p_param, p_game);
;;

(*
1 = green
2 = yellow
3 = orange
4 = blue
5 = red 
6 = magenta
7 = cyan
*)

(*
auteurs: Melodie, Nathan
insert la forme courante dans la matrice de jeu en
*)
let final_intert(p_shape, p_param, p_game : t_cur_shape*t_param*t_play):unit =
  let l_nb_color : int ref = ref int_of_color(p_shape.color)
  in
    for i = 0 to 3
    do p_game.game_space.(snd(p_shape.shape.position.(i)) + p_shape.position.(1)).(p_shape.position.(0) + fst(p_shape.shape.position.(i))) <- !l_nb_color
    done
;;

(*
auteur: Romain
deplace la forme en fonction de la touche appuyee dans la matrice de jeu
 *)
let move_shape(key, p_param, p_game : char*t_param*t_play):unit = 
  match key with 
    |'d' -> 
      (
        supp_shape(!(p_game.main_shape), p_param, p_game);
        p_game.main_shape := move_left(!(p_game.main_shape), p_param, p_game);
        draw_shape(!(p_game.main_shape), p_param, p_game), p_param, p_game;
        affiche_matrice(p_param, p_game);
      );
    |'h' -> 
      (
        supp_shape(!(p_game.main_shape), p_param, p_game);
        p_game.main_shape := move_right(!(p_game.main_shape), p_param, p_game);
        draw_shape(!(p_game.main_shape), p_param, p_game);
        affiche_matrice(p_param, p_game);
      );
    |'v' -> 
      (
        supp_shape(!(p_game.main_shape), p_param, p_game);
        p_game.main_shape := move_at_bottom(!(p_game.main_shape), p_param, p_game);
        draw_shape(!(p_game.main_shape), p_param, p_game), p_param, p_game;
        affiche_matrice(p_param, p_game);
      );
    |_ -> ();
;;

(*
auteur: Romain
creer une matrice en fonction d'une hauteur et d'une largeur donnee
*)
let create_game_space(p_height, p_width : int*int) : int array array =
  Array.init p_height (fun _ -> Array.make p_width 0);;
  (*
    Ne fonctionne pas 
    arr_make(p_height , arr_make(p_width, 0))
  *)
;;

(*
auteurs: Melodie, Nathan
ajoute une valeur donnee dans un tableau
*)
let add_to_array(array, value : 'a array* 'a) : 'a array =
  let l_final_array = arr_make(Array.length array + 1, 0)
  in
    for i = 0 to Array.length array - 1
    do l_final_array.(i) <- array.(i);
    done;
    l_final_array.(Array.length l_final_array - 1) <- value;
    l_final_array
;;
