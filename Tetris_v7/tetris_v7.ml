#use "AP1inter.ml";;

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
                    position: int array;
                    color: t_color;
                    shape: int ref
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
game_space : matrice representant l'espace de jeu
score : score du joueur
 *)
type t_game = {
                game_space : int array array;
                score : int ref;
                main_shape: t_cur_shape ref
              }
;;

(*
auteurs : Melodie, Nathan
fait apparaitre l'espace de jeu en fonction des parametres donnees
 *)
let draw_frame(p_param : t_param):unit =
  set_color(black);
  fill_rect(p_param.border, p_param.border,
           (p_param.size_x + 2) * p_param.dilat + (p_param.size_x + 1),
           (p_param.size_y + 1) * p_param.dilat + (p_param.size_y + 1));
  set_color(white);
  fill_rect(p_param.dilat + p_param.border, p_param.dilat + p_param.border,
           (p_param.size_x) * p_param.dilat + (p_param.size_x + 1),
           (p_param.size_y) * p_param.dilat + (p_param.size_y + 1))
;;

(*
auteurs : Romain, Lucas
colorie un carre en un point  en fonction des parametres et de la couleur donnees
*) 
let draw_point(p_x, p_y, p_param, p_game, p_color : int*int*t_param*t_game*t_color):unit =
  set_color(p_color);
  fill_rect((p_x + 1) * p_param.dilat + (p_x + p_param.border + 1),
            (p_y + 1) * p_param.dilat + (p_y + p_param.border + 1),
            p_param.dilat, p_param.dilat);
  set_color(black);
  draw_rect((p_x + 1) * p_param.dilat + (p_x + p_param.border + 1),
            (p_y + 1) * p_param.dilat + (p_y + p_param.border + 1),
            p_param.dilat, p_param.dilat)
;;

(*
auteurs : Melodie, Romain
change une couleur en un entier
 *)
let color_of_int(nb : int) : t_color = 
  if nb = 1
    then Graphics.green
    else
      if nb = 2
      then Graphics.yellow
      else
        if nb = 7
        then Graphics.cyan
        else
          if nb = 4 
          then Graphics.blue
          else
            if nb = 5
            then Graphics.red
            else
              if nb = 6
              then Graphics.magenta
              else 
                if nb = 3
                then (255 * 256 * 256) + (140 * 256) + 0
                else -1
;;

(* 
auteurs : Melodie, Romain
change un entier en une couleur
*)
let int_of_color(color : t_color) : int = 
  if color = Graphics.green
    then 1
    else
      if color = Graphics.yellow
      then 2
      else
        if color = Graphics.cyan
        then 7
        else
          if color = Graphics.blue
          then 4
          else
            if color = Graphics.red
            then 5
            else
              if color = Graphics.magenta
              then 6
              else 
                if color = (255 * 256 * 256) + (140 * 256) + 0
                then 3
                else -1
;;

(*
auteurs : Nathan, Lucas
affiche la matrice en fonction des parametres et de la matrice du jeu
*)
let print_matrix(p_param, p_game : t_param * t_game) : unit =
  draw_frame(p_param);
  for i = 0 to p_param.size_y - 1
  do
    for j = 0 to p_param.size_x - 1
    do
      let l_color = color_of_int(p_game.game_space.(p_param.size_y - 1 - i).(j))
      in
       if l_color <> -1
       then draw_point(j, i, p_param, p_game, l_color)
    done;
  done
;;

(*
auteurs : Lucas, Romain
ouvre une fenetre en fonction des parametres donnees
 *)
let open_graph(p_param : t_param) : unit =
  open_graph((p_param.size_x + 2) * p_param.dilat + (p_param.size_x + 2 * p_param.border + 1),
           (p_param.size_y + 1) * p_param.dilat + (p_param.size_y + p_param.border + 1))
;;

(*
auteur : Romain
dessine une forme concrete avec les parametres donnes
 *)
let draw_shape(p_cur_shape, p_param, p_game : t_cur_shape * t_param * t_game) : unit = 
  for i = 0 to Array.length p_param.shapes.(!(p_cur_shape.shape)).position - 1
  do
    p_game.game_space.(p_cur_shape.position.(1) + snd(p_param.shapes.(!(p_cur_shape.shape)).position.(i))).
                      (p_cur_shape.position.(0) + fst(p_param.shapes.(!(p_cur_shape.shape)).position.(i))) 
                       <- int_of_color(p_cur_shape.color)
  done;
  print_matrix(p_param, p_game)
;;

(*
auteurs : Nathan, Melodie
 pose une forme au hasard en fonction des parametres dans le haut de la fenetre
 *)
let cur_shape_choice(p_param : t_param) : t_cur_shape = 
  let l_ind_shape = Random.int(Array.length p_param.shapes)
  and l_color = p_param.colors.(Random.int(Array.length p_param.colors))
  in
   let l_random_x : int = Random.int(p_param.size_x - p_param.shapes.(l_ind_shape).width + 1)
   in
      {position = [|l_random_x; 0|]; color = l_color; shape = ref l_ind_shape}
;;

(*
auteurs : Nathan, Melodie
permet de decaler la forme vers la gauche en fonction de la forme concrete
*)
let move_left(p_cur_shape, p_param, p_game : t_cur_shape * t_param * t_game) : t_cur_shape =
  let l_x = p_cur_shape.position.(0) 
  and l_y = p_cur_shape.position.(1) 
  and l_left_colision : bool ref = ref false
  in
    if l_x = 0 
    then p_cur_shape
    else begin
        for i = 0 to p_param.shapes.(!(p_cur_shape.shape)).height - 1
        do 
          if p_game.game_space.(p_cur_shape.position.(1) + i).(p_cur_shape.position.(0) - 1) <> 0
          then l_left_colision := true
        done;
        if !l_left_colision
        then p_cur_shape 
        else {position = [|l_x - 1; l_y|]; color = p_cur_shape.color; shape = p_cur_shape.shape}
    end
;;

(*
auteurs : Nathan, Melodie
permet de decaler la forme vers la droite en fonction de la forme concrete
*)
let move_right(p_cur_shape, p_param, p_game : t_cur_shape * t_param * t_game) : t_cur_shape =
  let l_x = p_cur_shape.position.(0) 
  and l_y = p_cur_shape.position.(1) 
  and l_right_colision : bool ref = ref false
  in
    if p_cur_shape.position.(0) + p_param.shapes.(!(p_cur_shape.shape)).width = p_param.size_x
    then p_cur_shape
    else begin
        for i = 0 to p_param.shapes.(!(p_cur_shape.shape)).height - 1
        do 
          if p_game.game_space.(
                                p_cur_shape.position.(1) + i).(p_cur_shape.position.(0) +
                                p_param.shapes.(!(p_cur_shape.shape)).width
                               ) <> 0
          then l_right_colision := true
        done;
        if !l_right_colision
        then p_cur_shape
        else {position = [|l_x + 1; l_y|]; color = p_cur_shape.color; shape = p_cur_shape.shape}
    end
;;

(*
auteurs : Nathan, Melodie
permet de decaler la forme vers le bas en fonction de la forme concrete
*)
let move_down(p_cur_shape : t_cur_shape) : t_cur_shape =
  let l_x = p_cur_shape.position.(0) 
  and l_y = p_cur_shape.position.(1)
  in
    {position = [|l_x; l_y + 1|]; color = p_cur_shape.color; shape = p_cur_shape.shape}
;;

(*
auteurs : Nathan, Melodie
permet de decaler la forme tout en bas de la fenetre en fonction de la forme concrete
*)
let move_at_bottom(p_cur_shape, p_param, p_game : t_cur_shape * t_param * t_game) : t_cur_shape = 
  let l_x = p_cur_shape.position.(0) 
  and l_y = ref (p_param.size_y)
  in
    for i = p_cur_shape.position.(0) to p_param.shapes.(!(p_cur_shape.shape)).width + p_cur_shape.position.(0) - 1
    do
      for j = p_cur_shape.position.(1) to p_param.size_y - 1
      do 
        if p_game.game_space.(j).(i) <> 0
        then l_y := min !l_y j
      done;
    done;
    {
     position = [|l_x; !l_y - p_param.shapes.(!(p_cur_shape.shape)).height|];
     color = p_cur_shape.color;
     shape = p_cur_shape.shape
    }
;;

(*
auteur : Romain, Nathan
detecte si une forme touche une autre forme ou le bas de l'espace de jeu
en fonction de la forme concrete, des parametres et de la matrice de jeu
 *)
let colision(p_cur_shape, p_param, p_game : t_cur_shape * t_param * t_game) : bool =
  let l_colision : bool ref = ref false
  in
   if p_cur_shape.position.(1) = p_param.size_y - p_param.shapes.(!(p_cur_shape.shape)).height ||
      p_cur_shape.position.(0) + p_param.shapes.(!(p_cur_shape.shape)).width - 1 >= p_param.size_x ||
      p_cur_shape.position.(0) < 0
   then l_colision := true
   else 
       for i = 0 to p_param.shapes.(!(p_cur_shape.shape)).width - 1
       do 
         if p_game.game_space.(
                               p_cur_shape.position.(1) +
                               snd(p_param.shapes.(!(p_cur_shape.shape)).position.(i)) +
                               p_param.shapes.(!(p_cur_shape.shape)).height).(p_cur_shape.position.(0) +
                               fst(p_param.shapes.(!(p_cur_shape.shape)).position.(i))
                              ) <> 0
         then l_colision := true
      done;
   !l_colision
;;

(*auteurs : Romain, Nathan
 *)
let rot_rgt_shape(p_cur_shape : t_cur_shape) : int =
  match !(p_cur_shape.shape) with
    | 0 -> 1 ;
    | 1 -> 0 ;
    | _ -> !(p_cur_shape.shape)
;;
(* auteurs : Romain, Nathan
effectue une rotation de la piece
 *)
let rot_rgt_base(p_cur_shape, p_param, p_game : t_cur_shape * t_param * t_game): t_cur_shape =
 if !(p_cur_shape.shape) > 1
 then p_cur_shape
 else
   let l_x, l_y = p_cur_shape.position.(0), p_cur_shape.position.(1)
   and l_new_cur_shape : t_cur_shape ref = ref p_cur_shape
   in
     if !(p_cur_shape.shape) = 0 
     then l_new_cur_shape := {
                              position = [| l_x + 1; l_y - 1 |];
                              color = p_cur_shape.color;
                              shape = ref (rot_rgt_shape(p_cur_shape))
                             }
     else l_new_cur_shape := {
                              position = [| l_x - 1; l_y + 1 |];
                              color = p_cur_shape.color;
                              shape = ref (rot_rgt_shape(p_cur_shape))
                             };
     if not (colision(!(l_new_cur_shape), p_param, p_game)) && not (p_cur_shape.position.(1) = 0)
     then !(l_new_cur_shape)
     else p_cur_shape
;;
(*auteurs : Melodie, Lucas
supprime une forme dans l'espace de jeu
 *)
let supp_shape(p_cur_shape, p_param, p_game : t_cur_shape * t_param * t_game) : unit =
  for i = 0 to Array.length (p_param.shapes.(!(p_cur_shape.shape)).position) - 1
  do
    p_game.game_space.(
                       p_cur_shape.position.(1) +
                       snd(p_param.shapes.(!(p_cur_shape.shape)).position.(i))).(p_cur_shape.position.(0) +
                       fst(p_param.shapes.(!(p_cur_shape.shape)).position.(i))
                      ) <- 0
  done;
  print_matrix(p_param, p_game)
;;

(*
auteur: Romain
deplace la forme en fonction de la touche appuyee dans la matrice de jeu
 *)
let move_shape(key, p_param, p_game : char * t_param * t_game) : unit = 
  match key with 
    |'q' -> 
      (
        supp_shape(!(p_game.main_shape), p_param, p_game);
        p_game.main_shape := move_left(!(p_game.main_shape), p_param, p_game);
        draw_shape(!(p_game.main_shape), p_param, p_game), p_param, p_game;
        print_matrix(p_param, p_game)
      );
    |'d' -> 
      (
        supp_shape(!(p_game.main_shape), p_param, p_game);
        p_game.main_shape := move_right(!(p_game.main_shape), p_param, p_game);
        draw_shape(!(p_game.main_shape), p_param, p_game);
        print_matrix(p_param, p_game)
      );
    |'s' -> 
      (
        supp_shape(!(p_game.main_shape), p_param, p_game);
        p_game.main_shape := move_at_bottom(!(p_game.main_shape), p_param, p_game);
        draw_shape(!(p_game.main_shape), p_param, p_game), p_param, p_game;
        print_matrix(p_param, p_game);
        wait 1
      );
    |'z' ->
      (
        supp_shape(!(p_game.main_shape), p_param, p_game);
        p_game.main_shape := rot_rgt_base(!(p_game.main_shape), p_param, p_game);
        draw_shape(!(p_game.main_shape), p_param, p_game);
        print_matrix(p_param, p_game)
      );
    |_ -> ();
;;

(* auteurs : Nathan, Lucas
creer la matrice representant l'espace de jeu
 *)
let create_game_space(p_height, p_width : int * int) : int array array =
    Array.init p_height (fun _ -> Array.make p_width 0);;
;;

(*
auteurs: Melodie, Nathan
ajoute une valeur donnee dans un tableau
*)
let add_to_array(p_array, p_value : 'a array * 'a) : 'a array =
  let l_final_array = arr_make(Array.length p_array + 1, 0)
  in
    for i = 0 to Array.length p_array - 1
    do
      l_final_array.(i) <- p_array.(i)
    done;
    l_final_array.(Array.length l_final_array - 1) <- p_value;
    l_final_array
;;

(*auteurs : Romain, Lucas
deplace toutes les pieces au dessus d'un indice donnee d'une case vers le bas
 *)
let move_all_piece_down(p_param, p_game, p_ind : t_param * t_game * int) : unit =
  for i = p_param.size_y - 1 - p_ind downto 1
  do
    for j = 0 to p_param.size_x - 1
    do 
      p_game.game_space.(i).(j) <- p_game.game_space.(i - 1).(j)
    done;
  done;
  p_game.game_space.(0) <- arr_make(p_param.size_x, 0);
  print_matrix(p_param, p_game)
;;

(*auteurs : Lucas , Melodie
donne un aspect graphique lorsqu'une ligne disparait
*)
let supp_lane(p_ind_array, p_param, p_game : int array * t_param * t_game) : unit =
  for i = 0 to Array.length p_ind_array - 1
  do
    set_color(white);
    fill_rect(p_param.border + p_param.dilat, 
              p_param.border + (p_ind_array.(i) + 1 - i) * p_param.dilat + p_ind_array.(i) - i,
              p_param.size_x * p_param.dilat + p_param.size_x + 1,
              p_param.dilat + 2
              );
    move_all_piece_down(p_param, p_game, p_ind_array.(i) - i)
  done
;;

(*auteurs : Nathan, Romain
supprime les lignes pleines
 *)
let clear_play(p_param, p_game : t_param * t_game) : unit =
  let l_lane_to_supp : int array array = [|[||]|]
  in
    for i = p_param.size_y - 1 downto 0
    do
      let l_is_full : bool ref = ref true
      in
        for j = 0 to p_param.size_x - 1 
        do
          if p_game.game_space.(i).(j) <> 0
          then l_is_full := !l_is_full && true
          else l_is_full := false
        done;
        if !l_is_full
        then l_lane_to_supp.(0) <- add_to_array(l_lane_to_supp.(0), p_param.size_y - 1 - i)
   done;
   if Array.length l_lane_to_supp.(0) > 0
   then supp_lane(l_lane_to_supp.(0), p_param, p_game)
;;

(*
auteurs : Romain, Lucas, Nathan, Melodie
fonction de base du jeu, fait descendre un nombre de fois donnnes des formes et les parametres du jeu
 *)
 let drop_shape(p_nb, p_param, p_game : int * t_param * t_game) : unit = 
  draw_frame(p_param);
  for i = 1 to p_nb 
  do
    let l_cur_shape : t_cur_shape = cur_shape_choice(p_param) 
    and l_start_time : float ref = ref (get_time_of_day())
    in
      clear_play(p_param, p_game);
      p_game.main_shape := l_cur_shape;
      draw_shape(!(p_game.main_shape), p_param, p_game);
      wait 1;
      while not (colision(!(p_game.main_shape), p_param, p_game)) 
      do
        let l_time = get_time_of_day() 
        in
          if l_time -. !(l_start_time) >= !(p_param.tick) 
          then
            (
              supp_shape(!(p_game.main_shape), p_param, p_game);
              draw_shape(move_down(!(p_game.main_shape)), p_param, p_game);
              p_game.main_shape := move_down(!(p_game.main_shape));
              l_start_time := get_time_of_day()
            );
          if key_pressed() 
          then move_shape(read_key(), p_param, p_game);
      done;
  done
;;