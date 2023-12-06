#use "src_v6.ml";;

let horizontal_bar : t_shape =  {position = [|(0, 0); (1, 0); (2, 0); (3, 0)|];
                                 height = 1;
                                 width = 4
                                }
;;
let cur_horizontal_bar : t_cur_shape = { position =[|3 ;0|];
                                         color = red;
                                         shape = horizontal_bar
                                       }
;;
let test_param : t_param = { size_x = 10;
                             size_y = 20;
                             dilat = 25;
                             tick = ref 2.;
                             border = 10;
                             shapes = [|horizontal_bar|];
                             colors = [|red|]
                           }
;;

let test_play : t_play = { game_space = create_game_space(test_param.size_y, test_param.size_x); score = ref 0; main_shape = ref (cur_shape_choice(test_param)) }
;;

let clean_matrix(p_param, p_play:t_param*t_play) : unit =
  for i = 0 to p_param.size_y - 1
  do
    (
      for j = 0 to p_param.size_x - 1
      do
        p_play.game_space.(i).(j) <- 0
      done;
    )
  done;
;;

(*ouvre une fenetre pendant 5 secondes*)
let test_open_graph(p_param:t_param) : unit =
  open_graph(p_param);
  wait(5);
  close_graph()
;;

(*fait apparaitre l'espace de jeu pendant 5 secondes*)
let test_draw_frame(p_param, p_play:t_param*t_play) : unit =
  open_graph(p_param);
  draw_frame(p_param);
  wait(5);
  clean_matrix(p_param, p_play);
  close_graph()
;;

(*dessine un point en position (3,19) de couleur rouge pendant 5 secondes*)
let test_draw_point((p_point_x,p_point_y),p_param,p_play,p_color: (int*int)*t_param*t_play*t_color) : unit =
  open_graph(p_param);
  draw_frame(p_param);
  draw_point(p_point_x,p_point_y,p_param,p_play,p_color);
  clean_matrix(p_param, p_play);
  wait(5);
  close_graph()
;;



(*dessine une barre horizontal rouge en (3,19) pendant 5 secondes*)
let test_draw_shape(p_cur_shape,p_param,p_play: t_cur_shape*t_param*t_play) : unit =
  open_graph(p_param);
  draw_frame(p_param);
  draw_shape(p_cur_shape,p_param,p_play);
  clean_matrix(p_param, p_play);
  wait(5);
  close_graph()
;;

(*dessine une forme au hasard sur le haut de la fenetre*)
let test_cur_shape_choice(p_param,p_play:t_param*t_play) : unit =
  open_graph(p_param);
  draw_frame(p_param);
  p_play.main_shape := cur_shape_choice(p_param);
  draw_shape(!(p_play.main_shape),p_param,p_play);
  clean_matrix(p_param, p_play);
  wait(5);
  close_graph()
;;

(*la barre horizontal rouge bouge de 1 carreau a gauche*)
let test_move_left(p_cur_shape,p_param,p_play: t_cur_shape*t_param*t_play) : unit =
  open_graph(p_param);
  draw_frame(p_param);
  draw_shape(p_cur_shape,p_param, p_play);
  wait(3);
  supp_shape(p_cur_shape,p_param,p_play);
  draw_shape(move_left(p_cur_shape, p_param, p_play), p_param, p_play);
  clean_matrix(p_param, p_play);
  wait(3);
  close_graph()
;;

(*la barre horizontal rouge bouge de 1 carreau a droite*)
let test_move_right(p_cur_shape,p_param,p_play: t_cur_shape*t_param*t_play) : unit =
  open_graph(p_param);
  draw_frame(p_param);
  draw_shape(p_cur_shape,p_param,p_play);
  wait(3);
  supp_shape(p_cur_shape,p_param,p_play);
  draw_shape(move_right(p_cur_shape,p_param,p_play), p_param,p_play);
  clean_matrix(p_param, p_play);
  wait(3);
  close_graph()
;;

(*la barre horizontal rouge bouge de 1 carreau en bas*)
let test_move_down(p_cur_shape,p_param,p_play: t_cur_shape*t_param*t_play) : unit =
  open_graph(p_param);
  draw_frame(p_param);
  draw_shape(p_cur_shape,p_param,p_play);
  wait(3);
  supp_shape(p_cur_shape,p_param,p_play);
  draw_shape(move_down(p_cur_shape),p_param,p_play);
  clean_matrix(p_param, p_play);
  wait(3);
  close_graph()
;;

(*la barre horizontal rouge bouge en bas de la fenetre de jeu*)
let test_move_at_bottom(p_cur_shape,p_param, p_play: t_cur_shape*t_param*t_play) : unit =
  open_graph(p_param);
  draw_frame(p_param);
  draw_shape(p_cur_shape,p_param,p_play);
  wait(3);
  supp_shape(p_cur_shape,p_param,p_play);
  draw_shape(move_at_bottom(p_cur_shape,p_param, p_play), p_param, p_play);
  clean_matrix(p_param, p_play);
  wait(3);
  close_graph()
;;

(*on peut bouger la piece avec les touches d,h,v jusqu'a ce que la piece soit en bas de la fenetre*)
let test_move_shape(p_param,p_play: t_param*t_play):unit =
  open_graph(p_param);
  let l_start_time : float ref = ref (get_time_of_day())
  in
   while not(colision(!(p_play.main_shape), p_param, p_play))
   do
     let l_time = get_time_of_day() in
      if l_time -. !(l_start_time) >= !(p_param.tick) then
        (
          supp_shape(!(p_play.main_shape),p_param,p_play);
          draw_shape(move_down(!(p_play.main_shape)), p_param, p_play);
          p_play.main_shape := move_down(!(p_play.main_shape)); 
          l_start_time := get_time_of_day();
         );
      if key_pressed() then
       let l_key : char = read_key() in 
       move_shape(l_key, p_param,p_play);
   done;
   close_graph()
;;

(*fait apparaitre 5 forme aleatoire que l'on peut bouger*)
let test_drop_changer(p_nb,p_play,p_param: int*t_play*t_param) : unit =
  open_graph(p_param);
  drop_shape(p_nb,p_param,p_play)
;;

let test_int_of_color() : unit =
  if int_of_color(red) <> 5
  then failwith("Erreur test_int_of_color")
  else print_endline("test_int_of_color : Success");
;;

let test_color_of_int() : unit =
  if color_of_int(5) <> Graphics.red
  then failwith("Erreur test_color_of_int")
  else print_endline("test_color_of_int : Success")
;;
(*Doit afficher un espace de jeux avec des carrés verts partout*)
let test_affiche_matrice(p_param:t_param):unit =
  let l_play:t_play = {game_space = Array.init p_param.size_y (fun _ -> Array.make p_param.size_x 1);score = ref 0 ;main_shape = ref {position = [|0; 1|]; color = red; shape = horizontal_bar}} in
    open_graph(p_param);
    affiche_matrice(p_param, l_play);
    wait 5;
    close_graph()
;;

let test_colision(p_param, p_play:t_param*t_play) : unit =
  let cur_shape : t_cur_shape = {position = [|0; p_param.size_y - 1|]; color = red; shape = horizontal_bar} in
    if colision(move_left(cur_shape, p_param, p_play), p_param, p_play)
    then print_endline("test_colision : Success")
    else failwith("Erreur test_colision");
;;

let test_supp_shape(p_cur_shape, p_param, p_play:t_cur_shape*t_param*t_play) : unit =
  open_graph(p_param);
  draw_shape(p_cur_shape, p_param, p_play);
  wait 2;
  supp_shape(p_cur_shape, p_param, p_play);
  wait 4;
  close_graph()
;;

let test_final_insert(p_param, p_play:t_param*t_play) : unit =
  let l_cur_shape : t_cur_shape = {position = [|0; 3|]; color = red; shape = horizontal_bar} in
    open_graph(p_param);
    draw_shape(l_cur_shape, p_param, p_play);
    for i = 0 to Array.length l_cur_shape.shape.position - 1
    do
      (
        if p_play.game_space.(l_cur_shape.position.(1) + snd(l_cur_shape.shape.position.(i))).(l_cur_shape.position.(0) + fst(l_cur_shape.shape.position.(i))) <> 0
        then print_endline("test_final_insert : Success")
        else failwith("Erreur test_final_insert")
      )
    done;
;;

let test_create_game_space():unit =
  let game_space = create_game_space(10, 20) in
    if Array.length game_space.(0) = 20 && Array.length game_space = 10
    then print_endline("test_create_game_space : Success")
    else failwith("Erreur : test_create_game_space")
;;

let test_add_to_array():unit =
  let l_array = [|2; 4|] in
    let l_final_array = add_to_array(l_array, 6) in 
    if l_final_array.(2) = 6
    then print_endline("test_add_to_array : Success")
    else failwith("Erreur test_add_to_array")
;;

(*lance le test demander*)
let tetris_v5():unit = 
  let l_horizontal_bar:t_shape = {position = [|(0, 0); (1, 0); (2, 0); (3, 0)|]; height = 1; width = 4};
  and l_vertical_bar:t_shape = {position = [|(0, 0); (0, 1); (0, 2); (0, 3)|]; height = 4; width = 1};
  and l_square:t_shape = {position = [|(0, 0); (1, 0); (0, 1); (1, 1)|]; height = 2; width = 2}
  in
   let l_init:t_param = {size_x = 10;
                       size_y = 20;
                       dilat = 25;
                       tick = ref 0.5;
                       border = 20;
                       shapes = [|l_horizontal_bar; l_vertical_bar; l_square|];
                       colors = [|green; yellow; blue; red; magenta; cyan|]
                      };
   in
      let l_play:t_play = {game_space = create_game_space(l_init.size_y, l_init.size_x);score = ref 0 ;main_shape = ref {position = [|0; 1|]; color = red; shape = l_square}}                
      in
        open_graph(l_init);
        clean_matrix(l_param, l_play);
        drop_shape(100, l_init,l_play)
;;


(*test_open_graph(test_param);;
wait 1;
test_draw_frame(test_param, test_play);;
wait 1;
test_draw_point((0,0),test_param, test_play, red);;
wait 1;
test_draw_shape(cur_horizontal_bar, test_param, test_play);;
wait 1;
test_cur_shape_choice(test_param,test_play);;
wait 1;
test_move_left(cur_horizontal_bar, test_param,test_play);;
wait 1;
test_move_right(cur_horizontal_bar, test_param,test_play);;
wait 1;
test_move_down(cur_horizontal_bar, test_param,test_play);;
wait 1;
test_move_at_bottom(cur_horizontal_bar, test_param,test_play);;
wait 1;
test_move_shape(test_param,test_play);;*)
wait 1;
test_int_of_color();
wait 1;
test_color_of_int();
wait 1;
test_affiche_matrice(test_param);
wait 1;
test_colision(test_param, test_play);
wait 1;
test_supp_shape(cur_horizontal_bar, test_param, test_play);
wait 1;
test_final_insert(test_param, test_play);
wait 1;
test_create_game_space();
wait 1;
test_add_to_array();
wait 1;
test_drop_changer(5, test_play, test_param);;