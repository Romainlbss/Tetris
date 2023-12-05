#use "AP1inter.ml";;

type t_point = {
                x:int; 
                y:int; 
                }
;;

type t_shape = {
                position: (int * int) array;
                height: int;
                width: int
                }
;;

type t_cur_shape = {
                    position:int array;
                    color:t_color;
                    shape:t_shape
                    }
;;

type t_param = {
                size_x:int; 
                size_y:int; 
                dilat:int; 
                tick:float ref;
                border: int;
                shapes : t_shape array;
                colors : t_color array;
                }
;;

type t_game = {
                game_space : int array array;
                score : int ref;
                main_shape: t_cur_shape ref
              }

let draw_frame(p_param:t_param):unit =
  set_color(black);
  fill_rect(p_param.border, p_param.border,
           (p_param.size_x+2)*p_param.dilat + (p_param.size_x + 1),
           (p_param.size_y+1)*p_param.dilat + (p_param.size_y + 1));
  set_color(white);
  fill_rect(p_param.dilat + p_param.border, p_param.dilat + p_param.border,
           (p_param.size_x)*p_param.dilat + (p_param.size_x + 1),
           (p_param.size_y)*p_param.dilat + (p_param.size_y + 1))
;;

let draw_point(x, y, p_param, p_game, color: int*int*t_param*t_game*t_color):unit =
  set_color(color);
  fill_rect((x + 1) * p_param.dilat + (x + p_param.border + 1),
            (y + 1) * p_param.dilat + (y + p_param.border + 1),
            p_param.dilat, p_param.dilat);
  set_color(black);
  draw_rect((x + 1) * p_param.dilat + (x + p_param.border + 1),
            (y + 1) * p_param.dilat + (y + p_param.border + 1),
            p_param.dilat, p_param.dilat)
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

let color_of_int(nb:int) : t_color = 
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
                else -1;
;;

let int_of_color(color:t_color) : int = 
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
                else -1;
;;


let affiche_matrice(p_param, p_game:t_param*t_game) : unit =
  draw_frame(p_param);
  for i = 0 to p_param.size_y - 1
  do
    (
      for j = 0 to p_param.size_x - 1
      do
        (
          let color = color_of_int(p_game.game_space.(p_param.size_y - 1 - i).(j)) in
            if color <> -1
            then draw_point(j, i, p_param, p_game, color);
        )
      done;
    )
  done;
;;

let open_graph(p_param:t_param):unit =
  open_graph ((p_param.size_x+2)*p_param.dilat + (p_param.size_x + 2 * p_param.border + 1),
           (p_param.size_y+1)*p_param.dilat + (p_param.size_y + p_param.border + 1))
;;

let draw_shape(p_cur_shape, p_param, p_game:t_cur_shape*t_param*t_game):unit = 
  for i = 0 to Array.length p_cur_shape.shape.position - 1
  do p_game.game_space.(p_cur_shape.position.(1) + snd(p_cur_shape.shape.position.(i))).(p_cur_shape.position.(0) + fst(p_cur_shape.shape.position.(i))) <- int_of_color(p_cur_shape.color);
  done;
  affiche_matrice(p_param, p_game);
;;

let cur_shape_choice(p_param:t_param):t_cur_shape = 
  let shape = p_param.shapes.(Random.int(Array.length p_param.shapes)) in
    let color = p_param.colors.(Random.int(Array.length p_param.colors)) in
      let random_x : int = Random.int(p_param.size_x - shape.width + 1) in
        {position = [|random_x; 0|]; color = color; shape = shape}
;;

let move_left(p_cur_shape, p_param, p_game:t_cur_shape*t_param*t_game): t_cur_shape =
  let x = p_cur_shape.position.(0) 
  and y = p_cur_shape.position.(1) 
  and left_colision : bool ref = ref false in
    if x = 0 
    then p_cur_shape
    else
      (
        for i = 0 to p_cur_shape.shape.height - 1
          do 
            (
              if p_game.game_space.(p_cur_shape.position.(1) + i).(p_cur_shape.position.(0) - 1) <> 0
              then 
                (
                  left_colision := true;
                  print_int(p_cur_shape.position.(1) + i);
                  print_int(p_cur_shape.position.(0) - 1);
                )
              
            )
          done;
          if !left_colision
          then
            (
              p_cur_shape;
            )
            
          else {position = [|x - 1; y|]; color = p_cur_shape.color; shape = p_cur_shape.shape}
      )
;;

let move_right(p_cur_shape, p_param, p_game:t_cur_shape*t_param*t_game): t_cur_shape =
  let x = p_cur_shape.position.(0) 
  and y = p_cur_shape.position.(1) 
  and right_colision : bool ref = ref false in
    if p_cur_shape.position.(0) + p_cur_shape.shape.width = p_param.size_x
    then p_cur_shape
    else
      (
        for i = 0 to p_cur_shape.shape.height - 1
        do 
        (
          if p_game.game_space.(p_cur_shape.position.(1) + i).(p_cur_shape.position.(0) + p_cur_shape.shape.width) <> 0
          then right_colision := true;
        )
        done;
        if !right_colision
        then p_cur_shape
        else {position = [|x + 1; y|]; color = p_cur_shape.color; shape = p_cur_shape.shape}
      )
;;

let move_down(p_cur_shape:t_cur_shape): t_cur_shape =
  let x = p_cur_shape.position.(0) 
  and y = p_cur_shape.position.(1) in
    {position = [|x; y + 1|]; color = p_cur_shape.color; shape = p_cur_shape.shape}
;;

let move_at_bottom(p_cur_shape, p_param, p_game:t_cur_shape*t_param*t_game): t_cur_shape = 
  let x = p_cur_shape.position.(0) 
  and y = ref (p_param.size_y) in
    for i = p_cur_shape.position.(0) to p_cur_shape.shape.width + p_cur_shape.position.(0) - 1
    do 
      (
        for j = p_cur_shape.position.(1) to p_param.size_y - 1
        do 
          (
            if p_game.game_space.(j).(i) <> 0
            then 
              (
                y := min !y j;
              )
            
          )
        done;
      )
      done;
    {position = [|x; !y - p_cur_shape.shape.height|]; color = p_cur_shape.color; shape = p_cur_shape.shape}
;;

let colision(p_cur_shape, p_param, p_game:t_cur_shape*t_param*t_game): bool =
  let colision : bool ref = ref false in
    if p_cur_shape.position.(1) = p_param.size_y - p_cur_shape.shape.height;
    then colision := true
    else 
      for i = 0 to p_cur_shape.shape.width - 1
      do 
        if p_game.game_space.(p_cur_shape.position.(1) + snd(p_cur_shape.shape.position.(i)) + p_cur_shape.shape.height).(p_cur_shape.position.(0) + fst(p_cur_shape.shape.position.(i))) <> 0
        then 
        (
          colision := true; 
        );
     done;
  !colision
;;

let supp_shape(p_cur_shape, p_param, p_game : t_cur_shape*t_param*t_game) : unit =
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

let final_intert(p_shape, p_param, p_game:t_cur_shape*t_param*t_game):unit =
  let nb_color : int ref = ref 0 in
    if p_shape.color = Graphics.green
    then nb_color := 1
    else
      if p_shape.color = Graphics.yellow
      then nb_color := 2
      else
        if p_shape.color = Graphics.cyan
        then nb_color := 7
        else
          if p_shape.color = Graphics.blue
          then nb_color := 4
          else
            if p_shape.color = Graphics.red
            then nb_color := 5
            else
              if p_shape.color = Graphics.magenta
              then nb_color := 6
              else nb_color := 3;
    for i = 0 to 3
    do p_game.game_space.(snd(p_shape.shape.position.(i)) + p_shape.position.(1)).(p_shape.position.(0) + fst(p_shape.shape.position.(i))) <- !nb_color
    done;
;;


let move_shape(key, p_param, p_game:char*t_param*t_game):unit = 
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

let create_game_space(p_height, p_width:int*int) : int array array =
  Array.init p_height (fun _ -> Array.make p_width 0);;
  (*
    Ne fonctionne pas 
    arr_make(p_height , arr_make(p_width, 0))
  *)
;;

let add_to_array(array, value : 'a array* 'a) : 'a array =
  let final_array = arr_make(Array.length array + 1, 0) in
    for i = 0 to Array.length array - 1
    do final_array.(i) <- array.(i);
    done;
    final_array.(Array.length final_array - 1) <- value;
    final_array;
;;

let move_all_piece_down(p_param, p_game, ind:t_param*t_game*int) : unit =
  for i = p_param.size_y - 1 - ind downto 1
  do 
    (
      for j = 0 to p_param.size_x - 1
      do 
        (
          p_game.game_space.(i).(j) <- p_game.game_space.(i - 1).(j);
        )
      done;
    )
  done;
  p_game.game_space.(0) <- arr_make(p_param.size_x, 0);
  affiche_matrice(p_param, p_game);
;;

(*Si on veux un aspect graphique*)
let supp_lane(ind_array, p_param, p_game : int array*t_param*t_game) : unit =
  for i = 0 to Array.length ind_array - 1
  do
    (
      set_color(white);
      fill_rect(p_param.border + p_param.dilat, 
                p_param.border + (ind_array.(i) + 1 - i) * p_param.dilat + ind_array.(i) - i,
                p_param.size_x * p_param.dilat + p_param.size_x + 1,
                p_param.dilat + 2
              );
      move_all_piece_down(p_param, p_game, ind_array.(i) - i);
    )
  done;
;;

let clear_play (p_param, p_game : t_param*t_game) : unit =
  let lane_to_supp : int array array = [|[||]|] in
    for i = p_param.size_y - 1 downto 0
    do 
      (
        let is_full : bool ref = ref true in
          for j = 0 to p_param.size_x - 1 
          do 
            (
              if p_game.game_space.(i).(j) <> 0
              then is_full := !is_full && true
              else is_full := false;
            )
          done;
          if !is_full
          then 
            (
              lane_to_supp.(0) <- add_to_array(lane_to_supp.(0), p_param.size_y - 1 - i);
            )
      )
    done;
    if Array.length lane_to_supp.(0) > 0
    then supp_lane(lane_to_supp.(0), p_param, p_game);
        
;;

let drop_shape(p_nb, p_param, p_game:int*t_param*t_game) = 
    draw_frame(p_param);
    for i = 1 to p_nb do
      let cur_shape : t_cur_shape = cur_shape_choice(p_param) in
        if !(p_game.score) <> 0
        then final_intert(!(p_game.main_shape), p_param, p_game);
        clear_play(p_param, p_game);
        print_newline();
        wait 1;
        p_game.score := !(p_game.score) + 1;
        p_game.main_shape := cur_shape;
          let start_time : float ref = ref (get_time_of_day())in
            draw_shape(!(p_game.main_shape), p_param, p_game);
            while not (colision(!(p_game.main_shape), p_param, p_game)) do
              let time = get_time_of_day() in
                if time -. !(start_time) >= !(p_param.tick) 
                then
                  (
                    supp_shape(!(p_game.main_shape), p_param, p_game);
                    draw_shape(move_down(!(p_game.main_shape)), p_param, p_game);
                    p_game.main_shape := move_down(!(p_game.main_shape));
                    start_time := get_time_of_day();
                    print_newline();
                  );
                if key_pressed() then
                  let key : char = read_key() in 
                    move_shape(key, p_param, p_game);
              done;
        done;
;;


(* Main *)

let tetris_v6():unit = 
  
  let horizontal_bar:t_shape = {position = [|(0, 0); (1, 0); (2, 0); (3, 0)|]; height = 1; width = 4};
  and vertical_bar:t_shape = {position = [|(0, 0); (0, 1); (0, 2); (0, 3)|]; height = 4; width = 1};
  and square:t_shape = {position = [|(0, 0); (1, 0); (0, 1); (1, 1)|]; height = 2; width = 2} in
  let init:t_param = {size_x = 10; size_y = 20; dilat = 25; tick = ref 0.2; border = 20; shapes = [|horizontal_bar; vertical_bar; square|]; colors = [|green; yellow; orange; blue; red; magenta; cyan|]} in
    let game:t_game = { game_space = create_game_space(init.size_y, init.size_x); score = ref 0; main_shape = ref (cur_shape_choice(init))}
    in
      open_graph(init);
      drop_shape(100, init, game);
;;


