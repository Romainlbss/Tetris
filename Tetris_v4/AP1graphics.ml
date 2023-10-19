
(* ------------------------ *)
(*  pause durant execution  *)
(* ------------------------ *)
(* ------------------------ *)
let wait(n : int) : unit =
  Unix.sleep(n)
 ;;

let get_time_of_day() : float =
  Unix.gettimeofday()
;;

 (* ------------------------ *)
 (*        graphique         *)
 (* ------------------------ *)
 
 let open_graph (dx, dy : int * int) : unit = 
   if Sys.os_type = "Unix" then  
     let s = ":0 "^string_of_int(dx)^"x"^string_of_int(dy) in
       Graphics.open_graph s
   else
     let s = string_of_int(dx)^"x"^string_of_int(dy) in
       Graphics.open_graph s
 ;;
 
 let close_graph () : unit = Graphics.close_graph() ;;
 
 let clear_graph () : unit = Graphics.clear_graph() ;;
 
 let resize_window (x, y : int * int) : unit = Graphics.resize_window x y ;;
 
 
 let moveto (x, y : int * int) : unit = Graphics.moveto x y ;;
 
 let lineto (x, y : int * int) : unit = Graphics.lineto x y ;;
 
 let plot(x, y : int * int) : unit = Graphics.plot x y ;;
 
 let current_point () : int * int = Graphics.current_point() ;;
 
 let draw_poly_line (t : (int * int) array) : unit = Graphics.draw_poly_line t ;;
 
 let draw_circle (x, y, r : int * int * int) : unit = Graphics.draw_circle x y r ;;
 
 let draw_ellipse (x, y, dx, dy : int * int * int * int) : unit = 
           Graphics.draw_ellipse x y dx dy 
 ;;
 
 let draw_rect (x, y, dx, dy : int * int * int * int) : unit = 
   if Sys.os_type = "Unix" then  
     Graphics.draw_rect x y (dx- 1) (dy - 1)
   else
     Graphics.draw_rect x (y+1) (dx-1) (dy-1)
 ;;
 
 let fill_rect (x, y, dx, dy : int * int * int * int) : unit = 
   if Sys.os_type = "Unix" then  
     Graphics.fill_rect x y (dx- 1) (dy - 1)
   else
     Graphics.fill_rect x y dx dy
 ;;
 
 let fill_poly(t : (int * int) array) : unit = Graphics.fill_poly t ;;
 
 let fill_circle (x, y, r : int * int * int) : unit = Graphics.fill_circle x y r ;;
 
 
 let fill_ellipse (x, y, dx, dy : int * int * int * int) : unit = 
           Graphics.fill_ellipse x y dx dy 
 ;;
 
 let set_line_width(e : int) : unit = Graphics.set_line_width e ;;
 
 let draw_string (s : string) : unit = Graphics.draw_string s ;;
 
 let set_text_size (n : int) : unit = 
   let s = "-*-courier-medium-r-*-*-"^string_of_int(n)^"-*"
   in Graphics.set_font s ;;
 
 
 
 type t_color = Graphics.color ;;
 
 let black : t_color = Graphics.black ;;
 let blue : t_color = Graphics.blue ;;
 let red : t_color = Graphics.red ;;
 let green : t_color = Graphics.green ;;
 let white : t_color = Graphics.white ;;
 let yellow : t_color = Graphics.yellow ;;
 let cyan : t_color = Graphics.cyan ;;
 let magenta : t_color = Graphics.magenta ;;
 let grey : t_color = 128 * 256 * 256 + 128 * 256 + 128 ;;
 
 let color_of_rgb (r, g, b : int * int * int) : t_color =
   let valid(x : int) : bool = ((0 <= x) && x <= 255) in
     if not(valid(r)) ||  not(valid(g)) || not(valid(b))
     then failwith("erreur color_of_rgb : valeurs invalides")
     else Graphics.rgb r g b
 ;;
 
 
 
 let set_color (color : t_color) : unit = Graphics.set_color color ;;
 
 (* ------------------------ *)
 (*   controle evenements    *)
 (* ------------------------ *)
 
 
 let key_pressed () : bool =
   Graphics.key_pressed()
 ;;
 
 let read_key () : char =
   Graphics.read_key()
 ;;
 
 let mouse_pos () : int * int =
   Graphics.mouse_pos()
 ;;
 
 let button_down () : bool = 
   Graphics.button_down()
 ;;