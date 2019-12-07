open ANSITerminal

(** [string_of_chars chars] is the string containing the chars in [chars],
    in order. *)
let string_of_chars chars = 
  let buf = Buffer.create 16 in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf

(** [str_to_arr str] is the grid representation of [s], with rows
    delimited by newlines. *)
let str_to_arr str =
  let explode s = List.init (String.length s) (String.get s)
  in
  String.split_on_char '\n' str
  |> Array.of_list
  |> Array.map
    (fun s -> s
              |> explode
              |> Array.of_list)

(** [arr_to_str arr] is the string representation of [arr], with newlines
    delimiting rows. *)
let arr_to_str arr =
  arr
  |> Array.to_list
  |> List.map
    (fun a -> a |> Array.to_list |> string_of_chars)
  |> String.concat "\n"

(** [make_word ls] is the horizontal concatenation of all of the strings
    in [ls], accounting for newlines.
    For instance, [make_word [a; b]], where [a] is ["18\n27"] and [b] is
    ["3\n4"], should be ["183\n274"].
*)
let rec make_word =
  let combine_letters s1 s2 = 
    let s1s = String.split_on_char '\n' s1 in
    let s2s = String.split_on_char '\n' s2 in
    List.map2 (fun a b -> a^b) s1s s2s
    |> String.concat "\n"
  in function
    | [] -> ""
    | a::[] -> a
    | a::b::t -> make_word ((combine_letters a b)::t)

(** [window arr w h t] is the window view of the logo in [arr] with width
    [w] and height [h], at the time-step corresponding to [t]. *)
let window arr w h t : string =
  let new_arr = Array.make_matrix h w ' ' in
  for i = 0 to (h-1) do
    for x = 0 to (w-1) do
      let j = x - w + t in
      new_arr.(i).(x) <- (
        try arr.(i).(j) with | _ -> ' '
      )
    done;
  done;
  new_arr |> arr_to_str

(** [scroll arr w h v] makes the string corresponding to [arr] scroll across
    the screen with width [w], height [h], and velocity [v]. *)
let scroll arr w h v = 
  let t = ref 0 in
  let inc x = x := (!x + 1) in
  let len_str = Array.length arr.(0) in
  while (!t)-w < len_str + 5 do
    (* ANSITerminal.(erase Screen) |> ignore; *)
    Sys.command("clear") |> ignore;
    window arr w h (!t) |> print_endline;
    Unix.sleepf (1. /. v);
    inc t;
  done

(** [scroll_words ls w] makes the string corresponding to horizontal
    concatenation of the strings represented in [ls] scroll across the screen,
    with width [w]. *)
let scroll_words letter_list w =
  scroll (make_word letter_list |> str_to_arr) w 10 75.
(* 75.0 is the "speed" *)

(** [letters ()] is a list of the letters in the battleship logo. *)
let letters () = match
    List.map Helpers.from_file
      [ "load_screen_a"; "load_screen_b"; "load_screen_e"; "load_screen_h";
        "load_screen_i"; "load_screen_l"; "load_screen_p"; "load_screen_s";
        "load_screen_tt";"load_screen_ships" ]
  with
  | [a; b; e; h; i; l; p; s; tt; ships] -> a, b, e, h, i, l, p, s, tt, ships
  | _ -> failwith "oops"

let scroll_battleship () =
  let width, _ = ANSITerminal.size () in
  let right_buffer = 2 in
  try
    let a, b, e, h, i, l, p, s, tt, ships = letters () in
    scroll_words [b; a; tt; l; e; s; h; i; p; ships] (width - right_buffer)
  with | _ -> ()





(* for my reference: *)

(* "------------\n" ^
   "||     \    \n" ^
   "|| ____/    \n" ^
   "||     \    \n" ^
   "||      |   \n" ^
   "||_____/    " *)

(* "---------------\n" ^
   "    //\\       \n" ^
   "   //  \\      \n" ^
   "  //____\\     \n" ^
   " //      \\    \n" ^
   "//        \\   " *)

(* "--------------
    | |           
    | |           
    | |           
    | |______     
    \ ______/     " *)

(* "---------
     /===    
    ||       
     \==\    
        ||   
     ===/    " *)

(* let p = "
   _______  
   |🏴🏳️🏴|  
   |🏳️🏴🏳️|  
   ------   
   |         
   |         " *)

