open ANSITerminal


let str_to_arr str =
  let explode s = List.init (String.length s) (String.get s)
  in
  String.split_on_char '\n' str
  |> Array.of_list
  |> Array.map
    (fun s -> s
              |> explode
              |> Array.of_list)

let string_of_chars chars = 
  let buf = Buffer.create 16 in
  List.iter (Buffer.add_char buf) chars;
  Buffer.contents buf

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

let arr_to_str arr =
  arr
  |> Array.to_list
  |> List.map
    (fun a -> a |> Array.to_list |> string_of_chars)
  |> String.concat "\n"

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

let scroll_words letter_list w =
  scroll (make_word letter_list |> str_to_arr) w 10 75.
(* 75.0 is the "speed" *)

let letters () = match
    List.map Helpers.from_file
      [ "load_screen_a"; "load_screen_b"; "load_screen_e"; "load_screen_h";
        "load_screen_i"; "load_screen_l"; "load_screen_p"; "load_screen_s";
        "load_screen_tt";"load_screen_ships" ]
  with
  | [a; b; e; h; i; l; p; s; tt; ships] -> a, b, e, h, i, l, p, s, tt, ships
  | _ -> "", "", "", "", "", "", "", "", "", ""

let scroll_battleship () =
  let width, _ = ANSITerminal.size () in
  let a, b, e, h, i, l, p, s, tt, ships = letters () in
  scroll_words [b; a; tt; l; e; s; h; i; p; ships] (width - 2)





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

