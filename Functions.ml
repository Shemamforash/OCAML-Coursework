open Str;;

let rec write argument = match argument with
  | [] -> print_string ""
  | hd::[] -> print_int hd
  | hd::tl -> print_int hd ; print_string ", " ; write tl

let read : int list =
  let columnarr = ref [] in
    try while (true) do
        let line = Str.split (Str.regexp " ") (input_line stdin) in
        let num = int_of_string (List.nth line 0) in columnarr := num::!columnarr; print_int num ;
      done; !columnarr
    with e -> List.rev !columnarr;;
