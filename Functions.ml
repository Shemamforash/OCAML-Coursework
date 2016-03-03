let write argument = Printf.printf "%d" argument ; Printf.printf "EOL";;

let read : int list =
  let columnarr = ref [] in
    try while (true) do
        let line = input_line stdin in
        let num = int_of_string (String.make 1 line.[0]) in columnarr := num::!columnarr; write num ;
      done; !columnarr
    with e -> List.rev !columnarr;;
