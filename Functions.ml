let read (val : int) : list =
  let infile = open_in Sys.argv.(1) in
  let columnarr = ref [] in try while (true) do
        let line = input_line infile in
        let num = int_of_string (String.make 1 line.[val*2]) in columnarr := num::!columnarr;
      done; !columnarr
    with e -> close_in infile;
      List.rev !columnarr;;

let write val = Printf.printf "%d" val;;
