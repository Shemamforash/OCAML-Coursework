open Str;;
open Scanf;;

let line_stream_of_channel =
   Stream.from (fun _ -> try Some (input_line stdin)
                         with End_of_file -> None);;
let lines = line_stream_of_channel;;

let getfromstream () =
  try (
    Some (Stream.next lines)
  ) with Stream.Failure -> None

let rec stringlisttointlist strlist intlist = match strlist with
  | [] -> intlist
  | hd :: [] -> (int_of_string hd) :: intlist
  | hd :: tl -> stringlisttointlist tl ((int_of_string hd) :: intlist)

let rec printlist intlist = match intlist with
| [] -> print_string ""
| hd :: [] -> print_string hd
| hd :: tl -> print_string hd ; printlist tl

let readstream () : int list=
  let arr = [] in
      let rawline = getfromstream () in (match rawline with
        | None -> []
        | Some line -> let linearr = Str.split (Str.regexp " ") line in List.rev (stringlisttointlist linearr arr))

let rec write argument = match argument with
  | [] -> print_string ""
  | hd::[] -> print_int hd
  | hd::tl -> print_int hd ; print_string " " ; write tl
