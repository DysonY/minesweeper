(* printModel : model -> unit *)
let printModel model =
  for row = 0 to Array.length model - 1 do
    for col = 0 to Array.length model.(0) - 1 do
      Lib.pfmt "%b " model.(row).(col).isMine
    done ;
    print_string "\n"
  done ;
  print_string "\n"

(* printAdjacent : model -> unit *)
let printAdjacent model =
  for row = 0 to Array.length model - 1 do
    for col = 0 to Array.length model.(0) - 1 do
      Lib.pfmt "%d " model.(row).(col).adjacent
    done ;
    print_string "\n"
  done ;
  print_string "\n"

(* printAdjacent : model -> unit *)
let printRevealed model =
  for row = 0 to Array.length model - 1 do
    for col = 0 to Array.length model.(0) - 1 do
      match model.(row).(col).state with
      | Revealed -> Lib.pfmt "O "
      | Hidden -> Lib.pfmt "X "
    done ;
    print_string "\n"
  done ;
  print_string "\n"
