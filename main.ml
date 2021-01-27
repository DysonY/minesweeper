type state = Revealed | Hidden

type tile = { state : state
            ; isMine : bool
            ; adjacent : int}

type model = tile array array

let defaultTile =
  { state = Hidden
  ; isMine = false
  ; adjacent = 0}

(* countAdjacent : model -> int -> int -> int *)
let countAdjacent model row col =
  let count = ref 0 in
  let rows = Array.length model in
  let cols = Array.length model.(0) in
  if row > 0 then
    begin
      if model.(row - 1).(col).isMine then count := !count + 1;
      if col > 0 then
        if model.(row - 1).(col - 1).isMine then count := !count + 1;
      if col < cols - 1 then
        if model.(row - 1).(col + 1).isMine then count := !count + 1;
    end;
  if row < rows - 1 then
    begin
      if model.(row + 1).(col).isMine then count := !count + 1;
      if col > 0 then
        if model.(row + 1).(col - 1).isMine then count := !count + 1;
      if col < cols - 1 then
        if model.(row + 1).(col + 1).isMine then count := !count + 1;
    end;
  if col > 0 then
    if model.(row).(col - 1).isMine then count := !count + 1;
  if col < cols - 1 then
    if model.(row).(col + 1).isMine then count := !count + 1
  ; !count

(* initialModel : int -> int -> int -> model *)
(* Randomly populate a grid with specified number of mines *)
let initialModel mines rows cols =
  let board = Array.make_matrix rows cols defaultTile in
  let checks = Array.make_matrix rows cols false in
  let n = ref mines in
  (* First pass: place mines *)
  while !n > 0 do
    try
      while true do
        let row = Random.int rows in
        let col = Random.int cols in
        if not checks.(row).(col) then
          begin
            checks.(row).(col) <- true;
            board.(row).(col) <- { board.(row).(col) with isMine = true };
            raise Exit
          end
      done
    with
    | Exit -> ()
    ; n := !n - 1
  done;
  (* Second pass: count adjacent *)
  for row = 0 to rows - 1 do
    for col = 0 to cols - 1 do
      let adj = countAdjacent board row col in
      board.(row).(col) <- { board.(row).(col) with adjacent = adj }
    done
  done
  ; board

(* revealTile : model -> int -> int -> model *)
(* Flood fill on click *)
let revealTile model x y =
  let row = x / 50 in
  let col = y / 50 in
  let numRows = Array.length model in
  let numCols = Array.length model.(0) in
  let checkedTiles = ref [] in
  if model.(row).(col).isMine then exit 0;
  let rec floodFill model row col =
    checkedTiles := !checkedTiles @ [(row, col)];
    match model.(row).(col).adjacent with
    | 0 ->
      if row > 0 && not (List.mem (row - 1, col) !checkedTiles) then
        begin
          floodFill model (row - 1) col;
          if col > 0 && not (List.mem (row - 1, col - 1) !checkedTiles) then
            floodFill model (row - 1) (col - 1);
          if col < numCols - 1 && not (List.mem (row - 1, col + 1) !checkedTiles) then
            floodFill model (row - 1) (col + 1)
        end;
      if row < numRows - 1 && not (List.mem (row + 1, col) !checkedTiles) then
        begin
          floodFill model (row + 1) col;
          if col > 0 && not (List.mem (row + 1, col - 1) !checkedTiles) then
            floodFill model (row + 1) (col - 1);
          if col < numCols - 1 && not (List.mem (row + 1, col + 1) !checkedTiles) then
            floodFill model (row + 1) (col + 1)
        end;
      if col > 0 && not (List.mem (row, col - 1) !checkedTiles) then
        floodFill model row (col - 1);
      if col < numCols - 1 && not (List.mem (row, col + 1) !checkedTiles) then
        floodFill model row (col + 1);
      model.(row).(col) <- {model.(row).(col) with state = Revealed}
    | _ -> model.(row).(col) <- {model.(row).(col) with state = Revealed}
  in
  floodFill model row col
  ; model

(* Revealed tile *)
let lightGray =
  Image.rectangle 40. 40. Color.lightGray

(* Hidden tile *)
let darkGray =
  Image.rectangle 40. 40. ~fill:true Color.darkGray

(* dispCoords : int -> int -> (float * float) *)
let dispCoords row col =
  (float_of_int row *. 50. +. 5., float_of_int col *. 50. +. 5.)

let printMouseCoords x y =
  let row = (int_of_float y) / 50 in
  let col = (int_of_float x) / 50 in
  Lib.pfmt "(%d, %d)" (row) (col)
  ; print_string "\n"

(* textOf : int -> Image.t *)
let textOf numAdjacent =
  match numAdjacent with
  | 1 -> Image.text "1" Color.blue
  | 2 -> Image.text "2" Color.darkGreen
  | 3 -> Image.text "3" Color.red
  | 4 -> Image.text "4" Color.violet
  | 5 -> Image.text "5" Color.maroon
  | 6 -> Image.text "6" Color.turquoise
  | 7 -> Image.text "7" Color.brown
  | 8 -> Image.text "8" Color.black
  | _ -> lightGray

(* view : model -> Image.t *)
let view model =
  let numRows = Array.length model in
  let numCols = Array.length model.(0) in
  let background = Image.empty (float_of_int numRows) (float_of_int numCols) in
  let images = ref [] in
  let imgCoords = ref [] in
  for row = 0 to numRows - 1 do
    for col = 0 to numCols - 1 do
      let coords = dispCoords row col in
      let img =
        match model.(row).(col).state with
        | Hidden -> darkGray
        | Revealed -> textOf model.(row).(col).adjacent
      in
      images := !images @ [img];
      imgCoords := !imgCoords @ [coords]
    done
  done
  ; Image.placeImages !images !imgCoords background

(* validateInput : int -> int -> int -> unit *)
let validateInput mines rows cols =
  if rows < 1 then
    failwith "# of rows must be positive";
  if cols < 1 then
    failwith "# of columns must be positive";
  if mines > rows * cols then
    failwith "Too many mines"

(* handleMouse : model -> int -> int -> string -> model *)
let handleMouse model x y event =
  match event = "button_up" with
  | true -> revealTile model (int_of_float x) (int_of_float y)
  | false -> model

let go inputs =
  Random.self_init () ;
  let mines = int_of_string inputs.(1) in
  let rows = int_of_string inputs.(2) in
  let cols = int_of_string inputs.(3) in
  validateInput mines rows cols;
  let displayWidth = float_of_int cols *. 50. in
  let displayHeight = float_of_int rows *. 50. in
  let initial = initialModel mines rows cols in
  Animate.start initial
    ~name: "Minesweeper"
    ~width: displayWidth
    ~height: displayHeight
    ~view: view
    ~onMouse: handleMouse
    ~viewLast: view

(* > dune exec ./main.exe mines rows cols *)
let _ = go Sys.argv
