type pieceType = 
| FLAG
| O
| I
| S
| Z
| L
| J
| T

let all_pieces = [O;I;S;Z;L;J;T]

type pieceInfo = {
  piece: pieceType;
  rotation: int;
  position: int;
  row: int 
}

let getPiece pieceInfo =
  let doShift toShift = 
    if (pieceInfo.position >= 0) then
      toShift lsr pieceInfo.position
    else toShift lsl (abs pieceInfo.position)
  in
  let base = Tools.safe_init (pieceInfo.row) (fun _ -> 0) @
   match pieceInfo.piece with
  | FLAG -> failwith "cannot make piece of Type FLAG"
  | O -> (let out = doShift 0x180
         in 
         [out; 
          out])

  | S -> (
      match pieceInfo.rotation with
      | 0 | 2 -> [(doShift 0xC0); 
                  (doShift 0x180)]
      | 1 | _ -> [(doShift 0x100); 
                  (doShift 0x180);
                  (doShift 0x80)]
    )
  | Z -> (
      match pieceInfo.rotation with
      | 0 | 2 -> [(doShift 0x180);
                  (doShift 0xC0)]
      | 1 | _ -> [(doShift 0x80); 
                  (doShift 0x180);
                  (doShift 0x100)]
    )
  | L -> (
      match pieceInfo.rotation with
      | 0  -> [(doShift 0x80);
                  (doShift 0x80);
                  (doShift 0xC0)]
      | 1  -> [(doShift 0x0); 
                  (doShift 0x1C0);
                  (doShift 0x100)]
      | 2 -> [(doShift 0x180); 
                  (doShift 0x80);
                  (doShift 0x80)]
      | _ -> [(doShift 0x40); 
                  (doShift 0x1C0);
                  (doShift 0x000)]
    )
  
  | J -> (
      match pieceInfo.rotation with
      | 2  -> [(doShift 0xC0);
                  (doShift 0x80);
                  (doShift 0x80)]
      | 0 -> [(doShift 0x80); 
                  (doShift 0x80);
                  (doShift 0x180)]
      | 1  -> [(doShift 0x100); 
                  (doShift 0x1C0);
                  (doShift 0x0)]
      | _ -> [(doShift 0x0); 
                  (doShift 0x1C0);
                  (doShift 0x40)]
    )

  | T -> (
      match pieceInfo.rotation with
      | 0  -> [(doShift 0x80);
                  (doShift 0x1C0)]
      | 1 -> [(doShift 0x80); 
                  (doShift 0xC0);
                  (doShift 0x80)]
      | 2  -> [(doShift 0x1C0); 
                  (doShift 0x80)]
      | _ -> [(doShift 0x80); 
                  (doShift 0x180);
                  (doShift 0x80)]
    )

  | I -> (
      match pieceInfo.rotation with
      | 0 | 2 ->  let out = doShift 0x80
          in 
           [out;   (* 0000 00 00 10 00 0000*)
            out;   (* 0000 00 00 10 00 0000*)
            out;   (* 0000 00 00 10 00 0000*)
            out]   (* 0000 00 00 10 00 0000*)

      | 1 | _ ->  let out = doShift 0x3C0
        in
           [0x0;
            out;] (* 0000 0011 1100 0000 *)
          )
    in base

let getInitialState () =
  Tools.safe_init 20 (fun _ -> 0xE007) @ [0xFFFF]

let getRandomPiece () =
  let pieceType = List.nth all_pieces (Random.int (List.length all_pieces))
in let rotation = Random.int (4)
in {piece=pieceType; rotation=rotation; position=0; row=0}

let render gameField currentPiece score =
  print_string "\027[2J";
  print_string "\027[H";
  print_newline ();
  print_newline ();
  Printf.printf "----- %10d-----\n" score;
  let rec iter1 gameList pieceList =
    match gameList, pieceList with
    | g :: gs , [] ->
        let rec printBinary i =
          if i < 10 then (
            let mask = 1 lsl (12 - i) in
            if (g land mask <> 0)
            then print_string "██"
            else print_string "  ";
            printBinary (i + 1)
          )
        in
        print_string "|";
        printBinary 0;
        print_string "|";
        print_newline ();
        iter1 gs []
    | g :: gs, p :: ps ->
        let rec printBinary i =
          if i < 10 then (
            let mask = 1 lsl (12 - i) in
            if (g land mask <> 0 || p land mask <> 0)
            then print_string "██"
            else print_string "  ";
            printBinary (i + 1)
          )
        in
        print_string "|";
        printBinary 0;
        print_string "|";
        print_newline ();
        iter1 gs ps
    | [], [] -> ()
    | [], _  -> failwith "Lists must have same length"
  in
  iter1 gameField currentPiece;
  flush Stdlib.stdout
  


let updatePostition pieceInfo =
  let piece, newRotation, newPosition, newRow = 
    match Tools.read_char_nonblock () with
    | Some 'w' -> (pieceInfo.piece, (pieceInfo.rotation + 1) mod 4, pieceInfo.position, pieceInfo.row)   
    | Some 'a' -> (pieceInfo.piece, pieceInfo.rotation, pieceInfo.position - 1, pieceInfo.row)   
    | Some 'd' -> (pieceInfo.piece, pieceInfo.rotation, pieceInfo.position + 1, pieceInfo.row)   
    | Some 's' -> (FLAG, pieceInfo.rotation, pieceInfo.position, pieceInfo.row+1)   
    | _ -> (pieceInfo.piece, pieceInfo.rotation, pieceInfo.position, pieceInfo.row)   
  in {piece=piece; rotation=newRotation; position=newPosition; row=newRow}

let lowerPiece pieceInfo = 
  {piece=pieceInfo.piece; rotation=pieceInfo.rotation; position=pieceInfo.position; row =(pieceInfo.row + 1)}

type actionResult = 
| InvalidMove (* should switch back to curPiece discarding tempPiece *)
| SuccessfulMove (* action was successful continue *)

let rec checkIfValidAction gameField currentPiece =
  match gameField, currentPiece with
  | _, [] -> SuccessfulMove
  | [], _ -> InvalidMove
  | g :: gs, p :: ps ->
      if (g land p = 0) then checkIfValidAction gs ps
      else InvalidMove
  
let setAndCheckPiece gameField currentPiece = 
  let rec setPiece gameField currentPiece i =
    match gameField, currentPiece, i with
    | _, _, 20 -> [0xFFFF]
    | g :: gs, [], _ -> [g] @ setPiece gs [] (i + 1)
    | [], [], _ -> []
    | g :: gs, p :: ps, _ ->
          let newValue = g lor p
        in 
          if (newValue = 0xFFFF) then [] @ setPiece gs ps (i + 1) else [newValue] @ setPiece gs ps (i + 1)
    | [], _, _ -> failwith "Lists must have same length"
  in
    let tempField = setPiece gameField currentPiece 0
  in 
    let difference = 21 - (List.length tempField)
  in let finalField = Tools.safe_init difference (fun _ -> 0xE007) @ tempField
  in (finalField, difference)

let actionloop gameField currentPieceInfo i score =
  render gameField (getPiece currentPieceInfo) score;
  let rec actionloopiterator currentPieceInfo i = 
    let nextPieceInfo, next_i = 
      let tempPieceInfo = updatePostition currentPieceInfo
    in 
    if (tempPieceInfo <> currentPieceInfo && tempPieceInfo.piece <> FLAG) then
      let pieceField = 
        getPiece tempPieceInfo
      in
      match checkIfValidAction gameField pieceField with 
      | SuccessfulMove -> 
        render gameField pieceField score;
        tempPieceInfo, i
      | InvalidMove -> currentPieceInfo, i
    else currentPieceInfo, if (tempPieceInfo.piece = FLAG) then 0 else i
    in
    Unix.sleepf 0.01;
    if (next_i != 0) then actionloopiterator nextPieceInfo (next_i - 1)
    else nextPieceInfo
  in let out = actionloopiterator currentPieceInfo i
in (out)


let calculateScore clears =
  match clears with
  | 1 -> 100
  | 2 -> 300
  | 3 -> 500
  | 4 -> 800
  |_ -> 0


let main () = 
 let rec mainloop gameField currentPiece score = 
    let afterActionInfo = actionloop gameField currentPiece 100 score
    in let movedDownInfo = lowerPiece afterActionInfo
  in
    let pieceField = getPiece movedDownInfo
  in
    let nextGameField, nextPieceInfo, n_score = 
      match checkIfValidAction gameField pieceField with 
      | SuccessfulMove -> 
        (gameField, movedDownInfo, score)
      | InvalidMove -> 
          let field, add2Score = setAndCheckPiece gameField (getPiece afterActionInfo)
        in 
          let new_score = score + calculateScore (max 0 add2Score)
        in
          let randomPiece = getRandomPiece ()
        in 
          match checkIfValidAction gameField (getPiece randomPiece) with
          | InvalidMove -> (gameField, {piece=FLAG; rotation=0; position=0; row=0}, new_score)
          | _ -> (field, randomPiece, new_score);
            
  in
    match nextPieceInfo.piece with
    | FLAG -> print_string ("GAME ENDED - Score: " ^ (string_of_int score) ^ "\n")
    | _ -> mainloop nextGameField nextPieceInfo n_score
  in
  let initialField = getInitialState () in
  let initialPiece = getRandomPiece () in 
  mainloop initialField initialPiece 0
  