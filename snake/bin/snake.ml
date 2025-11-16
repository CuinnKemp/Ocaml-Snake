type stateIndicator =
  | Snake
  | Food
  | Empty
  | End

type direction = 
  | Up
  | Down
  | Left
  | Right

type snake = {
  body: (int * int) Queue.t;
  mutable head: int * int;
  mutable dir : direction;
}

let kbhit () =
  let read_fds, _, _ = Unix.select [Unix.stdin] [] [] 0.0 in
  read_fds <> []

let read_char_nonblock () =
  if kbhit () then
    let buf = Bytes.create 1 in
    let _ = Unix.read Unix.stdin buf 0 1 in
    Some (Bytes.get buf 0)
  else
    None

let update_direction snake =
  match read_char_nonblock () with
  | Some 'w' -> snake.dir <- if (snake.dir != Down) then Up else Down
  | Some 's' -> snake.dir <- if (snake.dir != Up) then Down else Up
  | Some 'a' -> snake.dir <- if (snake.dir != Right) then Left else Right
  | Some 'd' -> snake.dir <- if (snake.dir != Left) then Right else Left
  | _ -> ()

let createSnake () = 
  let queue = Queue.create () in
  Queue.add (10, 10) queue;
  {body = queue; head = (10,10); dir = Right}


let createGameField () = 
  let field = Array.make_matrix 20 20 Empty in
  field.(10).(10) <- Snake;
  field.(10).(15) <- Food;
  (field)

let render gameField =
  print_string "\027[2J";   (* clear screen *)
  print_string "\027[H";    (* move cursor to top-left *)
  print_newline ();
  print_newline ();
  for y = 0 to 19 do
    for x = 0 to 19 do
      match gameField.(y).(x) with
      | Snake -> print_string "S "
      | Food  -> print_string "F "
      | Empty -> print_string ". "
      | End   -> print_string "X "
    done;
    print_newline ();
  done;
  flush Stdlib.stdout


let spawnFood gameField =
  let rec itter x y =
    match gameField.(y).(x) with
    | Empty -> gameField.(y).(x) <- Food
    | _ -> itter (Random.int 20) (Random.int 20)
  in itter (Random.int 20) (Random.int 20)

let updateSnake snake gameField =
  let newHX, newHY = 
  let headX, headY = snake.head in 
    match snake.dir with
    | Up -> headX, headY - 1
    | Down -> headX, headY + 1
    | Left -> headX - 1, headY
    | Right -> headX + 1, headY
  in 
  
  let shouldFail = 
    if (newHX < 0 || newHX >= 20) || (newHY < 0 || newHY >= 20) then 
      true
    else
      match gameField.(newHY).(newHX) with
      | Snake -> true
      | Food -> spawnFood gameField;
                false
      | _ -> let tx, ty = Queue.take snake.body in  
            gameField.(ty).(tx) <- Empty;
            false
  in

  if (shouldFail) then 
    gameField.(0).(0) <- End
  else begin 
    Queue.add (newHX, newHY) snake.body;
    snake.head <- (newHX, newHY);
    gameField.(newHY).(newHX) <- Snake;
  end;
  (gameField, snake)


let main () = 
  let rec mainloop gameField snake = 
    render gameField;
    update_direction snake;

    let gameField, snake = updateSnake snake gameField in

    if gameField.(0).(0) = End then
      print_endline "Game Over!"
    else begin
      Unix.sleepf 0.1;
      mainloop gameField snake
    end
  in 

  let initialField = createGameField () in
  let initialSnake = createSnake () in 
  let _ = mainloop initialField initialSnake in 
  ()
  