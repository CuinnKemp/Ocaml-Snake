let createGameField () = 
  let field = Array.make_matrix 20 20 Empty in
  field.(10).(10) <- Snake;
  field.(10).(15) <- Food;
  (field)



let spawnFood gameField =
  let rec itter x y =
    match gameField.(y).(x) with
    | Empty -> gameField.(y).(x) <- Food
    | _ -> itter (Random.int 20) (Random.int 20)
  in itter (Random.int 20) (Random.int 20)
  

let main () = 
  let rec mainloop gameField snake = 
    InputOutput.render gameField;
    Snake.update_direction snake;

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