let set_raw_mode () =
  let termios = Unix.tcgetattr Unix.stdin in
  let new_termios = { termios with
    Unix.c_icanon = false;   (* disable canonical mode *)
    Unix.c_echo   = false;   (* disable echo *)
  } in
  Unix.tcsetattr Unix.stdin Unix.TCSANOW new_termios

let restore_terminal () =
  let termios = Unix.tcgetattr Unix.stdin in
  let new_termios = { termios with
    Unix.c_icanon = true;
    Unix.c_echo   = true;
  } in
  Unix.tcsetattr Unix.stdin Unix.TCSANOW new_termios


let () =
  set_raw_mode ();
  try
    Snake.main ();          (* your existing main *)
    restore_terminal ()
  with e ->
    restore_terminal ();
    raise e
