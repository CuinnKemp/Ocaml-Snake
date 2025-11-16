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