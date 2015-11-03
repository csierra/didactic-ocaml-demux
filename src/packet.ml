open Core.Std

type t = String.t

type reader = In_channel.t * String.t

let reader ch = (ch, String.create 188)

let rewind ch n =
  let open Int64 in
  In_channel.seek ch (In_channel.pos ch - n)

let sync (ch, buffer) =
  let rec loop () =
    match In_channel.input_byte ch with
    | Some 0x47 -> rewind ch (Int64.of_int 1)
    | _ -> print_string "Skipping\n"; loop () in
  loop ()

let read ((ch, buffer) as reader) =
  sync reader;
  match In_channel.really_input ch buffer 0 188 with
  | Some () -> Some buffer
  | None -> None

let iter reader ~f =
  let rec loop () =
    match read reader with
    | Some x -> f x ; loop ()
    | None -> () in
  loop ()

let to_string p = p
