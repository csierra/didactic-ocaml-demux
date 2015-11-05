open Core.Std

let main =
  let process ch =
      let open Packet in
          let p packet = 
              match (parse packet) with
              | Metadata(PAT(payload)) -> dump_pat payload
              | _ -> print_string "." in
    Packet.iter ~f:p (Packet.reader ch) in
  In_channel.with_file Sys.argv.(1) ~f: process
