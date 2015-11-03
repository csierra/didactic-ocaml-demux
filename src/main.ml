open Core.Std

let main =
  let process ch =
    Packet.iter ~f: Packet.parse (Packet.reader ch) in
  In_channel.with_file Sys.argv.(1) ~f: process
