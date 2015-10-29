open Bitstring

let line_stream_from_channel channel =
  let bytes = String.create 188 in
    Stream.from (
      (fun _ ->
         try 
          really_input channel bytes 0 188;
          Some (bytes)
         with End_of_file -> None));;

let parse_packet packet =
  let bits = Bitstring.bitstring_of_string packet in 
    bitmatch bits with
    | { synchro   : 8 ;
        rest      : -1 : bitstring } -> print_string "packet\n"
    | { _ } -> failwith "not a packet"

let main =  
  let ic = open_in_bin "football.ts" in
    try
      let stream = line_stream_from_channel ic in
        Stream.iter parse_packet stream;
        close_in ic;
    with e ->
      close_in ic;
      raise e

