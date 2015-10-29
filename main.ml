open Bitstring

let sync_channel_in channel = 
  let rec loop_channel channel = 
    match (input_byte channel) with
     0x47 -> ()
    | _ -> print_string "Skipping\n"; loop_channel channel in
  loop_channel channel

let line_stream_from_channel channel =
  let bytes = String.create 188 in
    bytes.[0] <- Char.chr(0x47);
    Stream.from (
      (fun _ ->
         try 
           sync_channel_in channel;
           really_input channel bytes 1 187;
           Some (bytes)
         with End_of_file -> None));;

let print_pat = Bitstring.hexdump_bitstring stdout

let print_packet = function
  | 0 -> print_pat 
  | _ -> fun _ -> ()

let parse_packet packet =
  let bits = Bitstring.bitstring_of_string packet in 
    bitmatch bits with
    | { 
        _ : 8;
        _ : 1;
        _ : 1;
        _ : 1;
        pid : 13 : int;
        payload      : -1 : bitstring 
      } -> print_packet pid payload
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

