open Core.Std
open Bitstring

let sync_channel_in channel =
  let rec loop_channel channel =
    match (input_byte channel) with
     0x47 -> ()
    | _ -> print_string "Skipping\n"; loop_channel channel in
  loop_channel channel

let line_stream_from_channel channel =
  let bytes = String.create 188 in
    bytes.[0] <- '\x47';
    Stream.from (
      (fun _ ->
         try
           sync_channel_in channel;
           really_input channel bytes 1 187;
           Some (bytes)
         with End_of_file -> None));;

let print_table table =
  let module Char = Caml.Char in
    bitmatch table with
    | {
        table_id                 :                  8;
        section_syntax_indicator :                  1;
        private_bit              :                  1;
        reserved                 :                  2;
        section_length_unused    :                  2;
        section_length           :                 10;
        tsid                     :                 16;
        3                        :                  2;
        version_number           :                  5;
        current_next             :                  1;
        section_number           :                  8;
        last_section_number      :                  8;
        table_data               : section_length * 8: bitstring;
        crc32                    :                 32
      } ->
          let rec consume_pat pat =
              bitmatch pat with
              | {
                  program_num : 16;
                  7           :  3;
                  pmt_pid     : 13
                } ->
                  print_string ("PMT " ^ string_of_int(program_num) ^ " IN PID: " ^ string_of_int(pmt_pid) ^ "\n");
                  consume_pat (Bitstring.dropbits 32 pat)
              | { 0xFF : 8} ->
                  print_string ("END PAT?\n");
              | { _ } ->
                  print_string ("Match failed: \n");
                  Bitstring.hexdump_bitstring stdout pat;
                  try
                      consume_pat (Bitstring.dropbits 32 pat)
                  with Invalid_argument(argument) -> ()

          in consume_pat table_data

let print_pat pat =
  let module Char = Caml.Char in
    bitmatch pat with
    | {
        pointer_field :8;
        pointer_filler_bytes : pointer_field * 8;
        table : -1 : bitstring
      } when pointer_field > 0 -> print_table table

    | {
        pointer_field : 8;
        table : -1 : bitstring
      } -> print_table table

let print_packet = function
  | 0 -> print_pat
  | _ -> fun _ -> ()

let parse_packet packet =
  let module Char = Caml.Char in
  let bits = Bitstring.bitstring_of_string packet in
    bitmatch bits with
    | {
        (0x47)                                          : 8;
        transport_error_indicator                       : 1;
        payload_unit_start_indicator                    : 1;
        transport_priority                              : 1;
        pid                                             :13;
        scrambling_control                              : 2;
        adaptation_field_exist                          : 1;
        contains_payload                                : 1;
        continuity_counter                              : 4;
        payload                                         :-1:bitstring
      } -> print_packet pid payload
    | { _ } -> failwith "not a packet"

let main =
  let ic = open_in_bin Sys.argv.(1) in
    try
      let stream = line_stream_from_channel ic in
        Stream.iter parse_packet stream;
        Caml.close_in ic;
    with e ->
      Caml.close_in ic;
      raise e
