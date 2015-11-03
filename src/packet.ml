open Bitstring
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
                  Printf.printf "PMT %d IN PID: %d\n" program_num pmt_pid;
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

let parse packet =
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
