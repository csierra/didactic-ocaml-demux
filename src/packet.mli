open Core.Std

(** The type representing packets. *)
type t

(** A [reader] is a context from which packets can be read. *)
type reader

(** Given an input channel, return a reader backed on it. *)
val reader : In_channel.t -> reader

(** Read a packet from the given [reader]. *)
val read : reader -> t option

(** Apply [f] over all packets available in the [reader].  On return,
    the reader will be exhausted and subsequent calls to [read] or
    [iter] on that reader will return not results. *)
val iter : reader -> f: (t -> unit) -> unit

(** Parse and dump a packet into stdout *)
val parse : t -> unit
