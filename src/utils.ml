(** General utilities used throughout pplcore. *)

open Printf
open Lexing

(** Convert lexing positions to strings *)
let string_of_position pos =
  sprintf "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

(** String map *)
module StrMap = Map.Make(struct type t = string let compare = compare end)

(** Generates a list with i values v as elements *)
let replicate i v =
  let rec recurse i v acc = match i with
    | 0 -> acc
    | _ -> recurse (i-1) v (v :: acc)
  in recurse i v []


