(** General utilities used throughout pplcore. *)

open Lexing
open Printf

(** Convert lexing positions to strings *)
let string_of_position pos =
  sprintf "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

(** String map *)
module StrMap = Map.Make(struct type t = string let compare = compare end)

(** Generates a list with i values v as elements *)
let replicate i v =
  let rec recurse i acc = match i with
    | 0 -> acc
    | i when i > 0 -> recurse (i-1) (v :: acc)
    | _ -> failwith "Negative number in replicate"
  in recurse i []

(** Tail recursive map *)
let map f l =
  let rec recurse acc = function
    | [] -> List.rev acc
    | x::xs -> recurse (f x::acc) xs
  in
  recurse [] l

(** Append n elements of x to the list ls *)
let rec append n x ls =
  if n < 1 then
    ls
  else append (n-1) x (x :: ls)

(** Map accumulate for lists. Maps f over the array while also carrying an
    accumulator between applications of f. *)
let map_accum f acc ls =
  let rec recurse acc ls = match ls with
    | x::ls ->
      let acc,x = f acc x in
      let acc,ls = recurse acc ls in
      acc,x::ls
    | [] -> acc,[]
  in recurse acc ls

(** Map accumulate for arrays. Maps f over the array while also carrying an
    accumulator between applications of f. *)
let arr_map_accum f acc arr =
  let len = Array.length arr in
  let acc,elem = f acc arr.(0) in
  let res = (Array.make len elem) in
  let rec recurse acc i =
    if i < len then
      let acc,elem = f acc arr.(i) in
      res.(i) <- elem;
      recurse acc (i+1)
    else
      acc,res
  in recurse acc 1

