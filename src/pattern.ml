(** Patterns used in the match construct *)

type pat =

  (* Variables *)
  | PatVar    of string

  (* Recursive patterns *)
  | PatRec    of (string * pat) list
  | PatList   of pat list
  | PatTup    of pat list
  | PatCons   of pat * pat

  (* Constant patterns *)
  | PatUnit
  | PatChar   of char
  | PatString of string
  | PatInt    of int
  | PatFloat  of float
