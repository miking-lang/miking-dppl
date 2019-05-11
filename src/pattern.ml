(** Patterns used in the match construct *)

type pat =

  (* Variables *)
  | PVar    of string

  (* Recursive patterns *)
  | PRec    of (string * pat) list
  | PList   of pat list
  | PTup    of pat list
  | PCons   of pat * pat

  (* Constant patterns *)
  | PUnit
  | PChar   of char
  | PString of string
  | PInt    of int
  | PFloat  of float

