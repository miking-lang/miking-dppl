(** Patterns used in the match construct *)

open Printf

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

let rec string_of_pat = function
  | PatVar(x)    -> x
  | PatUnit      -> "()"
  | PatChar(c)   -> String.make 1 c
  | PatString(s) -> sprintf "\"%s\"" s
  | PatInt(i)    -> string_of_int i
  | PatFloat(f)  -> string_of_float f

  | PatRec(pls) ->
    sprintf "{%s}"
      (String.concat "," (List.map (fun (s,p) ->
           let p = string_of_pat p in
           if s = p then
             s
           else
             sprintf "%s:%s" s p)
           pls))

  | PatList(pls)   -> sprintf "[%s]"
                        (String.concat "," (List.map string_of_pat pls))
  | PatTup(pls)    -> sprintf "(%s)"
                        (String.concat "," (List.map string_of_pat pls))
  | PatCons(p1,p2) -> sprintf "%s:%s"
                        (string_of_pat p1) (string_of_pat p2)

