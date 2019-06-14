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

(** Convert patterns to strings *)
let rec string_of_pat = function
  | PVar(x)    -> x
  | PUnit      -> "()"
  | PChar(c)   -> String.make 1 c
  | PString(s) -> Printf.sprintf "\"%s\"" s
  | PInt(i)    -> string_of_int i
  | PFloat(f)  -> string_of_float f

  | PRec(pls) ->
    Printf.sprintf "{%s}"
      (String.concat "," (List.map (fun (s,p) ->
           let p = string_of_pat p in
           if s = p then
             s
           else
             Printf.sprintf "%s:%s" s p)
           pls))

  | PList(pls)   -> Printf.sprintf "[%s]"
                      (String.concat "," (List.map string_of_pat pls))
  | PTup(pls)    -> Printf.sprintf "(%s)"
                      (String.concat "," (List.map string_of_pat pls))
  | PCons(p1,p2) -> Printf.sprintf "%s:%s"
                      (string_of_pat p1) (string_of_pat p2)

