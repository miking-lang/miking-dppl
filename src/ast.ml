open Utils

(* Attributes of terms.
   Can easily be extended with more data fields as needed. *)
type attr = { label:int; var_label:int; pos:Lexing.position }
let na = { label = -1; var_label = -1; pos = Lexing.dummy_pos }

(* Used for indicating uninitialized debruijn indices *)
let noidx = -1

(* Evaluation environment *)
type env = tm list

(* Term/expression *)
and tm =
  | TmVar         of attr * string * int
  | TmLam         of attr * string * tm
  | TmClos        of attr * string * tm * env
  | TmApp         of attr * tm * tm
  | TmConst       of attr * const
  | TmIfexp       of attr * bool option * tm option
  | TmFix         of attr

  | TmRec         of attr * tm StrMap.t
  | TmProj        of attr * tm * string

  | TmUtest       of attr * tm * tm
  | TmNop         of attr

(* Constant terms/expressions *)
and const =

  (* MCore intrinsic: Boolean constant and operations *)
  | CBool of bool
  | CNot
  | CAnd  of bool option
  | COr   of bool option

  | CChar of char
  (* TODO Add char intrinsics *)

  | CString of string
  (* TODO Add string builtins *)

  (* MCore intrinsic: Integer constant and operations *)
  | CInt  of int
  | CModi of int option
  | CSlli of int option
  | CSrli of int option
  | CSrai of int option

  (* MCore intrinsic: Floating-point number constant and operations *)
  | CFloat of float
  | CLog

  (* Mcore intrinsic: Polymorphic integer and floating-point functions *)
  | CAdd   of const option
  | CSub   of const option
  | CMul   of const option
  | CDiv   of const option
  | CNeg
  | CLt    of const option
  | CLeq   of const option
  | CGt    of const option
  | CGeq   of const option

  (* Polymorphic functions *)
  | CEq    of tm option
  | CNeq   of tm option

  (* Probabilistic constructs and probability distributions*)
  | CInfer
  | CLogPdf   of tm option
  | CSample   of tm list (* Not CPS transformed (cont included in args) *)
  | CWeight   of tm list (* Not CPS transformed (cont included in args) *)
  | CDWeight  of tm list (* Not CPS transformed (cont included in args) *)
  | CNormal   of const list
  | CUniform  of const list
  | CGamma    of const list
  | CExp      of const option
  | CBern     of const option

(* Returns the number of expected arguments *)
let arity c =
  match c with

  (* MCore intrinsic: Boolean constant and operations *)
  | CBool(_)    -> 0
  | CNot        -> 1
  | CAnd(None)  -> 2  | CAnd(Some(_))  -> 1
  | COr(None)   -> 2  | COr(Some(_))   -> 1

  (* MCore intrinsic: Integer constant and operations *)
  | CInt(_)     -> 0
  | CModi(None) -> 2  | CModi(Some(_)) -> 1
  | CSlli(None) -> 2  | CSlli(Some(_)) -> 1
  | CSrli(None) -> 2  | CSrli(Some(_)) -> 1
  | CSrai(None) -> 2  | CSrai(Some(_)) -> 1

  (* MCore intrinsic: Floating-point number constant and operations *)
  | CFloat(_)   -> 0
  | CLog        -> 1

  (* Mcore intrinsic: Polymorphic integer and floating-point numbers *)
  | CAdd(None) -> 2  | CAdd(_)        -> 1
  | CSub(None) -> 2  | CSub(_)        -> 1
  | CMul(None) -> 2  | CMul(_)        -> 1
  | CDiv(None) -> 2  | CDiv(_)        -> 1
  | CNeg       -> 1
  | CLt(None)  -> 2  | CLt(_)         -> 1
  | CLeq(None) -> 2  | CLeq(_)        -> 1
  | CGt(None)  -> 2  | CGt(_)         -> 1
  | CGeq(None) -> 2  | CGeq(_)        -> 1

  (* Ragnar temp functions for handling polymorphic arguments *)
  | CEq(None)  -> 2  | CEq(Some(_))  -> 1
  | CNeq(None) -> 2  | CNeq(Some(_)) -> 1

  (* Probabilistic constructs and probability distributions*)
  | CInfer          -> 1
  | CLogPdf(None)   -> 2 | CLogPdf(Some _) -> 1
  | CSample([])     -> 2 | CSample([_])   -> 1 | CSample([_;_])  -> 0
  | CWeight([])     -> 2 | CWeight([_])   -> 1 | CWeight([_;_])  -> 0
  | CDWeight([])    -> 2 | CDWeight([_])  -> 1 | CDWeight([_;_]) -> 0
  | CNormal([])     -> 2 | CNormal([_])   -> 1 | CNormal([_;_])  -> 0
  | CUniform([])    -> 2 | CUniform([_])  -> 1 | CUniform([_;_]) -> 0
  | CGamma([])      -> 2 | CGamma([_])    -> 1 | CGamma([_;_])   -> 0
  | CExp(None)      -> 1
  | CBern(None)     -> 1

  | _ -> failwith "Arity not defined"

(* Returns the label of a term *)
let tm_label = function
  | TmVar({label;_},_,_)
  | TmLam({label;_},_,_)
  | TmClos({label;_},_,_,_)
  | TmApp({label;_},_,_)
  | TmConst({label;_},_)
  | TmIfexp({label;_},_,_)
  | TmFix({label;_})

  | TmRec({label;_},_)
  | TmProj({label;_},_,_)

  | TmUtest({label;_},_,_)
  | TmNop({label;_}) -> label

(** Function for wrapping a const in a tm *)
let tm_of_const c = TmConst(na, c)

