(** Constants and constant operations (intrinsics) used as part of pplcore *)

open Printf

(** Constant terms/expressions *)
type const =

  (* Unit constant *)
  | CUnit

  (* Boolean constant and operations *)
  | CBool    of bool
  | CNot
  | CAnd     of bool option
  | COr      of bool option

  (* Character constant and operations *)
  | CChar    of char

  (* String constant and operations *)
  | CString  of string

  (* Integer constant and operations *)
  | CInt     of int
  | CMod     of int option
  | CSll     of int option
  | CSrl     of int option
  | CSra     of int option

  (* Floating-point number constant and operations *)
  | CFloat   of float
  | CLog

  (* Polymorphic integer/floating-point functions *)
  | CAdd     of const option
  | CSub     of const option
  | CMul     of const option
  | CDiv     of const option
  | CNeg
  | CLt      of const option
  | CLeq     of const option
  | CGt      of const option
  | CGeq     of const option

  (* Polymorphic functions *)
  | CEq      of const option
  | CNeq     of const option

  (* Probability distributions *)
  | CNormal  of float option * float option
  | CUniform of float option * float option
  | CGamma   of float option * float option
  | CExp     of float option
  | CBern    of float option

(** Returns the number of expected arguments for constants *)
let arity c = match c with
  | CUnit         -> 0

  | CBool _       -> 0
  | CNot          -> 1
  | CAnd(None)    -> 2  | CAnd(Some _) -> 1
  | COr(None)     -> 2  | COr(Some _)  -> 1

  | CChar _       -> 0

  | CString _     -> 0

  | CInt _        -> 0
  | CMod(None)    -> 2  | CMod(Some _) -> 1
  | CSll(None)    -> 2  | CSll(Some _) -> 1
  | CSrl(None)    -> 2  | CSrl(Some _) -> 1
  | CSra(None)    -> 2  | CSra(Some _) -> 1

  | CFloat _      -> 0
  | CLog          -> 1

  | CAdd(None)    -> 2  | CAdd _        -> 1
  | CSub(None)    -> 2  | CSub _        -> 1
  | CMul(None)    -> 2  | CMul _        -> 1
  | CDiv(None)    -> 2  | CDiv _        -> 1
  | CNeg          -> 1
  | CLt(None)     -> 2  | CLt _         -> 1
  | CLeq(None)    -> 2  | CLeq _        -> 1
  | CGt(None)     -> 2  | CGt _         -> 1
  | CGeq(None)    -> 2  | CGeq _        -> 1

  | CEq(None)     -> 2  | CEq(Some _)   -> 1
  | CNeq(None)    -> 2  | CNeq(Some _)  -> 1

  | CNormal(None,None)       -> 2
  | CNormal(Some _,None)     -> 1
  | CNormal(Some _,Some _)   -> 0
  | CNormal _                -> failwith "Should not happen"

  | CUniform(None,None)      -> 2
  | CUniform(Some _, None)   -> 1
  | CUniform(Some _, Some _) -> 0
  | CUniform _               -> failwith "Should not happen"

  | CGamma(None,None)        -> 2
  | CGamma(Some _, None)     -> 1
  | CGamma(Some _, Some _)   -> 0
  | CGamma _                 -> failwith "Should not happen"

  | CExp(None)  -> 1 | CExp _  -> 1

  | CBern(None) -> 1 | CBern _ -> 1

(* Convert constants to pretty printed strings *)
let rec string_of_const c = match c with
  | CUnit -> "unit"

  | CBool(b) -> string_of_bool b
  | CNot -> "not"
  | CAnd(None) -> "and"
  | CAnd(Some(v)) -> "and(" ^ string_of_bool v ^ ")"
  | COr(None) -> "or"
  | COr(Some(v)) -> "or(" ^ string_of_bool v ^ ")"

  | CChar(c) -> String.make 1 c

  | CString(s) -> "\"" ^ s ^ "\""

  | CInt(v) -> sprintf "%d" v
  | CMod(None) -> "mod"
  | CMod(Some(v)) -> sprintf "mod(%d)" v
  | CSll(None) -> "sll"
  | CSll(Some(v)) -> sprintf "sll(%d)" v
  | CSrl(None) -> "srl"
  | CSrl(Some(v)) -> sprintf "srl(%d)" v
  | CSra(None) -> "sra"
  | CSra(Some(v)) -> sprintf "sra(%d)" v

  | CFloat(v) -> sprintf "%f" v
  | CLog -> "log"

  | CAdd(Some(CInt(v))) -> sprintf "add(%d)" v
  | CAdd(Some(CFloat(v))) -> sprintf "add(%f)" v
  | CAdd(None) -> "add"
  | CAdd _ -> failwith "Not supported"
  | CSub(Some(CInt(v))) -> sprintf "sub(%d)" v
  | CSub(Some(CFloat(v))) -> sprintf "sub(%f)" v
  | CSub(None) -> "sub"
  | CSub _ -> failwith "Not supported"
  | CMul(Some(CInt(v))) -> sprintf "mul(%d)" v
  | CMul(Some(CFloat(v))) -> sprintf "mul(%f)" v
  | CMul(None) -> "mul"
  | CMul _ -> failwith "Not supported"
  | CDiv(Some(CInt(v))) -> sprintf "div(%d)" v
  | CDiv(Some(CFloat(v))) -> sprintf "div(%f)" v
  | CDiv(None) -> "div"
  | CDiv _ -> failwith "Not supported"
  | CNeg -> "neg"
  | CLt(Some(CInt(v))) -> sprintf "lt(%d)" v
  | CLt(Some(CFloat(v))) -> sprintf "lt(%f)" v
  | CLt(None) -> "lt"
  | CLt _ -> failwith "Not supported"
  | CLeq(Some(CInt(v))) -> sprintf "leq(%d)" v
  | CLeq(Some(CFloat(v))) -> sprintf "leq(%f)" v
  | CLeq(None) -> "leq"
  | CLeq _ -> failwith "Not supported"
  | CGt(Some(CInt(v))) -> sprintf "gt(%d)" v
  | CGt(Some(CFloat(v))) -> sprintf "gt(%f)" v
  | CGt(None) -> "gt"
  | CGt _ -> failwith "Not supported"
  | CGeq(Some(CInt(v))) -> sprintf "geq(%d)" v
  | CGeq(Some(CFloat(v))) -> sprintf "geq(%f)" v
  | CGeq(None) -> "geq"
  | CGeq _ -> failwith "Not supported"

  | CEq(None) -> "eq"
  | CEq(Some(v)) -> "eq(" ^ (string_of_const v) ^ ")"
  | CNeq(None) -> "neq"
  | CNeq(Some(v)) -> "neq(" ^ (string_of_const v) ^ ")"

  | CNormal(None,None)       -> "normal"
  | CNormal(Some f1,None)    -> sprintf "normal(%f)" f1
  | CNormal(Some f1,Some f2) -> sprintf "normal(%f,%f)" f1 f2
  | CNormal _                -> failwith "Not supported"

  | CUniform(None,None)       -> "uniform"
  | CUniform(Some f1,None)    -> sprintf "uniform(%f)" f1
  | CUniform(Some f1,Some f2) -> sprintf "uniform(%f,%f)" f1 f2
  | CUniform _                -> failwith "Not supported"

  | CGamma(None,None)       -> "gamma"
  | CGamma(Some f1,None)    -> sprintf "gamma(%f)" f1
  | CGamma(Some f1,Some f2) -> sprintf "gamma(%f,%f)" f1 f2
  | CGamma _                -> failwith "Not supported"

  | CExp(None)   -> "exp"
  | CExp(Some f) -> sprintf "exp(%f)" f

  | CBern(None)   -> "bern"
  | CBern(Some f) -> sprintf "bern(%f)" f
