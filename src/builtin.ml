(** Defines built-in variables. *)

open Ast
open Printf
open Sprint

(** Mapping between predefined variable names and builtin values *)
let builtin = [

  "not",          VNot;
  "and",          VAnd{b1=None};
  "or",           VOr{b1=None};

  "mod",          VMod{i1=None};
  "sll",          VSll{i1=None};
  "srl",          VSrl{i1=None};
  "sra",          VSra{i1=None};

  "inf",          VFloat{f=infinity};
  "log",          VLog;

  "add",          VAdd{v1=None};
  "sub",          VSub{v1=None};
  "mul",          VMul{v1=None};
  "div",          VDiv{v1=None};
  "neg",          VNeg;
  "lt",           VLt{v1=None};
  "leq",          VLeq{v1=None};
  "gt",           VGt{v1=None};
  "geq",          VGeq{v1=None};

  "eq",           VEq{v1=None};
  "neq",          VNeq{v1=None};

  "normal",       VDist{d=DNormal{mu=None;sigma=None}};
  "uniform",      VDist{d=DUniform{a=None;b=None}};
  "gamma",        VDist{d=DGamma{a=None;b=None}};
  "exponential",  VDist{d=DExp{lam=None}};
  "bernoulli",    VDist{d=DBern{p=None}};
  "beta",         VDist{d=DBeta{a=None;b=None}};

  "logpdf",       VLogPdf{v1=None};
  "sample",       VSample{cont=None;d=None};

  "weight",       VWeight{cont=None;w=None};

  "resample",     VResamp{dyn=false;cont=None;stoch_ctrl=None };

  "fix",          VFix;

] |> List.map (fun (x, y) -> x, tm_of_val' y)

(** Create a string representation of builtins *)
let string_of_builtin ?(labels = false) builtin =
  String.concat "\n"
    (List.map (fun (x, y) -> sprintf "%s = %s" x
                  (string_of_tm ~labels:labels ~pretty:false y)) builtin)

