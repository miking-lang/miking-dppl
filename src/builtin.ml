(** Defines built-in variables. *)

open Ast
open Printf
open Sprint

(** Mapping between predefined variable names and builtin constants *)
let builtin = [

  "not",          VNot{at=va};
  "and",          VAnd{at=va;b1=None};
  "or",           VOr{at=va;b1=None};

  "mod",          VMod{at=va;i1=None};
  "sll",          VSll{at=va;i1=None};
  "srl",          VSrl{at=va;i1=None};
  "sra",          VSra{at=va;i1=None};

  "inf",          VFloat{at=va;f=infinity};
  "log",          VLog{at=va};

  "add",          VAdd{at=va;v1=None};
  "sub",          VSub{at=va;v1=None};
  "mul",          VMul{at=va;v1=None};
  "div",          VDiv{at=va;v1=None};
  "neg",          VNeg{at=va};
  "lt",           VLt{at=va;v1=None};
  "leq",          VLeq{at=va;v1=None};
  "gt",           VGt{at=va;v1=None};
  "geq",          VGeq{at=va;v1=None};

  "eq",           VEq{at=va;v1=None};
  "neq",          VNeq{at=va;v1=None};

  "normal",       VNormal{at=va;mu=None;sigma=None};
  "uniform",      VUniform{at=va;a=None;b=None};
  "gamma",        VGamma{at=va;a=None;b=None};
  "exponential",  VExp{at=va;lam=None};
  "bernoulli",    VBern{at=va;p=None};
  "beta",         VBeta{at=va;a=None;b=None};

  "logpdf",       VLogPdf{at=va;v1=None};
  "sample",       VSample{at=va};
  "resample",     VResamp{at=va;dyn=false;cont=None;stoch_ctrl=None };

  "weight",       VWeight{at=va};

  "fix",          VFix{at=va};

] |> List.map (fun (x, y) -> x, tm_of_val y)

(** Create a string representation of builtins *)
let string_of_builtin ?(labels = false) builtin =
  String.concat "\n"
    (List.map (fun (x, y) -> sprintf "%s = %s" x
                  (string_of_tm ~labels:labels ~pretty:false y)) builtin)

