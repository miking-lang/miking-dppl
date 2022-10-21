-- Miking is licensed under the MIT license.
-- Copyright (C) David Broman. See file LICENSE.txt
--
-- File main.mc is the main file of the Miking DPPL project


include "parser.mc"
include "transformation.mc"
include "dppl-arg.mc"
include "extract.mc"
include "build.mc"
include "coreppl-to-mexpr/compile.mc"

include "option.mc"
include "string.mc"
include "common.mc"
include "mexpr/ast.mc"
include "mexpr/duplicate-code-elimination.mc"
include "mexpr/utils.mc"

lang CPPLLang =
  MExprAst + DPPLExtract + MExprCompile + TransformDist +
  MExprEliminateDuplicateCode + MExprSubstitute

  -- Generates an AST node for the function used to print a sampled value.
  -- TODO(larshum, 2022-10-21): Add support for printing int and bool types.
  -- This is problematic as their pretty-print functions are not built-in, nor
  -- are they guaranteed to be included in the user code.
  sem getTypePrintFunction : RuntimeEntry -> Type -> Expr
  sem getTypePrintFunction runtimeEntry =
  | TyFloat _ -> uconst_ (CFloat2string ())
  | TySeq {ty = TyChar _} -> ulam_ "x" (var_ "x")
  | _ -> error "Return type cannot be printed"

  -- Generates an infer AST node applied on the entire provided AST, using the
  -- provided inference method.
  sem inferMethodApplication : InferMethod -> Expr -> Expr
  sem inferMethodApplication inferMethod =
  | ast ->
    let info = infoTm ast in
    TmInfer {
      method = inferMethod,
      model = TmLam {
        ident = nameNoSym "",
        tyIdent = TyRecord {fields = mapEmpty cmpSID, info = info},
        body = ast, ty = TyUnknown {info = info}, info = info
      },
      ty = TyUnknown {info = info}, info = info }

  -- Generates code for printing the "result" of an empirical distribution.
  sem printResCode : () -> Expr
  sem printResCode =
  | _ ->
    let idx = ref 0 in
    let i = lam.
      let id = deref idx in
      modref idx (addi id 1);
      infoVal "<printRes code>" id 0 0 0
    in
    let recBody = ulam_ "weights" (ulam_ "samples" (
      match_ (utuple_ [var_ "weights", var_ "samples"])
        (ptuple_ [pseqedge_ [pvar_ "w"] "weights" [], pseqedge_ [pvar_ "s"] "samples" []])
        (bindall_ [
          -- NOTE(larshum, 2022-10-21): We add custom info here to prevent
          -- deadcode elimination from considering the bindings as equivalent.
          withInfo (i ()) (ulet_ "" (print_ (app_ (var_ "printFun") (var_ "s")))),
          withInfo (i ()) (ulet_ "" (print_ (str_ " "))),
          withInfo (i ()) (ulet_ "" (print_ (float2string_ (var_ "w")))),
          withInfo (i ()) (ulet_ "" (print_ (str_ "\n"))),
          appf2_ (var_ "rec") (var_ "weights") (var_ "samples")])
        unit_
    )) in
    ulam_ "printFun" (ulam_ "dist" (
      bind_
        (ureclets_ [("rec", recBody)])
        (match_ (app_ (uconst_ (CDistEmpiricalSamples ())) (var_ "dist"))
          (ptuple_ [pvar_ "samples", pvar_ "weights"])
          (appf2_ (var_ "rec") (var_ "weights") (var_ "samples"))
          never_)
    ))

  -- Applies a transformation on full-program models, that allows them to be
  -- compiled in the same way as programs that use infer.
  sem programModelTransform : Options -> Expr -> (Map InferMethod RuntimeEntry, Expr)
  sem programModelTransform options =
  | ast ->
      let inferMethod = inferMethodFromOptions options options.method in

      match loadCompiler options inferMethod with (runtime, _) in

      let runtimeEntry = loadRuntimeEntry inferMethod runtime in
      let runtimes = mapFromSeq cmpInferMethod [(inferMethod, runtimeEntry)] in

      let resTy = tyTm (typeCheck ast) in
      let tyPrintFun = getTypePrintFunction runtimeEntry resTy in

      let ast = bindall_ [
        ulet_ "d" (inferMethodApplication inferMethod ast),
        ulet_ "printRes" (printResCode ()),
        if options.printSamples then
          appf2_ (var_ "printRes") tyPrintFun (var_ "d")
        else unit_
      ] in

      (runtimes, ast)
end

mexpr

use CPPLLang in

-- Use the arg.mc library to parse arguments
let result = argParse default config in
match result with ParseOK r then
  let options: Options = r.options in
  -- Print menu if not exactly one file argument
  if neqi (length r.strings) 1 then
    print (menu ());
    exit 0
  else
    -- Read and parse the file
    let filename = head r.strings in
    let ast = parseMCorePPLFile filename in

    -- Load the runtimes used in the provided AST, and collect identifiers of
    -- common methods within the runtimes.
    let runtimes = loadRuntimes options ast in

    -- If no runtimes are found, it means there are no uses of 'infer' in the
    -- program. In this case, the entire AST is the model code, so we transform
    -- it as:
    --
    -- let d = infer <method> (lam. <model>) in
    -- let printRes = ... in
    -- printRes <pp> d
    --
    -- where <method> = inference method chosen according to options
    --       <model> = the entire AST
    --       <pp> = the pretty-print function used to print the result
    match
      if mapIsEmpty runtimes then programModelTransform options ast
      else (runtimes, ast)
    with (runtimes, ast) in

    -- Combine the required runtime ASTs to one AST and eliminate duplicate
    -- definitions due to files having common dependencies. The result is an
    -- updated map of runtime entries, a combined runtime AST and a
    -- symbolization environment.
    let runtimeData = combineRuntimes options runtimes in

    -- Compile the CorePPL AST using the provided options and runtime data.
    let ast = mexprCompile options runtimeData ast in

    -- Exit before producing the output files, if the flag is set
    if options.exitBefore then exit 0
    else buildMExpr options ast
else
  -- Error in Argument parsing
  argPrintError result;
  exit 1
