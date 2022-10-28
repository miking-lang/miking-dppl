-- Miking is licensed under the MIT license.
-- Copyright (C) David Broman. See file LICENSE.txt
--
-- File main.mc is the main file of the Miking DPPL project


include "parser.mc"
include "transformation.mc"
include "dppl-arg.mc"
include "extract.mc"
include "build.mc"
include "src-location.mc"
include "coreppl-to-mexpr/compile.mc"

include "option.mc"
include "string.mc"
include "common.mc"
include "mexpr/ast.mc"
include "mexpr/duplicate-code-elimination.mc"
include "mexpr/utils.mc"

let errMsgPrintFunction = "Return type cannot be printed"

lang CPPLLang =
  MExprAst + DPPLExtract + MExprCompile + TransformDist +
  MExprEliminateDuplicateCode + MExprSubstitute

  -- Generates an AST node for the function used to print a sampled value.
  -- TODO(larshum, 2022-10-21): Add support for printing int and bool types.
  -- This is problematic as their pretty-print functions are not built-in, nor
  -- are they guaranteed to be included in the user code.
  sem getTypePrintFunction : RuntimeEntry -> Type -> Expr
  sem getTypePrintFunction runtimeEntry =
  | TyInt _ -> var_ "int2string"
  | TyBool _ -> var_ "bool2string"
  | TyFloat _ -> uconst_ (CFloat2string ())
  -- TODO(dlunde,2022-10-28): For some reason, the versions below using ulam_
  -- do not seem to work.
  | TySeq {ty = TyChar _} -> ulam_ "x" (var_ "x")
  | TyChar _ -> ulam_ "x" (seq_ [(var_ "x")])
  | TyRecord r ->
    if mapIsEmpty r.fields then ulam_ "" (str_ "()")
    else error errMsgPrintFunction
  | _ -> error errMsgPrintFunction

  -- Generates an infer AST node applied on the entire provided AST, using the
  -- provided inference method.
  sem inferMethodApplication : InferMethod -> Expr -> Expr
  sem inferMethodApplication inferMethod =
  | ast ->
    let info = infoTm ast in
    TmInfer {
      method = inferMethod,
      model = ast,
      ty = TyUnknown {info = info}, info = info }

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

      let top =
        parseMCorePPLFileLib (join [corepplSrcLoc, "/coreppl-to-mexpr/top.mc"])
      in

      -- We do not use the -p cppl option to determine the number of iterations
      -- if the number is instead given as the first argument on the command
      -- line when running the program.
      let inferMethod = setRuns (var_ "particles") inferMethod in

      let inferCode = bindall_ [
        ulet_ "d" (inferMethodApplication inferMethod (var_ "ast")),

        ulet_ "" (
            match options.method with
                "mexpr-importance"
              | "mexpr-bpf"
              | "mexpr-apf"
            then
              app_ (var_ "printNormConst") (var_ "d")
            else match options.method with
                "mexpr-mcmc-naive"
              | "mexpr-mcmc-trace"
              | "mexpr-mcmc-lightweight"
            then
              if options.printAcceptanceRate then
                app_ (var_ "printAcceptRate") (var_ "d")
              else
                unit_
            else error "Inference algorithm not supported in global mode"
          ),

        if options.printSamples then
          appf2_ (var_ "printSamples") tyPrintFun (var_ "d")
        else unit_

      ] in

      let ast = bindall_ [
        ulet_ "ast"
          (TmLam {
            ident = nameNoSym "",
            tyIdent = TyRecord {fields = mapEmpty cmpSID, info = infoTm ast},
            body = ast, ty = TyUnknown {info = infoTm ast}, info = infoTm ast
          }),
        ulet_ "particles" (int_ options.particles),
        top,
        appf2_ (var_ "repeat") (ulam_ "" inferCode) (var_ "sweeps")
      ] in

      -- printLn (mexprPPLToString ast);

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
