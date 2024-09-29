include "runtimes.mc"

let errMsgPrintFunction = "Return type cannot be printed"

let _seqToStrTm = use BootParser in
    parseMExprStringExn (_defaultBootParserParseMExprStringArg ())
      "
lam f . lam xs.
  let join = foldl concat \"\" in
  switch xs
  case [] then \"[]\"
  case [x] ++ xs then join [\"[\", f x, join (map (cons ',') (map f xs)), \"]\"]
  end
      "

lang CPPLBackcompat = LoadRuntime
  -- Generates an AST node for the function used to print a sampled value.
  -- TODO(larshum, 2022-10-21): Add support for printing int and bool types.
  -- This is problematic as their pretty-print functions are not built-in, nor
  -- are they guaranteed to be included in the user code.
  sem getTypePrintFunction : InferRuntimeEntry -> Type -> Expr
  sem getTypePrintFunction runtimeEntry =
  | TyInt _ -> var_ "int2string"
  | TyBool _ -> var_ "bool2string"
  | TyFloat _ -> uconst_ (CFloat2string ())
  -- TODO(dlunde,2022-10-28): For some reason, the versions below using ulam_
  -- do not seem to work.
  | TySeq {ty = TyChar _} -> ulam_ "x" (var_ "x")
  | TySeq r -> app_ _seqToStrTm (getTypePrintFunction runtimeEntry r.ty)
  | TyChar _ -> ulam_ "x" (seq_ [(var_ "x")])
  | TyRecord r ->
    if mapIsEmpty r.fields then ulam_ "" (str_ "()")
    else
      match record2tuple r.fields with Some tys then
        ulam_ "x"
          (foldl_ (ulams_ ["x", "y"] (concat_ (var_ "x") (var_ "y")))
             (str_ "(")
             (seq_ (join [
               mapi
                 (lam i. lam ty.
                   let x =
                     appf1_ (getTypePrintFunction runtimeEntry ty)
                       (tupleproj_ i (var_ "x"))
                   in
                   if eqi i 0 then x
                   else cons_ (char_ ',') x)
                 tys,
               [str_ ")"]
             ])))
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
  sem programModelTransform : Options -> Expr -> (Map InferMethod InferRuntimeEntry, Expr)
  sem programModelTransform options =
  | ast ->
      let inferMethod = inferMethodFromOptions options options.method in

      match loadCompiler options inferMethod with (runtime, _) in

      let runtimeEntry = loadRuntimeEntry inferMethod runtime in
      let runtimes = mapFromSeq cmpInferMethod [(inferMethod, runtimeEntry)] in

      let resTy = tyTm (typeCheck ast) in
      let tyPrintFun = getTypePrintFunction runtimeEntry resTy in

      let top =
        parseMCorePPLFileLib options.test
          (join [corepplSrcLoc, "/coreppl-to-mexpr/top.mc"])
      in

      -- We do not use the -p cppl option to determine the number of iterations
      -- if the number is instead given as the first argument on the command
      -- line when running the program.
      let inferMethod = setRuns (var_ "particles") inferMethod in

      let inferCode = bindall_ [
        ulet_ "d" (inferMethodApplication inferMethod (var_ "ast")),

        ulet_ "" (
            match options.method with
                "is-lw"
              | "smc-bpf"
              | "smc-apf"
            then
              app_ (var_ "printNormConst") (var_ "d")
            else match options.method with
                "mcmc-naive"
              | "mcmc-trace"
              | "mcmc-lightweight"
              | "pmcmc-pimh"
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
            tyAnnot = TyRecord {fields = mapEmpty cmpSID, info = infoTm ast},
            tyParam = TyRecord {fields = mapEmpty cmpSID, info = infoTm ast},
            body = ast, ty = TyUnknown {info = infoTm ast}, info = infoTm ast
          }),
        ulet_ "particles" (int_ options.particles),
        top,
        appf2_ (var_ "repeat") (ulam_ "" inferCode) (var_ "sweeps")
      ] in

      -- printLn (mexprPPLToString ast);

      (runtimes, ast)
end
