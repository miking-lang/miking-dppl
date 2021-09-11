

let compileMCore = lam sourcePath. lam mexprAst.
  use MCoreCompile in
    -- Re-symbolize the MExpr AST and re-annotate it with types
    let ast = symbolizeExpr symEnv ast in
    let ast = typeAnnot ast in
    let ast = removeTypeAscription ast in

    -- Translate the MExpr AST into an OCaml AST
    match typeLift ast with (env, ast) then
      match generateTypeDecl env ast with (env, ast) then
        let env : GenerateEnv =
          chooseExternalImpls globalExternalImplsMap env ast
        in
        let ast = generate env ast in

        -- Collect external library dependencies
        let libs = collectLibraries env.exts in

        -- Pretty print OCaml program
        let ocamlProg = pprintOcaml ast in

        -- Compile OCaml AST
        ocamlCompile options libs sourcePath ocamlProg

    else never
  else never
