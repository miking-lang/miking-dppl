
include "mexpr/boot-parser.mc"
include "mexpr/symbolize.mc"
include "mexpr/type-annot.mc"
include "mexpr/remove-ascription.mc"
include "mexpr/utesttrans.mc"
include "mexpr/tuning/decision-points.mc"
include "mexpr/tuning/tune.mc"
include "ocaml/ast.mc"
include "ocaml/generate.mc"
include "ocaml/pprint.mc"
include "ocaml/external-includes.mc"
include "ocaml/sys.mc"

lang MCoreCompile =
  BootParser +
  MExprHoles +
  MExprSym + MExprTypeAnnot + MExprUtestTrans +
  OCamlPrettyPrint + OCamlTypeDeclGenerate + OCamlGenerate +
  OCamlGenerateExternalNaive
end

let collectLibraries : ExternalNameMap -> ([String], [String])
= lam extNameMap.
  let f = lam s. lam str. setInsert str s in
  let g = lam acc : (Set String, Set String). lam impl :  ExternalImpl.
    match acc with (libs, clibs) then
      (foldl f libs impl.libraries, foldl f clibs impl.cLibraries)
    else never
  in
  let h = lam acc. lam. lam impls. foldl g acc impls in
  match mapFoldWithKey h (setEmpty cmpString, setEmpty cmpString) extNameMap
  with (libs, clibs) then (setToSeq libs, setToSeq clibs)
  else never

let compileRunMCore = lam ast.
  use MCoreCompile in
    -- Re-symbolize the MExpr AST and re-annotate it with types
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
        match collectLibraries env.exts with (libs, clibs) then

          -- Pretty print OCaml program
          let code = use OCamlPrettyPrint in expr2str ast
          in

          -- Compile and run the OCaml program
          let options =
            {optimize = true, libraries = libs, cLibraries = clibs}
          in
          let cunit = ocamlCompileWithConfig options code in
          let res = cunit.run "" [] in
          cunit.cleanup ();
          print (join [res.stdout, "\n"])
      else never
    else never
  else never

