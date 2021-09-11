
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

let collectLibraries = lam extNameMap : ExternalNameMap.
  let f = lam libs. lam lib. setInsert lib libs in
  let g =
    lam libs. lam impl :  ExternalImpl. foldl f libs impl.libraries
  in
  let h = lam libs. lam. lam impls. foldl g libs impls in
  let libs =
    mapFoldWithKey h (setEmpty cmpString) extNameMap
  in
  setToSeq libs


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
        let libs = collectLibraries env.exts in

        -- Pretty print OCaml program
        let code = use OCamlPrettyPrint in expr2str ast
        in

        -- Compile and run the OCaml program
        let options = {optimize = true, libraries = libs} in
        let cunit = ocamlCompileWithConfig options code in
        let res = cunit.run "" [] in
        cunit.cleanup ();
        print (join [res.stdout, "\n"])

   else never
  else never
