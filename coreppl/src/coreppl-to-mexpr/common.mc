include "mexpr/ast-builder.mc"
include "mexpr/boot-parser.mc"

include "../coreppl.mc"
include "../src-location.mc"

let parseRuntime = use BootParser in lam runtime. parseMCoreFile {
  defaultBootParserParseMCoreFileArg with
    eliminateDeadCode = false,
    allowFree = true
  } (join [corepplSrcLoc, "/coreppl-to-mexpr/", runtime])

lang MExprPPLCommon = MExprPPL

  sem _replaceHigherOrderConstant: Const -> Option Expr
  sem _replaceHigherOrderConstant =
  | CMap _ -> Some (var_ "map")
  | CMapi _ -> Some (var_ "mapi")
  | CFoldl _ -> Some (var_ "foldl")
  | CFoldr _ -> Some (var_ "foldr")
  | CCreate _ -> Some (var_ "create")
  | _ -> None ()

  sem _replaceHigherOrderConstantExpr: Expr -> Expr
  sem _replaceHigherOrderConstantExpr =
  | TmConst r ->
    match _replaceHigherOrderConstant r.val with Some t then
      withType r.ty (withInfo r.info t)
    else TmConst r
  | t -> t

  sem replaceHigherOrderConstants: Expr -> Expr
  sem replaceHigherOrderConstants =
  | t ->
    let t = mapPre_Expr_Expr _replaceHigherOrderConstantExpr t in
    let replacements = parseRuntime "runtime-const.mc" in
    bind_ replacements t

end
