include "mexpr/ast-builder.mc"
include "mexpr/externals.mc"
include "mexpr/boot-parser.mc"
include "sys.mc"

include "../coreppl.mc"

lang MExprImportanceCompile = MExprPPL

  sem compile (includes: Expr) =
  | prog -> map_Expr_Expr translateCorePPL prog

  sem translateCorePPL =
  | TmAssume t -> app_ (var_ "assume") (int_ 1234)
  | TmObserve t -> app_ (var_ "observe") t.value
  | TmWeight t -> app_ (var_ "weight") t.weight
  -- | TmDist _ -> TODO(dlunde,2022-05-04): How do we handle distribution objects?
  | expr -> expr

end

lang MExprCompile = MExprPPL + Externals
end

-- NOTE(dlunde,2022-05-04): No way to distinguish between CorePPL and MExpr AST
-- types here. Optimally, the type would be Options -> CorePPLExpr -> MExprExpr
-- or similar.
let mexprCompile: Options -> Expr -> Expr =
  use MExprCompile in
  lam options. lam prog.

    printLn (mexprToString prog); exit 0;

    let parse = use BootParser in parseMCoreFile {{
      defaultBootParserParseMCoreFileArg
      with eliminateDeadCode = false }
      with allowFree = true }
    in

    -- Load includes from Miking stdlib that must be available in the compiled program
    let tmpFile = sysTempFileMake () in
    -- NOTE: This should be an actual file _in Miking DPPL_ instead, so that we can include things relative to it.
    writeFile tmpFile (join (map (lam i. join ["include \"", i, "\"\n"]) [
      "ext/dist-ext.mc",
      "ext/math-ext.mc"
    ]));
    let includes: Expr = parse tmpFile in
    sysDeleteFile tmpFile;
    -- NOTE: If we also need dead code elimination, we can use the "keywords" option to parseMCoreFile

    -- Get the names of all externals loaded in the above step
    let externalIds: Set String = getExternalIds includes in

    -- Symbolize the input program ...
    let prog = symbolize prog in
    -- ... but unsymbolize externals and remove duplicate external definitions
    let prog = unSymbolizeExternals prog in
    let prog = removeExternalDefs externalIds prog in

    let prog =
      match options.method with "mexpr-is" then
        use MExprImportanceCompile in compile includes prog
      else
        error (join [
          "Unknown CorePPL to MExpr inference option:", options.method
        ])
    in

    prog

