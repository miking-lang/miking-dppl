-- Sketch of CUDA language fragment

include "option.mc"
include "c/ast.mc"

lang RootPPL = CTopAst + CStmtAst

  syn CTop =

  -- Global data declarations
  | TBBlockData { ty: CType,
                  id: Name }

  -- Basic block helpers (essentially regular functions with special syntax)
  | TBBlockHelper { ty     : CType,
                    id     : Name,
                    params : [(CType,Name)],
                    body   : [Stmt] }

  -- Basic blocks
  | TBBlock { id   : Name,
              body : [Stmt] }

  syn CExpr =
  | CESample { dist: CDist }
  | CEWeight { weight: CExpr }

  syn CDist =
  | CDBern { p: CExpr }
  | CDBeta { a: CExpr, b: CExpr}
  | CDCategorical { p: CExpr }
  | CDMultinomial { n: CExpr, p: CExpr }
  | CDDirichlet { a: CExpr }
  | CDExp { rate: CExpr }
  | CDEmpirical { samples: CExpr }

end

mexpr
use RootPPL in

()
