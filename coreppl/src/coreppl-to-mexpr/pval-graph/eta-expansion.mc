include "mexpr/ast.mc"
include "mexpr/ast-builder.mc"

lang EtaExpansion = Ast + AppAst + FunTypeAst
  sem etaExpand : Bool -> Expr -> Expr
  sem etaExpand covered =
  | TmApp x ->
    recursive let saturate : Type -> Expr -> Expr = lam ty.
      match unwrapType ty with TyArrow x then
        let cont = saturate x.to in
        lam tm.
          let n = nameSym "x" in
          withType ty (nlam_ n x.from (cont (withType x.to (app_ tm (withType x.from (nvar_ n))))))
      else lam tm. tm in
    let tm = TmApp {x with lhs = etaExpand true x.lhs, rhs = etaExpand false x.rhs} in
    if covered then tm else saturate x.ty tm
  | tm -> smap_Expr_Expr (etaExpand false) tm
end
