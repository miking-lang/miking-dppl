include "pgm.mc"

type Env = Map Name Expr

lang Transformation = ProbabilisticGraphicalModel

  sem save (savedLets:[Expr]) =
  | TmLet ({body=TmAssume _}&t) -> save savedLets t.inexpr
  | TmLet ({body=TmObserve _}&t) -> save savedLets t.inexpr
  | TmLet ({body=TmPlate _}&t) -> save savedLets t.inexpr
  | TmLet t -> save (cons (TmLet t) savedLets) t.inexpr
  | t -> sfold_Expr_Expr save savedLets t

  sem remove =
  | TmLet ({body=TmAssume _}&t) -> TmLet {t with inexpr = remove t.inexpr}
  | TmLet ({body=TmObserve _}&t) -> TmLet {t with inexpr = remove t.inexpr}
  | TmLet ({body=TmPlate _}&t) -> TmLet {t with inexpr = remove t.inexpr}
  | TmLet t -> remove t.inexpr
  | t -> smap_Expr_Expr remove t

  sem place (model:Expr) =
  | [TmLet t] ++ as -> place (TmLet {t with inexpr=model}) as
  | [] -> model

  sem collect (ctx:{assumeMap:Map Name Expr, observeAssumeMap:Map Name Name, env:Env}) =
  | TmLet ({body = TmAssume a} & t) -> let assumeMap =
    match a.dist with TmDist {dist = DBeta b} then
      mapInsert t.ident t.body ctx.assumeMap
    else ctx.assumeMap in
    collect {ctx with assumeMap=assumeMap} t.inexpr
  | TmLet ({body = TmObserve o} & t) ->
    let updatedCtx =
      match o.dist with TmDist {dist = DBernoulli b} then
        match b.p with TmVar v then
          match mapLookup v.ident ctx.assumeMap with Some (TmAssume a) then
            {{ctx with assumeMap= (mapInsert v.ident (TmAssume {a with dist=(updateAssume o.value a.dist)}) ctx.assumeMap)} with
            observeAssumeMap = mapInsert t.ident v.ident ctx.observeAssumeMap}
          else ctx
        else ctx
      else ctx
    in collect updatedCtx t.inexpr
  | TmLet ({body = TmPlate p} & t) ->
    match p.fun with TmLam l then
      match l.body with TmAssume a then
        let assumeMap =
          match a.dist with TmDist {dist = DBeta b} then
            mapInsert t.ident l.body ctx.assumeMap
          else ctx.assumeMap
        in
        collect {ctx with assumeMap=assumeMap} t.inexpr
      else match l.body with TmObserve o then
        let updatedCtx =
          match o.dist with TmDist {dist = DBernoulli b} then
            match p.lst with TmVar v then
              match mapLookup v.ident ctx.assumeMap with Some (TmAssume a) then
                -- make sure that o.value is not part of lambda!
                {{ctx with assumeMap=(mapInsert v.ident (TmAssume {a with dist=(updateAssume o.value a.dist)}) ctx.assumeMap)} with
            observeAssumeMap = mapInsert t.ident v.ident ctx.observeAssumeMap}
              else ctx
            else ctx
          else ctx
        in
        collect updatedCtx t.inexpr
      else never -- cannot be sth other than observe or assume
    else never -- plate should be consist of lambda term
  | TmLet t -> collect {ctx with env = (mapInsert t.ident t.body ctx.env)} t.inexpr
  | t -> sfold_Expr_Expr collect ctx t

  sem updateAssume (val:Expr) =
  | TmDist ({dist = DBeta t} & d)-> TmDist {d with dist = (DBeta ({{t with a = (if_ val (addf_ t.a (float_ 1.0)) t.a)} with b=(if_ val t.b (addf_ t.b (float_ 1.0)))}))}
  | t -> t

  sem reconstruct (ctx:{assumeMap:Map Name Expr, observeAssumeMap:Map Name Name, env:Env}) =
  | TmLet ({body=TmObserve o}&t) -> match mapLookup t.ident ctx.observeAssumeMap with Some _ then
                                       reconstruct ctx t.inexpr
                                     else TmLet {t with inexpr = reconstruct ctx t.inexpr}
  | TmLet ({body=TmAssume a}&t) -> match mapLookup t.ident ctx.assumeMap with Some a then
                                     TmLet {{t with body = a} with inexpr = reconstruct ctx t.inexpr}
                                   else TmLet {t with inexpr = reconstruct ctx t.inexpr}
  | TmLet ({body=TmPlate p}&t) -> match p.fun with TmLam l then
                                    match l.body with TmAssume a then
                                      match mapLookup t.ident ctx.assumeMap with Some a then
                                        TmLet {{t with body=(TmPlate {p with fun=(TmLam {l with body=a})})} with inexpr= reconstruct ctx t.inexpr}
                                      else TmLet {t with inexpr = reconstruct ctx t.inexpr}
                                    else match l.body with TmObserve o then
                                      match mapLookup t.ident ctx.observeAssumeMap with Some _ then
                                        reconstruct ctx t.inexpr
                                      else TmLet {t with inexpr = reconstruct ctx t.inexpr}
                                    else never
                                  else never
  | TmLet ({body=TmSeq s}&t) -> TmLet {t with inexpr = reconstruct ctx t.inexpr}
  | t -> smap_Expr_Expr (reconstruct ctx) t

end

let move = lam model.
  use Transformation in
  let savedLets = save [] model in
  let removedModel = remove model in
  place removedModel savedLets

let transform = lam model.
  use Transformation in
  let movedModel = move model in
  reconstruct (collect {assumeMap=_emptyEnv, observeAssumeMap=_emptyEnv, env=_emptyEnv} movedModel) movedModel

lang TestLang = Transformation + MExprPPL
end

mexpr

use TestLang in

let simple1example = use MExprPPL in
  bindall_
  [ ulet_ "x" (assume_ (beta_ (float_ 10.0) (float_ 5.0)))
  , ulet_ "obs" true_
  , ulet_ "obs2" (var_ "obs")
  , ulet_ "" (observe_ (var_ "obs2") (bern_ (var_ "x")))
  , var_ "x"
  ]
in

let tsimple1example = use MExprPPL in
  bindall_
  [ ulet_ "obs" true_
  , ulet_ "obs2" (var_ "obs")
  , ulet_ "x" (assume_ (beta_ (if_ (var_ "obs2") (addf_ (float_ 10.0) (float_ 1.0)) (float_ 10.0)) (if_ (var_ "obs2") (float_ 5.0) (addf_ (float_ 5.0) (float_ 1.0)))))
  , var_ "x"
  ]
  in

let simple2example = use MExprPPL in
  bindall_
  [ ulet_ "x" (assume_ (beta_ (float_ 10.0) (float_ 5.0)))
  , ulet_ "obs" (float_ 10.0)
  , ulet_ "" (observe_ (var_ "obs") (exp_ (var_ "x")))
  , var_ "x"
  ]
in
let tsimple2example = use MExprPPL in
  bindall_
  [ ulet_ "obs" (float_ 10.0)
  , ulet_ "x" (assume_ (beta_ (float_ 10.0) (float_ 5.0)))  , ulet_ "" (observe_ (var_ "obs") (exp_ (var_ "x")))
  , var_ "x"
  ]
in


let example1expanded = use MExprPPL in
  bindall_
  [ ulet_ "theta" (assume_ (beta_ (float_ 10.0) (float_ 10.0)))
  , ulet_ "" (observe_ true_ (bern_ (var_ "theta")))
  , var_ "theta"
  ]
in

let texample1expanded = use MExprPPL in
  bindall_
  [ ulet_ "theta" (assume_ (beta_ (if_ true_ (addf_ (float_ 10.0) (float_ 1.0)) (float_ 10.0)) (if_ true_ (float_ 10.0) (addf_ (float_ 10.0) (float_ 1.0)))))
  , var_ "theta"
  ]
in
/-
let example1plate = use MExprPPL in
  bindall_
  [ ulet_ "theta" (assume_ (beta_ (float_ 10.0) (float_ 10.0)))
  , ulet_ "obs" (seq_ [true_, false_, true_])
  , ulet_ "" (plate_ (ulam_ "x" (observe_ (var_ "x") (bern_ (var_ "theta")))) (var_ "obs"))
  , (var_ "theta")
  ] in

let texample1plate = use MExprPPL in
  bindall_
  [ ulet_ "theta" (assume_ (beta_ (addi_ (addi_ (float_ 10.0) (float_ 1.0)) (float_ 1.0)) (addi_ (float_ 10.0) (float_ 1.0))))
  , var_ "theta"
  ] in

let example2expanded = use MExprPPL in
  let r1 = assume (Beta 10.0 10.0) in
  let r2 = assume (Beta 15.0 1.0) in
  let r3 = assume (Beta 21.0 10.0) in
  observe true (Bernoulli r1);
  observe false (Bernoulli r2);
  observe true (Bernoulli r3);
  [r1,r2,r3]
in

let texample2expanded =
  let r1 = assume (Beta (if true then (addf 10.0 1.0) else 10.0) (if true then 10.0 else (addf 10.0 1.0))) in
  let r2 = assume (Beta (if false then (addf 10.0 1.0) else 10.0) (if false then 10.0 else (addf 10.0 1.0))) in
  let r3 = assume (Beta (if true then (addf 21.0 1.0) else 21.0) (if true then 10.0 else (addf 10.0 1.0))) in
  [r1,r2,r3] in
-/
let example2plate = use MExprPPL in
  bindall_
  [ ulet_ "params" (seq_ [(utuple_ [float_ 10.0,float_ 10.0]), (utuple_ [float_ 15.0,float_ 1.0]), (utuple_ [float_ 21.0,float_ 10.0])])
  , ulet_ "rvs" (plate_ (ulam_ "x" (assume_ (beta_ (tupleproj_ 0 (var_ "x")) (tupleproj_ 1 (var_ "x"))))) (var_ "params"))
 , ulet_ "obs" true_
  , ulet_ "" (plate_ (ulam_ "x" (observe_ (var_ "obs") (bern_ (var_ "x")))) (var_ "rvs"))
  , var_ "rvs"
  ] in

let texample2plate = use MExprPPL in
  bindall_
  [  ulet_ "params" (seq_ [(utuple_ [float_ 10.0,float_ 10.0]), (utuple_ [float_ 15.0,float_ 1.0]), (utuple_ [float_ 21.0,float_ 10.0])])
  , ulet_ "obs" true_
  , ulet_ "rvs" (plate_ (ulam_ "x" (assume_ (beta_ (if_ (var_ "obs") (addf_ (tupleproj_ 0 (var_ "x")) (float_ 1.0)) (tupleproj_ 0 (var_ "x")) ) (if_ (var_ "obs") (tupleproj_ 1 (var_ "x"))(addf_ (tupleproj_ 1 (var_ "x")) (float_ 1.0)))))) (var_ "params"))
  , var_ "rvs"
  ] in

let lda = use MExprPPL in
  bindall_
  [ ulet_ "numtopics" (int_ 2) -- the number of topics
  , ulet_ "numdocs" (int_ 3)
  , ulet_ "vocabsize" (int_ 4)
  , ulet_ "vocabulary" (seq_ [int_ 0, int_ 1, int_ 2, int_ 3]) -- word ids
  , ulet_ "docs" (seq_ [int_ 2, int_ 1, int_ 1, int_ 3, int_ 0, int_ 3, int_ 0, int_ 1, int_ 2, int_ 2]) -- word ids for the corpus
  , ulet_ "docids" (seq_ [int_ 0, int_ 0, int_ 0, int_ 1, int_ 1, int_ 1, int_ 1, int_ 2, int_ 2, int_ 2]) -- doc id for each word in the corpus
  , ulet_ "alpha" (appf2_ (var_ "make") (var_ "numtopics") (float_ 0.1)) --topic prior distributions hyperparameter
  , ulet_ "beta" (appf2_ (var_ "make") (var_ "vocabsize") (float_ 0.1)) --word prior distributions hyperparameter
  , ulet_ "phi" (plate_ (ulam_ "x" (assume_ (dirichlet_ (var_ "beta_")))) (appf3_ (var_ "range") (int_ 1) (var_ "numtopics") (int_ 1)))
  , ulet_ "theta" (plate_ (ulam_ "x" (assume_ (dirichlet_ (var_ "alpha_")))) (appf3_ (var_ "range") (int_ 1) (var_ "numdocs") (int_ 1)))
  , ulet_ "z" (plate_ (ulam_ "w" (assume_ (categorical_ (get_ (var_ "theta") (get_ (var_ "docids") (var_ "w")))))) (range (int_ 0) (length_ (var_ "docs"))))
  , ulet_ "" (plate_ (ulam_ "w" (observe_ (get_ (var_ "docs") (var_ "w")) (categorical_ (get_ (var_ "phi") (get_ (var_ "w") (var_ "z")))))) (range (int_ 0) (length_ (var_ "docs"))))
  , seq_ [var_ "phi", var_ "theta", var_ "z"]
  ]
in

utest (transform simple1example) with tsimple1example using eqExpr in
utest (transform simple2example) with tsimple2example using eqExpr in
utest transform example1expanded with texample1expanded using eqExpr in
utest transform example2plate with texample2plate using eqExpr in
()
