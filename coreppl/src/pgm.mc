include "mexpr/ast.mc"
include "coreppl.mc"
include "name.mc"

type Env = use Ast in Map Name Expr
let _emptyEnv = mapEmpty nameCmp

lang Plate = Eq + Sym + ANF + PrettyPrint
  syn Expr =
  | TmPlate { fun:Expr
            , lst:Expr
            , ty:Type
            , info:Info
            }
  sem infoTm =
  | TmPlate t -> t.info

  sem withInfo (info: Info) =
  | TmPlate t -> TmPlate { t with info = info }

  sem withType (ty : Type) =
  | TmPlate t -> TmPlate {t with ty=ty}

  sem smapAccumL_Expr_Expr (f : acc -> Expr -> (acc, Expr)) (acc : acc) =
  | TmPlate t ->
    match f acc t.fun with (acc,fun) in
    match f acc t.lst with (acc,lst) in
    (acc, TmPlate {{ t with fun = fun } with lst = lst})

  sem eqExprH (env : EqEnv) (free : EqEnv) (lhs : Expr) =
  | TmPlate r ->
    match lhs with TmPlate l then
      match eqExprH env free l.fun r.fun with Some free then
        eqExprH env free l.lst r.lst
      else None ()
    else None ()

  sem symbolizeExpr (env: SymEnv) =
  | TmPlate t ->
    TmPlate {{{ t with fun = symbolizeExpr env t.fun }
                  with lst = symbolizeExpr env t.lst }
                  with ty = symbolizeType env t.ty }
  sem isAtomic =
  | TmPlate _ -> false

  sem pprintCode (indent : Int) (env : PPrintEnv) =
  | TmPlate t ->
    match pprintCode 0 env t.fun with (env,fun) then
      match pprintCode 0 env t.lst with (env,lst) then
        (env, join ["plate ", fun, lst])
      else never
    else never
end

lang ProbabilisticGraphicalModel = CorePPL + MExprAst + Plate
  -- Some if error otherwise None
  -- Only lets ending with
  -- 1. TmAssume t with valid t.dist
  -- 2. TmPlate t with valid t.fun (t.fun.body contains 1. 2. 3. 4.)
  -- 3. TmVar with valid value 1. or 2. or 4.
  -- 4. TmSeq consists of valid elements 1. 2., 3. or 4.
  sem validate (env:Env) =
  | TmLet t ->
    match validateBody env t.body with Some err then Some err
    else validate (mapInsert t.ident t.body env) t.inexpr
  | TmVar t -> match mapLookup t.ident env with Some t then
                 validate env t
               else Some ("Variable not found")
               --if sth is surrounded with a lambda then it is not allowed
  | TmAssume t -> (validateDist env) t.dist
  | TmPlate t ->  match t.fun with TmLam l then
                    match l.body with TmAssume t then
                      validateDist env t.dist
                    else Some ("Plate fun check: Not supported expression in plate fun")
                  else Some ("Plate fun should be a lambda expression")
  | TmSeq t -> sfold_Expr_Expr (_validFold (validateBody env)) (None ()) (TmSeq t)
  | t -> Some ("Not supported inexpression")

  sem _validFold (f: b -> a) (acc: a) =
  | t -> match acc with Some err then Some err
         else f t

  -- Let body can be consists of
  -- 1. TmAssume t with valid t.dist
  -- 2. TmObserve t with valid t.dist
  -- 3. TmPlate t with valid t.fun (t.fun.body 1. 2. 3. 5.
  -- 4. TmConst
  -- 5. TmVar with valid value 1. 2. 3. 4.
  -- 6. A valid expression that does not contain 1. 2. 3. 4. 5.
  sem validateBody (env:Env) =
  | TmAssume t -> validateDist env t.dist
  | TmPlate t ->  match t.fun with TmLam l then
                    validatePlateFun env l.body
                  else Some ("Plate fun should be a lambda expression")
  | TmObserve t -> validateDist env t.dist
  | TmConst t -> None ()
  | expr -> sfold_Expr_Expr (_validFold (validateBody env)) (None ()) expr

  -- Plate fun can only be consists of
  -- 1. TmAssume t with valid t.dist
  -- 2. TmObserve t with valid t.dist
  sem validatePlateFun (env:Env) =
  | TmAssume t -> validateDist env t.dist
  | TmObserve t -> validateDist env t.dist
  | t -> Some ("Plate fun check: Not supported expression in plate fun")

  -- Distribution can be consists of
  --  1. DBernoulli
  --  2. DBeta
  --  3. TmDist t with valid t.dist
  --  4. TmVar with valid value 1. 2. 3.
  sem validateDist (env:Env) =
  | DBernoulli d -> None ()
  | DBeta d -> None ()
  | DCategorical d -> None ()
  | DDirichlet d -> None ()
  | TmDist d -> validateDist env d.dist
  | TmVar t -> match mapLookup t.ident env with Some t then
                 validateDist env t
               else Some ("Distribution check: variable not found")
  | t -> Some ("Distribution check: Not supported distribution")

end

let plate_ = use Plate in
  lam f. lam lst. TmPlate {fun=f, lst=lst, ty=tyunknown_, info=NoInfo()}

lang TestLang = ProbabilisticGraphicalModel
end

mexpr

--- TESTS ---
use TestLang in
let valid: Expr -> Env = lam t: Expr. lam e: Env. validate e t in

let env = _emptyEnv in

let lambdaletmodel1 =
  bindall_
  [ ulet_ "theta" (assume_ (beta_ (float_ 10.0) (float_ 10.0)))
  , ulet_ "n" (int_ 10)
  , ulet_ "a" (ulam_ "x" (var_ "x"))
  , ulet_ "observation" (int_ 1)
  , ulet_ ""  (observe_ true (bern_ (app_ (ulam_ "x" (var_ "x")) (var_ "theta"))))
  , var_ "theta"
  ] in

let rejectmodel1 =
  bindall_
  [ ulet_ "theta" (assume_ (beta_ (float_ 10.0) (float_ 10.0)))
  , ulet_ "n" (int_ 10)
  , ulet_ "a" (ulam_ "x" (var_ "x"))
  , ulet_ "observation" (int_ 1)
  , ulet_ ""  (observe_ true (bern_ (app_ (ulam_ "x" (var_ "x")) (var_ "theta"))))
  , var_ "observation"
  ] in

-- Cannot have variable in plate fun only assume and observe
let rejectmodel2 =
  bindall_
  [ ulet_ "theta" (assume_ (beta_ (float_ 10.0) (float_ 10.0)))
  , ulet_ "rvs" (plate_ (ulam_ "x" (var_ "theta")) (seq_ [1., 2., 3.]))
  , var_ "rvs"
  ] in

-- Inexpression of let cannot be unit
let rejectmodel3 =
  bindall_
  [ ulet_ "theta" (assume_ (beta_ (float_ 10.0) (float_ 10.0)))
  , ulet_ "rvs" (plate_ (ulam_ "x" (var_ "theta")) (seq_ [1., 2., 3.]))
  ] in

-- Inexpression of let cannot be observe
let rejectmodel4 =
  bindall_
  [ ulet_ "theta" (assume_ (beta_ (float_ 10.0) (float_ 10.0)))
  , ulet_ "rvs" (plate_ (ulam_ "x" (var_ "theta")) (seq_ [1., 2., 3.]))
  , observe_ (float_ 5) (var_ "theta")
  ] in

-- No lambda surrounding assume
let rejectmodel5 =
  bindall_
  [ plate_ (assume_ (beta_ (float_ 5.0) (float_ 10.0))) (seq_ [1., 2.])
  ] in

-- Closure cannot be handled by PGM
let rejectmodel6 =
  bindall_
  [ plate_ (ulam_ "x" (assume_ (beta_ (float_ 5.0)))) (seq_ [1., 2.])
  ] in

-- Cannot return observe
let rejectmodel7 =
  bindall_
  [ plate_ (ulam_ "x" (observe_ (float_ 0.5) (beta_ (float_ 10.0) (float_ 10.0)))) (seq_ [1., 2.])
  ] in

-- Distribution in assume should be valid.
let rejectmodel8 =
  bindall_
  [ assume_ (int_ 4)
  ] in

let rejectmodel9 =
  bindall_
  [ ulet_ "theta" (assume_ (beta_ (float_ 1.0) (float_ 1.0)))
  , plate_ (ulam_ "x" (var_ "theta")) (seq_ [1., 2.])
  ] in

let rejectmodel10 =
  bindall_
  [ ulet_ "theta" (if_ true_ (assume_ (beta_ (float_ 10.0) (float_ 10.0))) (assume_ (beta_ (float_ 10.0) (float_ 15.0))))
  , var_ "theta"
  ] in

let example1expanded = use MExprPPL in
  bindall_
  [ ulet_ "theta" (assume_ (beta_ (float_ 10.0) (float_ 10.0)))
  , ulet_ "" (observe_ true_ (bern_ (var_ "theta")))
  , ulet_ "" (observe_ false_ (bern_ (var_ "theta")))
  , ulet_ "" (observe_ true_ (bern_ (var_ "theta")))
  , (var_ "theta")
  ] in

let texample1expanded = use MExprPPL in
  bindall_
  [ ulet_ "theta" (assume_ (beta_ (addi_ (addi_ (float_ 10.0) (float_ 1.0)) (float_ 1.0)) (addi_ (float_ 10.0) (float_ 1.0))))
  , var_ "theta"
  ] in

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
  bindall_
  [ ulet_ "r1" (assume_ (beta_ (float_ 10.0) (float_ 10.0)))
  , ulet_ "r2" (assume_ (beta_ (float_ 15.0) (float_ 1.0)))
  , ulet_ "r3" (assume_ (beta_ (float_ 21.0) (float_ 10.0)))
  , ulet_ "obs" true_
  , ulet_ "" (observe_ (var_ "obs") (bern_ (var_ "r1")))
  , ulet_ "" (observe_ (var_ "obs") (bern_ (var_ "r2")))
  , ulet_ "" (observe_ (var_ "obs") (bern_ (var_ "r3")))
  , seq_ [var_ "r1", var_ "r2", var_ "r3"]
  ] in

let texample2expanded = use MExprPPL in
 bindall_
  [ ulet_ "r1" (assume_ (beta_ (float_ 11.0) (float_ 10.0)))
  , ulet_ "r2" (assume_ (beta_ (float_ 16.0) (float_ 1.0)))
  , ulet_ "r3" (assume_ (beta_ (float_ 22.0) (float_ 10.0)))
  , seq_ [var_ "r1", var_ "r2", var_ "r3"]
  ] in

let example2plate = use MExprPPL in
  bindall_
  [ ulet_ "params" (seq_ [(utuple_ [float_ 10.0,float_ 10.0]), (utuple_ [float_ 15.0,float_ 1.0]), (utuple_ [float_ 21.0,float_ 10.0])])
  , ulet_ "obs" true_
  , ulet_ "rvs" (plate_ (ulam_ "x" (assume_ (beta_ (tupleproj_ 0 (var_ "x")) (tupleproj_ 1 (var_ "x"))))) (var_ "params"))
  , ulet_ "" (plate_ (ulam_ "x" (observe_ (var_ "obs") (bern_ (var_ "x")))) (var_ "rvs"))
  , var_ "rvs"
  ] in

let texample2plate = use MExprPPL in
  bindall_
  [  ulet_ "params" (seq_ [(utuple_ [float_ 10.0,float_ 10.0]), (utuple_ [float_ 15.0,float_ 1.0]), (utuple_ [float_ 21.0,float_ 10.0])])
  , ulet_ "rvs" (plate_ (ulam_ "x" (assume_ (beta_ (tupleproj_ 0 (var_ "x")) (tupleproj_ 1 (var_ "x"))))) (var_ "params"))
  , var_ "rvs"
  ] in

let coinmodel = use MExprPPL in
  bindall_
  [ ulet_ "theta" (assume_ (beta_ (float_ 10.0) (float_ 10.0)))
  , ulet_ "n" (int_ 10)
  , ulet_ "observation" (int_ 1)
  , ulet_ "" (observe_ true (bern_ (var_ "theta")))
  , var_ "theta"
  ] in

-- add test case let not returning sth
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

let valid_tests =
[
   (coinmodel, None (), env)
 , (example1expanded, None (), env)
 , (texample1expanded, None (), env)
 , (example1plate, None (), env)
 , (texample1plate, None (), env)
 , (example2expanded, None (), env)
 , (texample2expanded, None (), env)
 , (example2plate, None (), env)
 , (texample2plate, None (), env)
 , (rejectmodel1, Some ("Not supported inexpression"), env)
 , (rejectmodel2, Some ("Plate fun check: Not supported expression in plate fun"), env)
 , (rejectmodel3, Some ("Plate fun check: Not supported expression in plate fun"), env)
 , (rejectmodel4, Some ("Plate fun check: Not supported expression in plate fun"), env)
 , (rejectmodel5, Some ("Plate fun should be a lambda expression"), env)
 , (rejectmodel6, Some ("Distribution check: Not supported distribution"), env)
 , (rejectmodel7, Some ("Plate fun check: Not supported expression in plate fun"), env)
 , (rejectmodel8, Some ("Distribution check: Not supported distribution"), env)
 , (rejectmodel9, Some ("Plate fun check: Not supported expression in plate fun"), env)
 , (rejectmodel10, Some ("Not supported inexpression"), env)
 , (lambdaletmodel1, None (), env)
 , (lda, None (), env)
] in
let test_f = lam f. lam lst. (map (lam x. utest f x.0 x.2 with x.1 in ()) lst) in
test_f valid valid_tests;

()

