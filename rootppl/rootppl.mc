-- RootPPL language fragment

include "option.mc"
include "c/ast.mc"
include "c/pprint.mc"

lang RootPPL = CAst + CPrettyPrint

  --------------------------
  -- AST (based on C AST) --
  --------------------------

  syn CTop =

  -- Global data declarations
  | CTBBlockData { ty: CType, id: Name }

  -- Basic block helpers (essentially regular functions with special syntax)
  | CTBBlockHelperDecl { ty: CType, id: Name }
  | CTBBlockHelper { ret: CType, id: Name, params: [(CType,Name)], body: [Stmt] }

  -- Basic blocks
  | CTBBlockDecl { id : Name }
  | CTBBlock { id: Name, body: [Stmt] }

  sem sreplace_CTop_CStmt (f: CStmt -> [CStmt]) =
  | CTBBlockData _ & t -> t
  | CTBBlockHelperDecl _ & t -> t
  | CTBBlockHelper t -> CTBBlockHelper { t with body = join (map f t.body) }
  | CTBBlockDecl _ & t -> t
  | CTBBlock t -> CTBBlock { t with body = join (map f t.body) }

  syn CExpr =
  | CESample { dist: CDist }
  | CEWeight { weight: CExpr }
  | CEPState {}
  | CEPC {}

  sem sfold_CExpr_CExpr (f: a -> CExpr -> a) (acc: a) =
  | CESample t -> sfold_CDist_CExpr f acc t.dist
  | CEWeight t -> f acc t.weight
  | CEPState _ -> acc
  | CEPC _ -> acc

  sem smap_CExpr_CExpr (f: CExpr -> CExpr) =
  | CESample t -> CESample { t with dist = smap_CDist_CExpr f t.dist }
  | CEWeight t -> CEWeight { t with weight = f t.weight }
  | CEPState _ & t -> t
  | CEPC _ & t -> t

  syn CDist =
  | CDBern { p: CExpr }
  | CDBeta { a: CExpr, b: CExpr }
  | CDCategorical { p: CExpr }
  | CDMultinomial { n: CExpr, p: CExpr }
  | CDDirichlet { a: CExpr }
  | CDExp { rate: CExpr }
  | CDEmpirical { samples: CExpr }

  sem sfold_CDist_CExpr (f: a -> CExpr -> a) (acc: a) =
  | CDBern t -> f acc t.p
  | CDBeta t -> f (f acc t.a) t.b
  | CDCategorical t -> f acc t.p
  | CDMultinomial t -> f (f acc t.n) t.p
  | CDDirichlet t -> f acc t.a
  | CDExp t -> f acc t.rate
  | CDEmpirical t -> f acc t.samples

  sem smap_CDist_CExpr (f: CExpr -> CExpr) =
  | CDBern t -> CDBern { t with p = f t.p }
  | CDBeta t -> CDBeta {{ t with a = f t.a } with b = f t.b }
  | CDCategorical t -> CDCategorical { t with p = f t.p }
  | CDMultinomial t -> CDMultinomial {{ t with n = f t.n } with p = f t.p }
  | CDDirichlet t -> CDDirichlet { t with a = f t.a }
  | CDExp t -> CDExp { t with rate = f t.rate }
  | CDEmpirical t -> CDEmpirical { t with samples = f t.samples }

  syn RPProg =
  | RPProg { includes: [String], pStateTy: CType, tops: [CTop] }


  ---------------------
  -- PRETTY PRINTING --
  ---------------------

  sem printCTop (indent : Int) (env: PprintEnv) =
  | CTBBlock { id = id, body = body } ->
    let i = indent in
    let ii = pprintIncr indent in
    match pprintEnvGetStr env id with (env,id) then
      match printCStmts ii env body with (env,body) then
        (env, join [
          "BBLOCK(", id, ", {", pprintNewline ii, body, pprintNewline i, "})"
        ])
      else never
    else never

  sem printCExpr (env: PprintEnv) =
  | CESample { dist = dist } ->
    match printCDist env dist with (env,dist) then
      (env, _par (join ["SAMPLE(", dist, ")"]))
    else never
  | CEWeight { weight = weight } ->
    match printCExpr env weight with (env,weight) then
      (env, _par (join ["WEIGHT(", weight, ")"]))
    else never
  | CEPState {} -> (env, "PSTATE")
  | CEPC _ -> (env, "PC")

  sem printCDist (env: PprintEnv) =
  | CDBern { p = p } ->
    match printCExpr env p with (env,p) then
      (env, strJoin ", " ["bernoulli", p])
    else never

  | CDBeta { a = a, b = b } ->
    match printCExpr env a with (env,a) then
      match printCExpr env b with (env,b) then
        (env, strJoin ", " ["beta", a, b])
      else never
    else never

  | CDCategorical { p = p } ->
    match printCExpr env p with (env,p) then
      (env, strJoin ", " ["discrete", p, "TODO"])
    else never

  | CDMultinomial { n = n, p = p } ->
    match printCExpr env n with (env,n) then
      match printCExpr env p with (env,p) then
        (env, strJoin ", " ["multinomial", p, n, "TODO", "TODO"])
      else never
    else never

  | CDDirichlet { a = a } ->
    match printCExpr env a with (env,a) then
      (env, strJoin ", " ["dirichlet", a, "TODO", "TODO"])
    else never

  | CDExp { rate = rate } ->
    match printCExpr env rate with (env,rate) then
      (env, strJoin ", " ["exponential", rate])
    else never

  | CDEmpirical { samples = samples } ->
    error "Empirical distribution cannot be compiled"


  sem printRPProg (nameInit: [Name]) =
  | RPProg { includes = includes, pStateTy = pStateTy, tops = tops } ->
    let indent = 0 in
    let includes = map (lam inc. join ["#include ", inc]) includes in
    let addName = lam env. lam name.
      match pprintAddStr env name with Some env then env
      else error (join ["Duplicate name in printCProg: ", nameGetStr name])
    in
    let env = foldl addName pprintEnvEmpty nameInit in
    match mapAccumL (printCTop indent) env tops with (env,tops) then
      strJoin (pprintNewline indent) (join [includes, tops])
    else never


end

mexpr
use RootPPL in

()
