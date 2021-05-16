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

  syn CExpr =
  | CESample { dist: CDist }
  | CEWeight { weight: CExpr }
  | CEPState {}

  syn CDist =
  | CDBern { p: CExpr }
  | CDBeta { a: CExpr, b: CExpr }
  | CDCategorical { p: CExpr }
  | CDMultinomial { n: CExpr, p: CExpr }
  | CDDirichlet { a: CExpr }
  | CDExp { rate: CExpr }
  | CDEmpirical { samples: CExpr }

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
          "BBLOCK(", id, ", {", pprintNewline ii, body, pprintNewline i, "}"
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
  | RPProg { includes = includes, pstate = pstate, tops = tops } ->
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
