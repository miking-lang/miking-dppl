-- RootPPL language fragment

include "option.mc"
include "name.mc"
include "c/ast.mc"
include "c/pprint.mc"

--------------
-- KEYWORDS --
--------------

-- Explicit handles on certain keywords
let nameBblocksArr = nameSym "bblocksArr"
let nameBblockCall = nameSym "BBLOCK_CALL"
let nameBblockJump = nameSym "BBLOCK_JUMP"
let nameDataPointer = nameSym "DATA_POINTER"
let nameNull = nameSym "NULL"
let nameUIntPtr = nameSym "uintptr_t"
let nameProgStateTy = nameSym "progState_t"
let namePplFuncTy = nameSym "pplFunc_t"

let rpKeywords = concat (map nameNoSym [
  "BBLOCK", "BBLOCK_DECLARE", "SAMPLE", "WEIGHT", "PSTATE", "NEXT", "bernoulli",
  "beta", "discrete", "multinomial", "dirichlet", "exponential", "uniform",
  "poisson", "gamma", "INIT_MODEL", "MAIN", "SMC", "ADD_BBLOCK", "particleIdx",
  "lnFactorial"
]) [
  nameBblocksArr, nameBblockCall, nameBblockJump, nameDataPointer, nameNull,
  nameUIntPtr, nameProgStateTy, namePplFuncTy
]

lang RootPPL = CAst + CPrettyPrint

  --------------------------
  -- AST (based on C AST) --
  --------------------------

  syn CTop =

  -- Global data declarations
  | CTBBlockData { ty: CType, id: Name }

  -- Basic block helpers (essentially regular functions with special syntax)
  | CTBBlockHelperDecl { ret: CType, id: Name, params: [CType] }
  | CTBBlockHelper { ret: CType, id: Name, params: [(CType,Name)], body: [CStmt] }

  -- Basic blocks
  | CTBBlockDecl { id : Name }
  | CTBBlock { id: Name, body: [CStmt] }


  sem smap_CTop_CExpr (f: CExpr -> CExpr) =
  | CTBBlockData _ & t -> t
  | CTBBlockHelperDecl _ & t -> t
  | CTBBlockHelper t ->
    CTBBlockHelper { t with body = map (smap_CStmt_CExpr f) t.body }
  | CTBBlockDecl _ & t -> t
  | CTBBlock t -> CTBBlock { t with body = map (smap_CStmt_CExpr f) t.body }


  sem sreplace_CTop_CStmt (f: CStmt -> [CStmt]) =
  | CTBBlockData _ & t -> t
  | CTBBlockHelperDecl _ & t -> t
  | CTBBlockHelper t -> CTBBlockHelper { t with body = join (map f t.body) }
  | CTBBlockDecl _ & t -> t
  | CTBBlock t -> CTBBlock { t with body = join (map f t.body) }


  sem sfold_CTop_CStmt (f: a -> CStmt -> a) (acc: a) =
  | CTBBlockData _ -> acc
  | CTBBlockHelperDecl _ -> acc
  | CTBBlockHelper t -> foldl f acc t.body
  | CTBBlockDecl _ -> acc
  | CTBBlock t -> foldl f acc t.body


  syn CExpr =
  | CESample { dist: CDist }
  | CEWeight { weight: CExpr }
  | CEPState {}
  | CENext {}


  sem sfold_CExpr_CExpr (f: a -> CExpr -> a) (acc: a) =
  | CESample t -> sfold_CDist_CExpr f acc t.dist
  | CEWeight t -> f acc t.weight
  | CEPState _ -> acc
  | CENext _ -> acc


  sem smap_CExpr_CExpr (f: CExpr -> CExpr) =
  | CESample t -> CESample { t with dist = smap_CDist_CExpr f t.dist }
  | CEWeight t -> CEWeight { t with weight = f t.weight }
  | CEPState _ & t -> t
  | CENext _ & t -> t


  syn CDist =
  | CDBern { p: CExpr }
  | CDBeta { a: CExpr, b: CExpr }
  | CDCategorical { p: CExpr }
  | CDMultinomial { n: CExpr, p: CExpr }
  | CDDirichlet { a: CExpr }
  | CDExp { rate: CExpr }
  | CDEmpirical { samples: CExpr }
  | CDUniform { a: CExpr, b: CExpr }
  | CDPoisson { lambda: CExpr }
  | CDGamma { k: CExpr, theta: CExpr }


  sem sfold_CDist_CExpr (f: a -> CExpr -> a) (acc: a) =
  | CDBern t -> f acc t.p
  | CDBeta t -> f (f acc t.a) t.b
  | CDCategorical t -> f acc t.p
  | CDMultinomial t -> f (f acc t.n) t.p
  | CDDirichlet t -> f acc t.a
  | CDExp t -> f acc t.rate
  | CDEmpirical t -> f acc t.samples
  | CDUniform t -> f (f acc t.a) t.b
  | CDPoisson t -> f acc t.lambda
  | CDGamma t -> f (f acc t.k) t.theta


  sem smap_CDist_CExpr (f: CExpr -> CExpr) =
  | CDBern t -> CDBern { t with p = f t.p }
  | CDBeta t -> CDBeta {{ t with a = f t.a } with b = f t.b }
  | CDCategorical t -> CDCategorical { t with p = f t.p }
  | CDMultinomial t -> CDMultinomial {{ t with n = f t.n } with p = f t.p }
  | CDDirichlet t -> CDDirichlet { t with a = f t.a }
  | CDExp t -> CDExp { t with rate = f t.rate }
  | CDEmpirical t -> CDEmpirical { t with samples = f t.samples }
  | CDUniform t -> CDUniform {{ t with a = f t.a } with b = f t.b }
  | CDPoisson t -> CDPoisson { t with lambda = f t.lambda }
  | CDGamma t -> CDGamma {{ t with k = f t.k } with theta = f t.theta }


  syn RPProg =
  | RPProg {

      -- Header files to include
      includes: [String],

      -- The initial block for the model
      startBlock: Name,

      -- Program state type (None () for the RootPPL stack PSTATE)
      pStateTy: Option CType,

      -- Type definitions
      types: [CTop],

      -- Top-level definitions
      tops: [CTop],

      -- Code that runs prior to inference
      pre: [CStmt]

    }


  ---------------------
  -- PRETTY PRINTING --
  ---------------------

  sem printCTop (indent : Int) (env: PprintEnv) =

  | CTBBlockData { ty = ty, id = id } ->
    match pprintEnvGetStr env id with (env,id) then
      match printCType "" env ty with (env,ty) then
        (env, join ["BBLOCK_DATA_MANAGED(", id, ", ", ty, ", 1)"])
      else never
    else never

  | CTBBlockHelperDecl { ret = ret, id = id, params = params } ->
    let i = indent in
    let ii = pprintIncr indent in
    match pprintEnvGetStr env id with (env,id) then
      let f = printCType "" in
      match mapAccumL f env params with (env,params) then
        let params = strJoin ", " params in
        match printCType "" env ret with (env,ret) then
          (env, join ["BBLOCK_HELPER_DECLARE(", id, ", ", ret, ", ", params, ");"])
        else never
      else never
    else never

  | CTBBlockHelper { ret = ret, id = id, params = params, body = body } ->
    let i = indent in
    let ii = pprintIncr indent in
    match pprintEnvGetStr env id with (env,id) then
      let f = lam env. lam t: (CType,Name).
        match pprintEnvGetStr env t.1 with (env,t1) then
          printCDef env t.0 t1 (None ())
        else never in
      match mapAccumL f env params with (env,params) then
        let params = strJoin ", " params in
        match printCType "" env ret with (env,ret) then
          match printCStmts ii env body with (env,body) then
            (env, join ["BBLOCK_HELPER(", id, ", {",
                        pprintNewline ii, body, pprintNewline i,
                        "}, ", ret, ", ", params, ")"])
          else never
        else never
      else never
    else never

  | CTBBlockDecl { id = id } ->
    match pprintEnvGetStr env id with (env,id) then
      (env, join [ "BBLOCK_DECLARE(", id, ");" ])
    else never

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

  | CENext _ -> (env, "NEXT")


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
      error "CDCategorical not yet implemented"
      -- (env, strJoin ", " ["discrete", p, "TODO"])
    else never

  | CDMultinomial { n = n, p = p } ->
    match printCExpr env n with (env,n) then
      match printCExpr env p with (env,p) then
        error "CDMultinomial not yet implemented"
        -- (env, strJoin ", " ["multinomial", p, n, "TODO", "TODO"])
      else never
    else never

  | CDDirichlet { a = a } ->
    match printCExpr env a with (env,a) then
      error "CDDirichlet not yet implemented"
      -- (env, strJoin ", " ["dirichlet", a, "TODO", "TODO"])
    else never

  | CDExp { rate = rate } ->
    match printCExpr env rate with (env,rate) then
      (env, strJoin ", " ["exponential", rate])
    else never

  | CDEmpirical { samples = samples } ->
    error "Empirical distribution cannot be compiled"

  | CDUniform { a = a, b = b } ->
    match printCExpr env a with (env,a) then
      match printCExpr env b with (env,b) then
        (env, strJoin ", " ["uniform", a, b])
      else never
    else never

  | CDPoisson { lambda = lambda } ->
    match printCExpr env lambda with (env,lambda) then
      (env, strJoin ", " ["poisson", lambda])
    else never

  | CDGamma { k = k, theta = theta } ->
    match printCExpr env k with (env,k) then
      match printCExpr env theta with (env,theta) then
        (env, strJoin ", " ["gamma", k, theta])
      else never
    else never


  sem printRPProg (nameInit: [Name]) =
  | RPProg {
      includes = includes,
      startBlock = startBlock,
      pStateTy = pStateTy,
      types = types,
      tops = tops,
      pre = pre
    } ->

    let indent = 0 in
    let includes = map (lam inc. join ["#include ", inc]) includes in
    let addName = lam env. lam name.
      match pprintAddStr env name with Some env then env
      else error (join ["Duplicate name in printRPProg: ", nameGetStr name])
    in
    let env = foldl addName pprintEnvEmpty rpKeywords in
    let env = foldl addName env (map nameNoSym cKeywords) in
    let env = foldl addName env nameInit in

    let printProgState = lam indent. lam env. lam pStateTy.
      match pStateTy with Some pStateTy then
        let progState = CTTyDef { ty = pStateTy, id = nameProgStateTy } in
        printCTop indent env progState
      else (env,"")
    in

    match printProgState indent env pStateTy with (env,pStateTy) then
    match mapAccumL (printCTop indent) env types with (env,types) then
    match mapAccumL (printCTop indent) env tops with (env,tops) then
    match printCStmts 2 env pre with (env,pre) then
    match pprintEnvGetStr env startBlock with (env,startBlock) then

    let init =
      match pStateTy with Some _ then "INIT_MODEL(progState_t,"
      else "INIT_MODEL_STACK("
    in

    let pStateTy = match pStateTy with "" then [] else [pStateTy] in

    strJoin (pprintNewline indent) (join [
      includes,
      types,
      pStateTy,
      [ join [ init
             , ")"
             ]
      ],
      tops,
      [ "MAIN({"
      , if null pre then "" else concat "  " pre
      , join ["  FIRST_BBLOCK(", startBlock ,");"]
      , "  SMC(NULL);"
      , "})"
      ]
    ])

    else never
    else never
    else never
    else never
    else never
end

mexpr
use RootPPL in

()
