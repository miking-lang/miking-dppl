-- RootPPL language fragment, including pretty printing

include "option.mc"
include "string.mc"
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
let nameCallbackParticles = nameSym "N"
let namePStates = nameSym "PSTATES"
let nameWeights = nameSym "WEIGHTS"

-- NOTE(dlunde,2021-10-08): This list is currently not exhaustive
let rpKeywords = concat (map nameSym [
  "BBLOCK", "BBLOCK_DECLARE", "BBLOCK_DATA_MANAGED",
  "BBLOCK_DATA_MANAGED_SINGLE", "", "SAMPLE", "WEIGHT", "PSTATE", "NEXT",
  "bernoulli", "beta", "discrete", "multinomial", "dirichlet", "exponential",
  "uniform", "poisson", "gamma", "INIT_MODEL", "MAIN", "SMC", "ADD_BBLOCK",
  "particleIdx", "lnFactorial", "callback", "CALLBACK"
]) [
  nameBblocksArr, nameBblockCall, nameBblockJump, nameDataPointer, nameNull,
  nameUIntPtr, nameProgStateTy, namePplFuncTy, nameCallbackParticles,
  namePStates, nameWeights
]

lang RootPPL = CAst + CPrettyPrint

  --------------------------
  -- AST (based on C AST) --
  --------------------------

  syn CTop =

  -- Global data declarations
  | CTBBlockData { ty: CType, id: Name, len: Int }
  | CTBBlockDataSingle { ty: CType, id: Name }

  -- Basic block helpers (essentially regular functions with special syntax)
  | CTBBlockHelperDecl { ret: CType, id: Name, params: [CType] }
  | CTBBlockHelper { ret: CType, id: Name, params: [(CType,Name)], body: [CStmt] }

  -- Basic blocks
  | CTBBlockDecl { id : Name }
  | CTBBlock { id: Name, body: [CStmt] }

  sem smap_CTop_CExpr (f: CExpr -> CExpr) =
  | CTBBlockData _ & t -> t
  | CTBBlockDataSingle _ & t -> t
  | CTBBlockHelperDecl _ & t -> t
  | CTBBlockHelper t ->
    CTBBlockHelper { t with body = map (smap_CStmt_CExpr f) t.body }
  | CTBBlockDecl _ & t -> t
  | CTBBlock t -> CTBBlock { t with body = map (smap_CStmt_CExpr f) t.body }


  sem sreplace_CTop_CStmt (f: CStmt -> [CStmt]) =
  | CTBBlockData _ & t -> t
  | CTBBlockDataSingle _ & t -> t
  | CTBBlockHelperDecl _ & t -> t
  | CTBBlockHelper t -> CTBBlockHelper { t with body = join (map f t.body) }
  | CTBBlockDecl _ & t -> t
  | CTBBlock t -> CTBBlock { t with body = join (map f t.body) }


  sem sfold_CTop_CStmt f acc =
  | CTBBlockData _ -> acc
  | CTBBlockDataSingle _ -> acc
  | CTBBlockHelperDecl _ -> acc
  | CTBBlockHelper t -> foldl f acc t.body
  | CTBBlockDecl _ -> acc
  | CTBBlock t -> foldl f acc t.body


  syn CExpr =
  | CESample { dist: CDist }
  | CEObserve { value: CExpr, dist: CDist }
  | CEWeight { weight: CExpr }
  | CEPState {}
  | CENext {}


  sem sfold_CExpr_CExpr f acc =
  | CESample t -> sfold_CDist_CExpr f acc t.dist
  | CEObserve t -> sfold_CDist_CExpr f (f acc t.value) t.dist
  | CEWeight t -> f acc t.weight
  | CEPState _ -> acc
  | CENext _ -> acc


  sem smap_CExpr_CExpr (f: CExpr -> CExpr) =
  | CESample t -> CESample { t with dist = smap_CDist_CExpr f t.dist }
  | CEObserve t -> CEObserve {{ t with value = f t.value }
                                  with dist = smap_CDist_CExpr f t.dist }
  | CEWeight t -> CEWeight { t with weight = f t.weight }
  | CEPState _ & t -> t
  | CENext _ & t -> t


  syn CDist =
  | CDBern { p: CExpr }
  | CDBeta { a: CExpr, b: CExpr }
  | CDCategorical { p: CExpr }
  | CDBinomial { n: CExpr, p: CExpr }
  | CDMultinomial { n: CExpr, p: CExpr }
  | CDDirichlet { a: CExpr }
  | CDExp { rate: CExpr }
  | CDEmpirical { samples: CExpr }
  | CDUniform { a: CExpr, b: CExpr }
  | CDNormal { mu: CExpr, sigma: CExpr }
  | CDPoisson { lambda: CExpr }
  | CDGamma { k: CExpr, theta: CExpr }


  sem sfold_CDist_CExpr : all acc. (acc -> CExpr -> acc) -> acc -> CDist -> acc
  sem sfold_CDist_CExpr f acc =
  | CDBern t -> f acc t.p
  | CDBeta t -> f (f acc t.a) t.b
  | CDCategorical t -> f acc t.p
  | CDBinomial t -> f (f acc t.n) t.p
  | CDMultinomial t -> f (f acc t.n) t.p
  | CDDirichlet t -> f acc t.a
  | CDExp t -> f acc t.rate
  | CDEmpirical t -> f acc t.samples
  | CDUniform t -> f (f acc t.a) t.b
  | CDNormal t -> f (f acc t.mu) t.sigma
  | CDPoisson t -> f acc t.lambda
  | CDGamma t -> f (f acc t.k) t.theta


  sem smap_CDist_CExpr (f: CExpr -> CExpr) =
  | CDBern t -> CDBern { t with p = f t.p }
  | CDBeta t -> CDBeta {{ t with a = f t.a } with b = f t.b }
  | CDCategorical t -> CDCategorical { t with p = f t.p }
  | CDBinomial t -> CDBinomial {{ t with n = f t.n } with p = f t.p }
  | CDMultinomial t -> CDMultinomial {{ t with n = f t.n } with p = f t.p }
  | CDDirichlet t -> CDDirichlet { t with a = f t.a }
  | CDExp t -> CDExp { t with rate = f t.rate }
  | CDEmpirical t -> CDEmpirical { t with samples = f t.samples }
  | CDUniform t -> CDUniform {{ t with a = f t.a } with b = f t.b }
  | CDNormal t -> CDNormal {{ t with mu = f t.mu } with sigma = f t.sigma }
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

      -- Callback code (Put within the CALLBACK RootPPL macro)
      callback: Option [CStmt],

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

  | CTBBlockData { ty = ty, id = id, len = len } ->
    match cPprintEnvGetStr env id with (env,id) then
      match printCType "" env ty with (env,ty) then
        let len = int2string len in
        (env, join ["BBLOCK_DATA_MANAGED(", id, ", ", ty, ", ", len, ")"])
      else never
    else never

  | CTBBlockDataSingle { ty = ty, id = id } ->
    match cPprintEnvGetStr env id with (env,id) then
      match printCType "" env ty with (env,ty) then
        (env, join ["BBLOCK_DATA_MANAGED_SINGLE(", id, ", ", ty, ")"])
      else never
    else never

  | CTBBlockHelperDecl { ret = ret, id = id, params = params } ->
    let i = indent in
    let ii = pprintIncr indent in
    match cPprintEnvGetStr env id with (env,id) then
      let f = printCType "" in
      match mapAccumL f env params with (env,params) then
        let params = strJoin ", " params in
        match printCType "" env ret with (env,ret) then
          (env, join (join [
             ["BBLOCK_HELPER_DECLARE(", id, ", ", ret],
             if null params then [] else [ ", ", params],
             [");"]
             ])
          )
        else never
      else never
    else never

  | CTBBlockHelper { ret = ret, id = id, params = params, body = body } ->
    let i = indent in
    let ii = pprintIncr indent in
    match cPprintEnvGetStr env id with (env,id) then
      let f = lam env. lam t: (CType,Name).
        match cPprintEnvGetStr env t.1 with (env,t1) then
          printCDef env t.0 t1 (None ())
        else never in
      match mapAccumL f env params with (env,params) then
        let params = strJoin ", " params in
        match printCType "" env ret with (env,ret) then
          match printCStmts ii env body with (env,body) then
            (env, join (join [
               [
                 "BBLOCK_HELPER(", id, ", {",
                 pprintNewline ii, body, pprintNewline i, "}, ", ret
               ],
               if null params then [] else [", ", params],
               [")"]
               ])
            )
          else never
        else never
      else never
    else never

  | CTBBlockDecl { id = id } ->
    match cPprintEnvGetStr env id with (env,id) then
      (env, join [ "BBLOCK_DECLARE(", id, ");" ])
    else never

  | CTBBlock { id = id, body = body } ->
    let i = indent in
    let ii = pprintIncr indent in
    match cPprintEnvGetStr env id with (env,id) then
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

  | CEObserve { value = value, dist = dist } ->
    match printCExpr env value with (env,value) then
      match printCDist env dist with (env,dist) then
        (env, _par (join ["OBSERVE(", dist, ", ", value, ")"]))
      else never
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

  | CDBinomial { n = n, p = p } ->
    match printCExpr env n with (env,n) then
      match printCExpr env p with (env,p) then
        (env, strJoin ", " ["binomial", p, n])
      else never
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

  | CDNormal { mu = mu, sigma = sigma } ->
    match printCExpr env mu with (env,mu) then
      match printCExpr env sigma with (env,sigma) then
        (env, strJoin ", " ["normal", mu, sigma])
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
      callback = callback,
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

    let init =
      match pStateTy with Some _ then "INIT_MODEL(progState_t)"
      else "INIT_MODEL_STACK()"
    in

    match printProgState indent env pStateTy with (env,pStateTy) in
    match mapAccumL (printCTop indent) env types with (env,types) in
    match mapAccumL (printCTop indent) env tops with (env,tops) in
    match printCStmts 2 env pre with (env,pre) in
    match
      match callback with Some callback then
        printCStmts 2 env callback
      else (env,"")
    with (env,callback) in
    match cPprintEnvGetStr env startBlock with (env,startBlock) in

    let pStateTy = match pStateTy with "" then [] else [pStateTy] in

    strJoin (pprintNewline indent) (join [
      includes,
      types,
      pStateTy,
      [ init ],
      tops,
      if null callback then [] else [
        "CALLBACK(callback, {",
        concat "  " callback,
        "})"
      ],
      [ "MAIN({" ],
      if null pre then [] else [concat "  " pre],
      [
        join ["  FIRST_BBLOCK(", startBlock ,");"],
        if null callback then
          "  SMC(NULL);"
        else
          "  SMC(callback);"
        ,
        "})"
      ]
    ])
end

-------------
--- TESTS ---
-------------

mexpr
use RootPPL in

let test = printRPProg [] in

let vardef = lam s.
  CSDef { ty = CTyInt {}, id = Some (nameSym s), init = None () } in

let startBlock = nameSym "startBlock" in
let basic = RPProg {
  includes = ["test.h"],
  startBlock = startBlock,
  pStateTy = Some (CTyInt {}),
  callback = None (),
  types = [ CTTyDef { ty = CTyInt {}, id = nameSym "newint" } ],
  tops = [
    CTBBlockDecl { id = startBlock },
    CTBBlock { id = startBlock, body = [ vardef "bvar" ] }
    ],
  pre = [ CSDef { ty = CTyInt {}, id = Some (nameSym "pvar"), init = None () } ]
} in

utest test basic with strJoin "\n" [
  "#include test.h",
  "typedef int newint;",
  "typedef int progState_t;",
  "INIT_MODEL(progState_t)",
  "BBLOCK_DECLARE(startBlock);",
  "BBLOCK(startBlock, {",
  "  int bvar;",
  "})",
  "MAIN({",
  "  int pvar;",
  "  FIRST_BBLOCK(startBlock);",
  "  SMC(NULL);",
  "})"
] using eqString in

let wrapTops = lam tops. RPProg {
  includes = [],
  startBlock = startBlock,
  pStateTy = None (),
  callback = None (),
  types = [],
  tops = tops,
  pre = []
} in

let data = wrapTops [
  CTBBlockData { ty = CTyInt {}, id = nameSym "data", len = 3 },
  CTBBlockDataSingle { ty = CTyInt {}, id = nameSym "data" },
  CTBBlock { id = startBlock, body = [] }
] in

utest test data with strJoin "\n" [
  "INIT_MODEL_STACK()",
  "BBLOCK_DATA_MANAGED(data, int, 3)",
  "BBLOCK_DATA_MANAGED_SINGLE(data1, int)",
  "BBLOCK(startBlock, {",
  "  ",
  "})",
  "MAIN({",
  "  FIRST_BBLOCK(startBlock);",
  "  SMC(NULL);",
  "})"
] using eqString in

let helperName = nameSym "helper" in
let helper = wrapTops [
  CTBBlockHelperDecl {
    ret = CTyVoid {},
    id = helperName,
    params = [CTyInt {}, CTyDouble {}]
  },
  CTBBlockHelper {
    ret = CTyVoid {},
    id = helperName,
    params = [ (CTyInt {}, nameSym "p"), (CTyDouble {}, nameSym "p") ],
    body = [ vardef "MAIN", vardef "DATA_POINTER" ] -- Check keyword name handling
  },
  CTBBlock { id = startBlock, body = [] }
] in

utest test helper with strJoin "\n" [
  "INIT_MODEL_STACK()",
  "BBLOCK_HELPER_DECLARE(helper, void, int, double);",
  "BBLOCK_HELPER(helper, {",
  "  int MAIN1;",
  "  int DATA_POINTER1;",
  "}, void, int p, double p1)",
  "BBLOCK(startBlock, {",
  "  ",
  "})",
  "MAIN({",
  "  FIRST_BBLOCK(startBlock);",
  "  SMC(NULL);",
  "})"
] using eqString in

let wrapExprs = lam exprs.
  wrapTops [ CTBBlock {
    id = startBlock, body = map (lam e. CSExpr { expr = e }) exprs
  } ]
in

let f = lam f. CEFloat { f = f } in
let i = lam i. CEInt { i = i } in
let dists = wrapExprs (join [
  map (lam dist. CESample { dist = dist }) [
    CDBern { p = f 1.0 },
    CDBeta { a = f 1.0, b = f 2.0 },
    CDBinomial { n = i 1, p = f 1.0 },
    CDExp { rate = f 1.0 },
    CDUniform { a = f 0.0, b = f 1.0 },
    CDPoisson { lambda = f 1.0 },
    CDGamma { k = f 1.0, theta = f 2.0 }
  ],

  map (lam t: (CExpr, CDist). CEObserve { value = t.0, dist = t.1 }) [
    (i 1, CDBern { p = f 1.0 }),
    (f 1.0, CDBeta { a = f 1.0, b = f 2.0 }),
    (i 1, CDBinomial { n = i 1, p = f 1.0 }),
    (f 1.0, CDExp { rate = f 1.0 }),
    (f 1.0, CDUniform { a = f 0.0, b = f 1.0 }),
    (i 1, CDPoisson { lambda = f 1.0 }),
    (f 1.0, CDGamma { k = f 1.0, theta = f 2.0 })
  ]
]) in

utest test dists with strJoin "\n" [
  "INIT_MODEL_STACK()",
  "BBLOCK(startBlock, {",
  "  (SAMPLE(bernoulli, 1.));",
  "  (SAMPLE(beta, 1., 2.));",
  "  (SAMPLE(binomial, 1., 1));",
  "  (SAMPLE(exponential, 1.));",
  "  (SAMPLE(uniform, 0., 1.));",
  "  (SAMPLE(poisson, 1.));",
  "  (SAMPLE(gamma, 1., 2.));",
  "  (OBSERVE(bernoulli, 1., 1));",
  "  (OBSERVE(beta, 1., 2., 1.));",
  "  (OBSERVE(binomial, 1., 1, 1));",
  "  (OBSERVE(exponential, 1., 1.));",
  "  (OBSERVE(uniform, 0., 1., 1.));",
  "  (OBSERVE(poisson, 1., 1));",
  "  (OBSERVE(gamma, 1., 2., 1.));",
  "})",
  "MAIN({",
  "  FIRST_BBLOCK(startBlock);",
  "  SMC(NULL);",
  "})"
] using eqString in

let exprs = wrapExprs [
  CEWeight { weight = CEFloat { f = 2.0 } },
  CEPState {},
  CENext {}
] in

utest test exprs with strJoin "\n" [
  "INIT_MODEL_STACK()",
  "BBLOCK(startBlock, {",
  "  (WEIGHT(2.));",
  "  PSTATE;",
  "  NEXT;",
  "})",
  "MAIN({",
  "  FIRST_BBLOCK(startBlock);",
  "  SMC(NULL);",
  "})"
] using eqString in

()
