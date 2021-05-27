-- RootPPL language fragment

include "option.mc"
include "c/ast.mc"
include "c/pprint.mc"

lang RootPPL = CAst + CPrettyPrint

  --------------------------
  -- AST (based on C AST) --
  --------------------------

  syn CStmt =
  -- TODO(dlunde,2021-05-25): Args as well to support BBLOCK_HELPER
  | CSBBlockCall { block: Name }

  sem smap_CStmt_CStmt (f: CStmt -> CStmt) =
  | CSBBlockCall _ & t -> t

  sem sfold_CStmt_CExpr (f: a -> CExpr -> a) (acc: a) =
  | CSBBlockCall _ & t -> acc

  sem smap_CStmt_CExpr (f: CExpr -> CExpr) =
  | CSBBlockCall _ & t -> t

  sem sreplace_CStmt_CStmt (f: CStmt -> [CStmt]) =
  | CSBBlockCall _ & t -> t

  syn CTop =

  -- Global data declarations
  | CTBBlockData { ty: CType, id: Name }

  -- Basic block helpers (essentially regular functions with special syntax)
  | CTBBlockHelperDecl { ty: CType, id: Name }
  | CTBBlockHelper { ret: CType, id: Name, params: [(CType,Name)], body: [Stmt] }

  -- Basic blocks
  | CTBBlockDecl { id : Name }
  | CTBBlock { id: Name, body: [Stmt] }

  sem smap_CTop_CExpr (f: CExpr -> CExpr) =
  | CTBBlock t -> CTBBlock { t with body = map (smap_CStmt_CExpr f) t.body }

  sem sreplace_CTop_CStmt (f: CStmt -> [CStmt]) =
  | CTBBlockData _ & t -> t
  | CTBBlockHelperDecl _ & t -> t
  | CTBBlockHelper t -> CTBBlockHelper { t with body = join (map f t.body) }
  | CTBBlockDecl _ & t -> t
  | CTBBlock t -> CTBBlock { t with body = join (map f t.body) }

  syn CExpr =
  | CEBBlockName { name: Name } -- Block names (will be replace by indices when printing)
  | CESample { dist: CDist }
  | CEWeight { weight: CExpr }
  | CEPState {}
  | CEPC {}

  sem sfold_CExpr_CExpr (f: a -> CExpr -> a) (acc: a) =
  | CEBBlockName _ -> acc
  | CESample t -> sfold_CDist_CExpr f acc t.dist
  | CEWeight t -> f acc t.weight
  | CEPState _ -> acc
  | CEPC _ -> acc

  sem smap_CExpr_CExpr (f: CExpr -> CExpr) =
  | CEBBlockName _ & t -> t
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
  | RPProg { includes: [String], pStateTy: CType, types: [CTop], tops: [CTop] }


  ---------------------
  -- PRETTY PRINTING --
  ---------------------

  sem printCStmt (indent: Int) (env: PprintEnv) =
  | CSBBlockCall { block = block } ->
    match pprintEnvGetStr env block with (env,block) then
      (env, join ["BBLOCK_CALL(", block, ")"])
    else never

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
  | CTBBlockDecl { id = id } ->
    match pprintEnvGetStr env id with (env,id) then
      (env, join [ "BBLOCK_DECLARE(", id, ");" ])
    else never

  sem printCExpr (env: PprintEnv) =
  | CEBBlockName { name = name } ->
    match pprintEnvGetStr env name with (env,name) then
      (env, join ["###", name, "###"])
    else never
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
  | RPProg { includes = includes, pStateTy = pStateTy, types = types, tops = tops } ->

    let blockNames: [Name] = foldl (lam acc. lam top.
      match top with CTBBlock { id = id } then snoc acc id else acc
    ) [] tops in

    let blockDecls = map (lam n. CTBBlockDecl { id = n } ) blockNames in

    recursive let replaceBBlockWithIndex = lam expr: CExpr.
      match expr with CEBBlockName { name = name } then
        match index (nameEq name) blockNames with Some i then CEInt { i = i }
        else error "Could not find block in replaceBBlockWithIndex"
      else smap_CExpr_CExpr replaceBBlockWithIndex expr
    in

    let tops = map (smap_CTop_CExpr replaceBBlockWithIndex) tops in

    let indent = 0 in
    let includes = map (lam inc. join ["#include ", inc]) includes in
    let addName = lam env. lam name.
      match pprintAddStr env name with Some env then env
      else error (join ["Duplicate name in printRPProg: ", nameGetStr name])
    in
    let env = foldl addName pprintEnvEmpty nameInit in
    let nameProgState = nameSym "progState_t" in
    let env = foldl addName env [nameProgState] in

    let progState = CTTyDef { ty = pStateTy, id = nameProgState } in

    match printCTop indent env progState with (env,progState) then
      match mapAccumL (printCTop indent) env types with (env,types) then
        match mapAccumL (printCTop indent) env blockDecls
        with (env,blockDecls) then
          match mapAccumL (printCTop indent) env tops with (env,tops) then
            match mapAccumL pprintEnvGetStr env blockNames
            with (env, blockNames) then
              strJoin (pprintNewline indent) (join [
                includes,
                types,
                [ progState
                , join [ "INIT_MODEL(progState_t,"
                       , int2string (length blockNames)
                       , ")"
                       ]
                ],
                blockDecls,
                tops,
                [ "MAIN({"
                , strJoin "\n"
                    (map (lam s. join ["  ADD_BBLOCK(", s ,");"]) blockNames)
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
