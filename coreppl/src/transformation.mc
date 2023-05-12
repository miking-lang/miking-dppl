include "digraph.mc"
include "name.mc"
include "coreppl.mc"
include "ext/math-ext.mc"
include "mexpr/anf.mc"

type Label = Int

lang PBN
  syn Vertex =
  | RandomVarNode {ident:Name,
                    val:Option Expr,
                    color:Int, -- 0:blue (assume), 1:red (stable)
                    dist:Expr,
                    list:Bool}
  | CodeBlockNode {ident:Name,
                    code:Expr,
                    ret:Bool,
                    list:Bool}
  | ListNode {ident:Name,
              items:[Name],
              dist:Option Expr}
  | MultiplexerNode {ident:Name,
                      index:Expr}

  sem cmprVertex (v1:Vertex) =
  | RandomVarNode v2 -> match v1 with RandomVarNode t then
                          nameCmp t.ident v2.ident
                        else match v1 with CodeBlockNode t then
                          nameCmp t.ident v2.ident
                        else match v1 with ListNode t then
                          nameCmp t.ident v2.ident
                        else match v1 with MultiplexerNode t then
                          nameCmp t.ident v2.ident
                        else negi 1

  | CodeBlockNode v2 -> match v1 with CodeBlockNode t then
                          nameCmp t.ident v2.ident
                        else match v1 with RandomVarNode t then
                          nameCmp t.ident v2.ident
                        else match v1 with ListNode t then
                          nameCmp t.ident v2.ident
                        else match v1 with MultiplexerNode t then
                          nameCmp t.ident v2.ident
                        else negi 1

  | ListNode v2 -> match v1 with ListNode t then
                    nameCmp t.ident v2.ident
                   else match v1 with CodeBlockNode t then
                     nameCmp t.ident v2.ident
                   else match v1 with RandomVarNode t then
                     nameCmp t.ident v2.ident
                   else match v1 with MultiplexerNode t then
                     nameCmp t.ident v2.ident
                   else negi 1

  | MultiplexerNode v2 -> match v1 with MultiplexerNode t then
                            nameCmp t.ident v2.ident
                          else match v1 with ListNode t then
                            nameCmp t.ident v2.ident
                          else match v1 with CodeBlockNode t then
                            nameCmp t.ident v2.ident
                          else match v1 with RandomVarNode t then
                            nameCmp t.ident v2.ident
                          else negi 1

  sem cmprEdge (e1:(Vertex,Vertex,Label)) =
  | (v1, v2, _) -> let cmprV1 = cmprVertex e1.0 v1 in
                   if eqi cmprV1 0 then cmprVertex e1.1 v2 else cmprV1
  | _ -> negi 1

  sem getId =
  | RandomVarNode v -> v.ident
  | CodeBlockNode v -> v.ident
  | MultiplexerNode v -> v.ident
  | ListNode v -> v.ident

  -- given a multiplexer m and a graph g, returns the list node that is an input to m
  sem inputMultiplexer (g:Digraph Vertex Label) =
  | MultiplexerNode m ->
    let parent = filter (lam v. match v with ListNode _ then true else false) (digraphPredeccessors (MultiplexerNode m) g) in
    get parent 0
  | _ -> error "inputMultiplexer:given vertex is not a multiplexer"

  -- given a multiplexer m and a graph g, returns Some index node to m if it exists otherwise None ()
  sem indexMultiplexer (g:Digraph Vertex Label) =
  | MultiplexerNode m ->
    let parent = filter (lam v. match v with ListNode _ then false else true) (digraphPredeccessors (MultiplexerNode m) g) in
    if null parent then None () else Some (get parent 0)
 | _ -> error "indexMultiplexer:given vertex is not a multiplexer"

end

-- for debug printing of vertices
recursive
let v2str = use PBN in
use MExprAst in
use MExprPPL in
  lam v.
  match v with CodeBlockNode c then
    let id = c.ident in let ret = if c.ret then " true" else " false" in
    join ["\ncodeb", id.0, (int2string (sym2hash id.1)),"\ncode:",expr2str c.code, "\n",ret, "\n"]
  else match v with RandomVarNode r then
    let id = r.ident in join ["randomv ", id.0 , " ",(int2string (sym2hash id.1)), "color:", if eqi r.color 0 then "blue" else if eqi r.color 1 then "red" else "green" ," dist " , (mexprToString r.dist)]
  else match v with ListNode l then
    let id = l.ident in join ["listnodev ", id.0 , " ",(int2string (sym2hash id.1)),
      foldl (lam acc. lam v. join [acc, " ", v.0 ,":",(int2string (sym2hash v.1)),"\t"]) "" l.items]
  else match v with MultiplexerNode m then
    let id = m.ident in join ["muxnode ", id.0 , " ",(int2string (sym2hash id.1)), mexprToString m.index]
    else never
end

lang ConjugatePrior = CorePPL + MExprAst + MExprPPL + PBN

  -- (d1:likelihood,d2:prior) checks whether d1 and d2 are conjugate
  sem isConjugatePrior =
  | (TmDist ({dist=DBernoulli d1}&t1),TmDist ({dist=DBeta d2}&t2)) -> true
  | (TmDist ({dist=DGaussian d1}&t1),TmDist ({dist=DGaussian d2}&t2)) -> true
  | (TmDist ({dist=DCategorical d1}&t1),TmDist ({dist=DDirichlet d2}&t2)) -> true
  | _ -> false

  -- check if two distributions family is equivalent
  sem eqFamilyDist =
  | (TmDist ({dist=DBernoulli _}&t1),TmDist ({dist=DBernoulli _}&t2) ) -> true
  | (TmDist ({dist=DBeta _}&t1),TmDist ({dist=DBeta _}&t2) ) -> true
  | (TmDist ({dist=DGaussian _}&t1),TmDist ({dist=DGaussian _}&t2) ) -> true
  | (TmDist ({dist=DCategorical _}&t1),TmDist ({dist=DCategorical _}&t2) ) -> true
  | (TmDist ({dist=DDirichlet _}&t1),TmDist ({dist=DDirichlet _}&t2) ) -> true
  | _ -> false

  -- check whether a list consists of rv s with same distribution
   sem validList (commonDist:Option Expr) =
  | [RandomVarNode t] ++ as -> match commonDist with Some dist1 then
                                match t.dist with dist2 then
                                  (if eqFamilyDist (dist1,dist2) then
                                    validList commonDist as
                                  else None ())
                                else never
                              else validList (Some t.dist) as
  | [] -> commonDist
  | [t] ++ as -> None ()

  -- given the likelihood,the prior and the observartion calculates the posterior
  -- (d1: likelihood, d2: prior)
  sem posterior (obs: Option Expr) (indices:Option (Expr,Expr))  =
  | (TmDist ({dist=DBernoulli d1}&t1),TmDist ({dist=DBeta d2}&t2)) ->
    let val = match obs with Some val then val else never in
    let postAlpha = if_ val (addf_ d2.a (float_ 1.)) d2.a in
    let postBeta = if_ val d2.b (addf_ d2.b (float_ 1.)) in
    let code =
      match indices with Some (mInd, lInd) then
        if_ (eqi_ mInd lInd) (utuple_ [postAlpha,postBeta]) (utuple_ [d2.a,d2.b])
      else (utuple_ [postAlpha,postBeta]) in
    let tName = nameSym "paramR" in
    let letT = nulet_ tName code in
    let rho = CodeBlockNode {ident=tName, code=letT, ret=false,list=false} in
    (rho, TmDist {t2 with dist=DBeta {{d2 with a=tupleproj_ 0 (nvar_ tName)} with b= tupleproj_ 1 (nvar_ tName)}})

  | (TmDist ({dist=DGaussian d1}&t1),TmDist ({dist=DGaussian d2}&t2) ) ->
    let val = match obs with Some val then val else never in
    let s02 = (mulf_ d2.sigma d2.sigma) in
    let s2 = (mulf_ d1.sigma d1.sigma) in
    let muRHS = addf_ (divf_ d2.mu s02) (divf_ val s2) in
    let muLHS = divf_ (float_ 1.0) (addf_ (divf_ (float_ 1.0) s02) (divf_ (float_ 1.0) s2)) in
    let postMu = mulf_ muRHS muLHS in
    let sigma = divf_ (float_ 1.0) (addf_ (divf_ (float_ 1.0) s02) (divf_ (float_ 1.0) s2)) in
    let postSigma = appf1_ (var_ "externalSqrt") sigma in
    let code =
      match indices with Some (mInd, lInd) then
        if_ (eqi_ mInd lInd) (utuple_ [postMu, postSigma]) (utuple_ [d2.mu,d2.sigma])
      else (utuple_ [postMu, postSigma]) in
    let tName = nameSym "paramR" in
    let letT = nulet_ tName code in
    let rho = CodeBlockNode {ident=tName, code=letT, ret=false,list=false} in
    (rho, TmDist {t2 with dist=DGaussian {{d2 with mu= tupleproj_ 0 (nvar_ tName)} with sigma= tupleproj_ 1 (nvar_ tName)}})

  | (TmDist ({dist=DCategorical d1}&t1),TmDist ({dist=DDirichlet d2}&t2)) ->
    let val = match obs with Some val then val else never in
    let postA = mapi_ ( ulam_ "i" (ulam_ "e" (if_ (eqi_ (var_ "i") val) (addf_ (var_ "e") (float_ 1.0)) (var_ "e")))) d2.a in
    let code =
      match indices with Some (mInd, lInd) then
        if_ (eqi_ mInd lInd) postA d2.a
      else postA in
    let tName = nameSym "paramR" in
    let letT = nulet_ tName code in
    let rho = CodeBlockNode {ident=tName, code=letT, ret=false,list=false} in
    (rho, TmDist {t2 with dist=DDirichlet {d2 with a=nvar_ tName}})

  | _ -> error "posterior:not supported"

  -- input (d1: likelihood, d2: prior)
  -- output (rho:Vertex, q:Expr)
  sem posteriorPredictive =
  | (TmDist ({dist=DBernoulli d1}&t1),TmDist ({dist=DBeta d2}&t2)) ->
    let postAlpha = d2.a in
    let postBeta = d2.b in
    let postP = divf_ postAlpha (addf_ postAlpha postBeta) in
    let tName = nameSym "param" in
    let letT = nulet_ tName postP in
    let rho = CodeBlockNode {ident=tName, code=letT, ret=false,list=false} in
    (rho,TmDist {t1 with dist=DBernoulli {d1 with p= nvar_ tName}},CodeBlockNode {ident=nameSym "", code=postP, ret=false,list=true})

  | (TmDist ({dist=DGaussian d1}&t1),TmDist ({dist=DGaussian d2}&t2) ) ->
    let s02 = (mulf_ d2.sigma d2.sigma) in
    let s2 = (mulf_ d1.sigma d1.sigma) in
    let postMu = mulf_ s02 (divf_ d2.mu s02) in
    let postSigma = appf1_ (var_ "externalSqrt") (addf_ s02 s2) in
    let tName = nameSym "param" in
    let letT = nulet_ tName (utuple_ [postMu, postSigma]) in
    let rho = CodeBlockNode {ident=tName, code=letT, ret=false,list=false} in
    (rho, TmDist {t1 with dist=DGaussian {{d1 with mu= tupleproj_ 0 (nvar_ tName)} with sigma= tupleproj_ 1 (nvar_ tName)}},CodeBlockNode {ident=nameSym "", code=(utuple_ [postMu, postSigma]) ,ret=false,list=true})

  | (TmDist ({dist=DCategorical d1}&t1),TmDist ({dist=DDirichlet d2}&t2)) ->
    let sumai = foldl_ (ulam_ "acc" (ulam_ "i" (addf_ (var_ "acc") (var_ "i")))) (float_ 0.0) (d2.a) in
    let postA = map_ (ulam_ "ai" (divf_ (var_ "ai") sumai)) d2.a in
    let tName = nameSym "param" in
    let letT = nulet_ tName postA in
    let rho = CodeBlockNode {ident=tName, code=letT, ret=false,list=false} in
    (rho, TmDist {t1 with dist=DCategorical {d1 with p=nvar_ tName}},CodeBlockNode {ident=nameSym "", code=postA,ret=false,list=true})
  | _ -> error "posteriorPredictive:not supported"

  sem listParam (cbId:Name)=
  | TmDist ({dist=DBernoulli d1}&t1) ->
    TmDist {t1 with dist=DBernoulli {d1 with p= nvar_ cbId}}

  | TmDist ({dist=DGaussian d1}&t1)->
    TmDist {t1 with dist=DGaussian {{d1 with mu= tupleproj_ 0 (nvar_ cbId)} with sigma= tupleproj_ 0 (nvar_ cbId)}}
  | TmDist ({dist=DCategorical d1}&t1)->
    TmDist {t1 with dist=DCategorical {d1 with p=nvar_ cbId}}

  | TmDist ({dist=DBeta d1}&t1) ->
    TmDist {t1 with dist=DBeta {{d1 with a= tupleproj_ 0 (nvar_ cbId)} with b = tupleproj_ 1 (nvar_ cbId)}}

  | TmDist ({dist=DDirichlet d1}&t1)->
    TmDist {t1 with dist=DDirichlet {d1 with a=nvar_ cbId}}
  | _ -> error "listParam:not supported"
end

-- Language fragment to create PBN from a given program
lang StaticAnalyzer = PBN + MExprAst + MExprPPL + ConjugatePrior

  -- create edges based on the dependencies of vertex v
  sem createEdges (v:Vertex) (g:Digraph Vertex Label) (m1:Map Name Vertex) (m2:Map Name Name) (edges:Set (Vertex,Vertex,Label)) =
  | TmVar t ->
  -- find the corresponding vertex ident from the variable ident
  match mapLookup t.ident m2 with Some vertexId then
    let vFrom = mapLookupOrElse (lam. error "Lookup failed") vertexId m1 in
    -- create an edge to the source vertex from the vertex that it depends on
    if digraphEqv g vFrom v then edges --check if they are in the same codeblock if so no need to create an edge
    else setInsert (vFrom, v, 0) edges
  else edges -- if cannot find id then it must be created with lambda scoping so ignore
  | t -> sfold_Expr_Expr (createEdges v g m1 m2) edges t

  --
  sem findRandomVariables (m:Map Name Vertex) (idents:Set Name) =
  | TmVar t -> match mapLookup t.ident m with Some (RandomVarNode _) then setInsert t.ident idents else idents
  | t -> sfold_Expr_Expr (findRandomVariables m) idents t

  --
  sem findTargets (m:Map Name Vertex) (targets:Set Name) =
  | TmVar t -> targets -- if the random variable directly used
  | t-> findRandomVariables m targets t

  sem createVertex (ident:Option Name) (g:Digraph Vertex Label) (m1:Map Name Vertex) (m2:Map Name Name) (env:Map Name Expr) =
  | TmAssume t -> let id = match ident with Some id then id else nameSym "" in
                  let v = RandomVarNode {ident=id,val = None(),color=0,dist= t.dist,list=true} in
                  let g = digraphAddVertex v g in
                  let edges = setToSeq (createEdges v g m1 m2 (setEmpty cmprEdge) (TmAssume t)) in
                  let g = digraphAddEdges edges g in
                  ((g, mapInsert id v m1, mapInsert id id m2, mapInsert id (TmAssume t) env),v)
  | TmObserve t -> let id = match ident with Some id then id else nameSym "" in
                  let v = RandomVarNode {ident=id,val =  Some (t.value),color=0,dist= t.dist,list=true} in
                  let g = digraphAddVertex v g in
                  let edges = setToSeq (createEdges v g m1 m2 (setEmpty cmprEdge) (TmObserve t)) in
                  let g = digraphAddEdges edges g in
                  ((g, mapInsert id v m1, mapInsert id id m2, mapInsert id (TmObserve t) env),v)
  | TmSeq t -> let id = match ident with Some id then id else nameSym "" in
               let res = mapAccumL (lam acc. lam e. createVertex (None ()) acc.0 acc.1 acc.2 acc.3 e) (g,m1,m2,env) t.tms in
               let ids = map getId res.1 in
               let dist = validList (None ()) res.1 in
               let v = ListNode {ident=id, items=ids,dist=dist} in
               let acc = res.0 in
               let g = acc.0 in
               let m1 = acc.1 in
               let m2 = acc.2 in
               let env = acc.3 in
               let g = digraphAddVertex v g in
               let edges = setToSeq (createEdges v g m1 m2 (setEmpty cmprEdge) (TmSeq t)) in
               let g = digraphAddEdges edges g in
               ((g, mapInsert id v m1, mapInsert id id m2, mapInsert id (TmSeq t) env),v)
  | t -> let id = match ident with Some id then id else nameSym "" in
         let v = CodeBlockNode {ident=id, code=t,ret=false,list=true} in
         let g = digraphAddVertex v g in
         let edges = setToSeq (createEdges v g m1 m2 (setEmpty cmprEdge) t) in
         let g = digraphAddEdges edges g in
         ((g, mapInsert id v m1, mapInsert id id m2, mapInsert id t env),v)


  -- m1: a mapping from a vertex ident to a corresponding vertex
  -- m2: a mapping from a variable name to its corresponding vertex id. Several let bindings can corresspond to a single code block vertex
  sem createPBN (g:Digraph Vertex Label) (targets:Set Name) (m1:Map Name Vertex) (m2:Map Name Name) (env:Map Name Expr) (blockIdent:Option Name) =
    | TmLet ({body=TmAssume ({dist=TmDist d}&a)}&t) ->
    let v = RandomVarNode {ident = t.ident, val = None (), color = 0, dist = a.dist,list=false} in
    let g = digraphAddVertex v g in
    -- create edges due to dependency
    let edges = setToSeq (createEdges v g m1 m2 (setEmpty cmprEdge) t.body) in
    let g = digraphAddEdges edges g in
    -- check if the value of any random variable is needed on the parameters of the distribution
    let targets = sfold_Expr_Expr (findTargets m1) targets (TmDist d) in
    let m1 = mapInsert t.ident v m1 in
    let m2 = mapInsert t.ident t.ident m2 in
    let env = mapInsert t.ident t.body env in
    createPBN g targets m1 m2 env (None ()) t.inexpr

  | TmLet ({body=TmObserve ({dist=TmDist d}&o)}&t) ->
    let v = RandomVarNode {ident = t.ident, val = Some (o.value), color = 0, dist = o.dist,list=false} in
    let g = digraphAddVertex v g in
    let edges = setToSeq (createEdges v g m1 m2 (setEmpty cmprEdge) t.body) in
    let g = digraphAddEdges edges g in
    -- check if the value of any random variable is needed on the parameters of the distribution
    let targets = sfold_Expr_Expr (findTargets m1) targets (TmDist d) in
    -- check if the value of any random variable is needed on the observed value, e.g. observe observedValue (Gaussian a b)
    let targets = findRandomVariables m1 targets o.value in
    let targets = setInsert t.ident targets in -- observe node is a target node since it changes the weight
    let m1 = mapInsert t.ident v m1 in
    let m2 = mapInsert t.ident t.ident m2 in
    let env = mapInsert t.ident t.body env in
    createPBN g targets m1 m2 env (None ()) t.inexpr

  | TmLet ({body=TmSeq s}&t) ->
    let res = createVertex (Some t.ident) g m1 m2 env (TmSeq s) in
    let acc = res.0 in
    let v = res.1 in
    let g = acc.0 in
    let m1 = acc.1 in
    let m2 = acc.2 in
    let env = acc.3 in
    createPBN g targets m1 m2 env (None ()) t.inexpr

  | TmLet ({body=TmApp ({lhs=(TmApp ({lhs=TmConst ({val=CGet ()}&c),rhs=TmVar seq})&t2),rhs=index}&a)}&t) ->
    let trgt = get_ (TmVar seq) index in
    let seqV = mapLookupOrElse (lam. error "Lookup failed") seq.ident m1 in
    let m = MultiplexerNode {ident=t.ident,index=index} in
    let g = digraphAddVertex m g in
    let m1 = mapInsert t.ident m m1 in
    let m2 = mapInsert t.ident t.ident m2 in
    let edges = setToSeq (createEdges m g m1 m2 (setEmpty cmprEdge) t.body) in
    let g = digraphAddEdges edges g in
    let targets = findRandomVariables m1 targets index in
    createPBN g targets m1 m2 env (None ()) t.inexpr
/- not supported yet
  | TmLet ({body=TmApp ({lhs=(TmApp ({lhs=TmConst ({val=CIter()}&c),rhs=lambda})&a1),rhs=lst}&a2)}&t) ->  print "gothca";  createPBN g targets m1 m2 env (None ()) t.inexpr
  | TmLet t ->
    let v =
      match blockIdent with Some blockIdent then -- we can also put this to the previous code block
        -- find the codeblock that this expression should be added
        let vertex = mapLookupOrElse (lam. error "Lookup failed") blockIdent m1 in
        match vertex with CodeBlockNode c then
          CodeBlockNode {c with code = bind_ c.code (TmLet {t with inexpr = unit_})} -- bind it to previous code
        else never
      else -- we need to create a new code block
        CodeBlockNode {ident = nameSym "", code=TmLet {t with inexpr = unit_},ret=false,list=false} --create a code block with new ident which is not a return statement
    in
-- depending on whether we created a new node or updated the existing one add/update the node on the graph.
    let g = digraphAddUpdateVertex v g in

    -- update the mapping from the vertex ident to the vertex
    let blockIdent = match v with CodeBlockNode c then c.ident else never in
    let m1 = mapInsert blockIdent v m1 in

    --create the edges
    let edges = setToSeq (createEdges v g m1 m2 (setEmpty cmprEdge) t.body) in
    let g = digraphMaybeAddEdges edges g in

    -- create the targets
    let targets = findRandomVariables m1 targets t.body in
    createPBN g targets m1 (mapInsert t.ident blockIdent m2) (mapInsert t.ident t.body env) (Some blockIdent) t.inexpr
 -/
  | TmRecLets t ->
    -- for each binding add the ident to the map
    let v =
      match blockIdent with Some blockIdent then -- we can also put this to the previous code block
        -- find the codeblock that this expression should be added
        let vertex = mapLookupOrElse (lam. error "Lookup failed") blockIdent m1 in
        match vertex with CodeBlockNode c then
          CodeBlockNode {c with code = (bind_ c.code (TmRecLets {t with inexpr = unit_}))} -- add it to sequence of code
        else never
      else -- we need to create a new code block
        CodeBlockNode {ident = nameSym "", code=TmRecLets {t with inexpr = unit_}, ret=false,list=false} --create a code block with new ident which is not a return statement
    in
    let g = digraphAddUpdateVertex v g in
    let blockIdent = match v with CodeBlockNode c then c.ident else never in

    let res = foldl (lam acc. lam b.
      let edges = setToSeq (createEdges v g m1 m2 (setEmpty cmprEdge) b.body) in
      let g = digraphMaybeAddEdges edges g in

      -- create the targets
      let targets = findRandomVariables m1 targets b.body in
      (g,mapInsert b.ident v acc.1, mapInsert b.ident blockIdent acc.2, targets)
      ) (g,m1,m2,targets) t.bindings
    in
    createPBN res.0 res.3 res.1 res.2 env (Some blockIdent) t.inexpr
  | TmExt t ->
    let v =
      match blockIdent with Some blockIdent then -- we can also put this to the previous code block
        -- find the codeblock that this expression should be added
        let vertex = mapLookupOrElse (lam. error "Lookup failed") blockIdent m1 in
        match vertex with CodeBlockNode c then
          CodeBlockNode {c with code = (bind_ c.code (TmExt {t with inexpr = unit_}))} -- add it to sequence of code
        else never
      else -- we need to create a new code block
        CodeBlockNode {ident = nameSym "", code=TmExt {t with inexpr = unit_}, ret=false,list=false} --create a code block with new ident which is not a return statement
    in
    -- update the mapping from the vertex ident to the vertex
    let blockIdent = match v with CodeBlockNode c then c.ident else never in
    let m1 = mapInsert blockIdent v m1 in
    let g = digraphAddUpdateVertex v g in

    createPBN g targets (mapInsert t.ident v m1) (mapInsert t.ident blockIdent m2) env (Some blockIdent) t.inexpr
  | TmType t ->
  let v =
      match blockIdent with Some blockIdent then -- we can also put this to the previous code block
        -- find the codeblock that this expression should be added
        let vertex = mapLookupOrElse (lam. error "Lookup failed") blockIdent m1 in
        match vertex with CodeBlockNode c then
          CodeBlockNode {c with code = (bind_ c.code (TmType {t with inexpr = unit_}))} -- add it to sequence of code
        else never
      else -- we need to create a new code block
        CodeBlockNode {ident = nameSym "", code=TmType {t with inexpr = unit_}, ret=false,list=false} --create a code block with new ident which is not a return statement
    in
     -- update the mapping from the vertex ident to the vertex
    let blockIdent = match v with CodeBlockNode c then c.ident else never in
    let m1 = mapInsert blockIdent v m1 in
    let g = digraphAddUpdateVertex v g in
    createPBN g targets (mapInsert t.ident v m1) (mapInsert t.ident blockIdent m2) env (Some blockIdent) t.inexpr

  | TmConDef t ->
  let v =
      match blockIdent with Some blockIdent then -- we can also put this to the previous code block
        -- find the codeblock that this expression should be added
        let vertex = mapLookupOrElse (lam. error "Lookup failed") blockIdent m1 in
        match vertex with CodeBlockNode c then
          CodeBlockNode {c with code = (bind_ c.code (TmConDef {t with inexpr = unit_}))} -- add it to sequence of code
        else never
      else -- we need to create a new code block
        CodeBlockNode {ident = nameSym "", code=TmConDef {t with inexpr = unit_}, ret=false,list=false} --create a code block with new ident which is not a return statement
    in
     -- update the mapping from the vertex ident to the vertex
    let blockIdent = match v with CodeBlockNode c then c.ident else never in
    let m1 = mapInsert blockIdent v m1 in
    let g = digraphAddUpdateVertex v g in
    createPBN g targets (mapInsert t.ident v m1) (mapInsert t.ident blockIdent m2) env (Some blockIdent) t.inexpr

  | t ->
    let id = nameSym "" in
    let v = CodeBlockNode {ident = id, code =t, ret=true,list=false} in
    -- create the targets
    let targets = findRandomVariables m1 targets t in
    let g = digraphAddVertex v g in
    (g, mapInsert id v m1, targets)

   sem recreateLVertex =
   | CodeBlockNode t -> t.code
   | RandomVarNode v -> match v.val with Some val then -- observe
                          TmObserve {dist=v.dist, value=val,ty=tyunknown_, info = NoInfo ()}
                        else
                         (TmAssume {dist=v.dist, ty=tyunknown_, info = NoInfo ()})

   sem recreateVertex (vRet:Option Vertex) (g:Digraph Vertex Label) (m:Map Name Vertex) =
  | [CodeBlockNode t] ++ as -> if t.list then recreateVertex vRet g m as else bind_ t.code (recreateVertex vRet g m as)
  | [RandomVarNode v] ++ as ->
                        if v.list then recreateVertex vRet g m as
                        else
                         match v.val with Some val then -- observe
                          TmLet { ident = v.ident,
                                tyBody = tyunknown_,
                                body = (TmObserve {dist=v.dist, value=val,ty=tyunknown_, info = NoInfo ()}),
                                inexpr=(recreateVertex vRet g m as),
                                ty=tyunknown_,
                                info = NoInfo (),
                                tyAnnot = tyunknown_}
                        else
                          TmLet { ident = v.ident,
                                tyBody = tyunknown_,
                                body = (TmAssume {dist=v.dist, ty=tyunknown_, info = NoInfo ()}),
                                inexpr=(recreateVertex vRet g m as),
                                ty=tyunknown_,
                                info= NoInfo (),
                                tyAnnot = tyunknown_}
  | [ListNode l] ++ as -> let vItems = map (lam i. mapLookupOrElse (lam. error "Recreate:Lookup failed") i m) l.items in
                          TmLet { ident = l.ident,
                                  tyBody = tyunknown_,
                                  body = (TmSeq {tms=(map recreateLVertex vItems), ty=tyunknown_,info=NoInfo ()}),
                                  inexpr =(recreateVertex vRet g m as),
                                  ty = tyunknown_,
                                  info = NoInfo (),
                                  tyAnnot = tyunknown_}
  | [MultiplexerNode mu] ++ as ->  let listnode = inputMultiplexer g (MultiplexerNode mu) in
                                    match listnode with ListNode l then
                                      TmLet { ident = mu.ident,
                                        tyBody = tyunknown_,
                                        body = get_ (nvar_ l.ident) mu.index,
                                        inexpr=(recreateVertex vRet g m as),
                                        ty=tyunknown_,
                                        info= NoInfo (),
                                        tyAnnot = tyunknown_}
                                    else never
                                   -- else recreateVertex vRet g m as
  | [] -> match vRet with Some (CodeBlockNode c) then c.code else error "no return"

end

lang PBNTransformer = StaticAnalyzer + ConjugatePrior end
let debug = false

let modifiedBFS : all v. all l. v -> v -> Digraph v l -> Bool
  = lam source. lam dst. lam g.
  recursive let work = lam fs. lam level. lam dist:Map v Int. lam u.
    if null fs then u else
    match
      foldl (lam acc:([v], Map v Int,Bool). lam f.
        foldl (lam acc:([v], Map v Int,Bool). lam v.
          if mapMem v acc.1 then
            if digraphEqv g dst v then
              (acc.0,acc.1, false)
            else acc
          else (cons v acc.0, mapInsert v level acc.1,u)
        ) acc (digraphSuccessors f g)) ([],dist,u) fs
      with (ns, dist, u) then
        if not u then u
        else
          work ns (addi level 1) dist u
      else never
    in
    work [source] 1 (mapInsert source 0 (mapEmpty (digraphCmpv g))) true

-- create parameters during conditioning
let createRParameter =  use PBNTransformer in
  lam t:Vertex. lam p:Vertex. lam qDist:Map Name Expr. lam g:Digraph Vertex Label. lam m:Map Name Vertex. lam index:Option (Expr,Expr).
  match t with RandomVarNode v1 then
    match p with RandomVarNode v2 then
        let obs = match v1.val with Some obs then obs else (nvar_ v1.ident) in
        let pMarginalizedDist = mapLookupOrElse (lam. error "CreateRParameter:Lookup failed") v2.ident qDist in
        let res = posterior (Some obs) index (v1.dist,pMarginalizedDist) in
        let rho = res.0 in
        let updatedD = res.1 in
        let g = digraphAddVertex rho g in
        -- add the created cb to the id vertex list
        let id = match rho with CodeBlockNode r then r.ident else never in
        let m = mapInsert id rho m in
        -- inherit parents
        -- get the codeblock parents and stabilized nodes of t
        let parentsT = filter (lam v. match v with CodeBlockNode _ then true
                                  else match v with RandomVarNode r then
                                    eqi r.color 1
                                  else false) (digraphPredeccessors t g) in
        -- get the codeblock parents and stabilized nodes of p
        let parentsP = filter (lam v. match v with CodeBlockNode _ then true
                                  else match v with RandomVarNode r then
                                    eqi r.color 1
                                  else false) (digraphPredeccessors p g) in
        -- inherit the dependencies
        let g = foldl (lam acc. lam gp. digraphMaybeAddEdge gp rho 0 acc) g parentsT in
        let g = foldl (lam acc. lam gp. let g = digraphRemoveEdge gp p 0 acc in digraphMaybeAddEdge gp rho 0 g) g parentsP in
        (rho,updatedD,g,m)
    else never
  else never

-- create parameters during marginalization
let createMParameter = use PBNTransformer in
  lam t:Vertex. lam p:Vertex. lam qDist:Map Name Expr. lam g:Digraph Vertex Label. lam m:Map Name Vertex. lam list:Bool.
  match t with RandomVarNode v1 then
    match p with RandomVarNode v2 then
        let pMarginalizedDist = mapLookupOrElse (lam. error "Marginalize:Lookup failed") v2.ident qDist in
        let res = posteriorPredictive (v1.dist, pMarginalizedDist) in -- returns parameter codeblock, and updated distribution
        let rho = if list then res.2 else res.0 in
        let q = res.1 in
        let g = digraphAddVertex rho g in
        -- add the created cb to the id vertex list
        let id = match rho with CodeBlockNode r then r.ident else never in
        let m = mapInsert id rho m in
        -- get the codeblock parents and stabilized nodes of t
        let parentsT = filter (lam v. match v with CodeBlockNode _ then true
                                  else match v with RandomVarNode r then
                                    eqi r.color 1
                                  else false) (digraphPredeccessors t g) in
          -- get the codeblock parents and stabilized nodes of p
        let parentsP = filter (lam v. match v with CodeBlockNode _ then true
                                  else match v with RandomVarNode r then
                                    eqi r.color 1
                                  else false) (digraphPredeccessors p g) in
          -- inherit the dependencies
        let g = foldl (lam acc. lam gp. digraphMaybeAddEdge gp rho 0 acc) g parentsT in
        let g = foldl (lam acc. lam gp. digraphMaybeAddEdge gp rho 0 acc) g parentsP in
        (rho,q,g,m)
    else never
  else never


recursive
let reorder = use PBNTransformer in
  lam qDist:Map Name Expr. lam g:Digraph Vertex Label. lam m:Map Name Vertex. lam t:Vertex.
  (if debug then print (join ["Reorder ", v2str t, "\n"]) else ());
  match t with RandomVarNode v then
    if eqi v.color 1 then (qDist,g,m) else
    let children = digraphSuccessors t g in
    let parents = filter (lam p. match p with RandomVarNode p then eqi p.color 0 else
                                  match p with MultiplexerNode m then
                                    let lst = inputMultiplexer g p in
                                    match lst with ListNode l then
                                      match l.dist with Some _ then true else false
                                    else never --lst should be a ListNode
                                  else false) (digraphPredeccessors t g) in
    if null parents then
      (if debug then print ("Random variable has no parents so directly stabilize") else ());
      let stabilizedT = RandomVarNode {{v with color=1} with dist=mapLookupOrElse (lam. error "Reorder:Lookup failed") v.ident qDist} in
      let qDist = mapRemove v.ident qDist in
      -- add update vertex may not be working
      let g = digraphAddUpdateVertex stabilizedT g in
      let m = mapRemove v.ident m in
      let m = mapInsert v.ident stabilizedT m in
      (qDist, g, m)
    else -- if it has a parent that is either a marginalized rv or a list
      let rvParents = filter (lam p. match p with RandomVarNode _ then true else false) parents in
      let muxParents = filter (lam p. match p with MultiplexerNode _ then true else false) parents in
      if null rvParents then
        let muxParent = get muxParents 0 in
        match muxParent with MultiplexerNode mux then
          match inputMultiplexer g muxParent with ListNode l then
            let g = digraphRemoveEdge muxParent t 0 g in
            let res = foldl (lam acc. lam iid.
              let e = mapLookupOrElse (lam. error "Reorder:Lookup failed") iid acc.2 in
              let res = createRParameter t e acc.0 acc.1 acc.2 (Some (mux.index, int_ acc.3)) in
              let rho = res.0 in
              let updatedD = res.1 in
              let g = res.2 in
              let m = res.3 in
              let marginalizedT = match e with RandomVarNode e then
              RandomVarNode {e with dist=updatedD} else never in
              let g= digraphAddUpdateVertex marginalizedT g in
              let g = digraphAddEdge t rho 0 g in
              let g = digraphAddEdge rho marginalizedT 0 g in
              let g = match indexMultiplexer g muxParent with Some ind then
                    digraphAddEdge ind rho 0 g else g in
              let qDist = mapInsert iid updatedD acc.0 in
              let m = mapInsert iid marginalizedT m in
              (qDist,g,m,addi acc.3 1)) (qDist,g,m,0) l.items in
            let qDist = res.0 in
            let g = res.1 in
            let m = res.2 in
            let stabilizedT = RandomVarNode {{v with color=1} with
                        dist=mapLookupOrElse (lam. error "Reorder:Lookup failed") v.ident qDist} in
            let g = digraphAddUpdateVertex stabilizedT g in
            let m = mapInsert v.ident stabilizedT m in
            let qDist = mapRemove v.ident qDist in
            (qDist, g, m)
          else never
        else never
      else
        let rvParent = get rvParents 0 in
          (if debug then print (join ["Random variable a parent:" ,(v2str rvParent),"\n"]) else ());
        match rvParent with RandomVarNode p then
          if not (modifiedBFS rvParent t g) then
            (if debug then print "Graft: can cause cycles reordering the parent\n" else ());
              reorder qDist g m rvParent
          else
          let res = createRParameter t rvParent qDist g m (None ()) in
          let rho = res.0 in
          let updatedD = res.1 in
          let g = res.2 in
          let m = res.3 in
          let stabilizedT = RandomVarNode {{v with color=1} with dist=mapLookupOrElse (lam. error "Reorder:Lookup failed") v.ident qDist} in
          let qDist = mapRemove v.ident qDist in
          let marginalizedP = RandomVarNode {p with dist=updatedD} in
          let m = mapInsert p.ident marginalizedP m in
          let g = digraphRemoveEdge rvParent t 0 g in
          let g = digraphAddEdge rho rvParent 0 g in
          let g = digraphAddUpdateVertex stabilizedT g in
          let g = digraphAddEdge stabilizedT rho 0 g in
          let m = mapInsert v.ident stabilizedT m in
          let qDist = mapInsert p.ident updatedD qDist in
          (qDist,g,m)
    else never -- rvParent should be a RandomVarNode
  else never -- t should be a rv

let marginalize = use PBNTransformer in
  lam qDist:Map Name Expr. lam g:Digraph Vertex Label. lam m:Map Name Vertex. lam t:Vertex.
  -- t should be a random variable node to be marginalized
  match t with RandomVarNode v then
    (if debug then print (join ["Marginalize ", v2str t, "\n"]) else ());
    -- get its parents
    let parents = digraphPredeccessors t g in
    -- filter its random variable parents that are not stabilized
    let parents = filter (lam p. match p with RandomVarNode p then (if eqi p.color 0 then true else false) else
                                  match p with MultiplexerNode m then
                                    (match inputMultiplexer g p with ListNode l then true else never)
                                  else false) parents in
    -- if it has no random variable parent or a list with rv parents then only add its distribution to the marginalized distribution list
    (if null parents then
      match t with RandomVarNode v then
          (mapInsert v.ident v.dist qDist, g, m) -- q(t) <- d(t)
      else never
    -- if it has a random variable parent or a list pr with rvs
    else (
      if gti (length parents) 1 then error "Marginalize:This should have been handled in Graft"
      else
      let parent = get parents 0 in
      match (t, parent) with (RandomVarNode v, RandomVarNode p) then
          -- find the marginalized distribution for the parent
          let pMarginalizedDist = mapLookupOrElse (lam. error "Marginalize:Lookup failed") p.ident qDist in
        -- check if there is a conjugate prior relation between the parent and the t
        (if (isConjugatePrior (v.dist, pMarginalizedDist)) then
          -- if so create the parameters
          let res = createMParameter t parent qDist g m false in
          let rho = res.0 in
          let q = res.1 in
          let g = res.2 in
          -- insert the marginalized dist
          let qDist = mapInsert v.ident q qDist in
          -- add an edge from rho to t
          let g = digraphAddEdge rho t 0 g in
          -- return the updated dist
          (qDist,g,m)
          -- if there is no conjugate prior relation then reorder the parent
          else
            (if debug then print "Marginalize: no conjugate prior rel\n" else ());
            let res = reorder qDist g m parent in
            (mapInsert v.ident v.dist res.0, res.1, res.2))
      else match (t,parent) with (RandomVarNode v, MultiplexerNode p) then
        let l = inputMultiplexer g parent in
        (match l with ListNode l then
          let conjAll = foldl (lam acc. lam i.
                  let i = mapLookupOrElse (lam. error "Marginalize:Lookup failed") i m in
                  match i with RandomVarNode r then
                    -- get the marginalized dist for the current parent rv.
                    let pMarginalizedDist = mapLookupOrElse (lam. error "Marginalize:Lookup failed") r.ident qDist in
                    and acc (isConjugatePrior (v.dist, pMarginalizedDist))
                  else never) true l.items in
          (if conjAll then
              let res = foldl (lam acc. lam i.
              let i = mapLookupOrElse (lam. error "Marginalize:Lookup failed") i m in
              match i with RandomVarNode r then
                let res = createMParameter t i acc.0 acc.1 acc.2 true in
                let q = res.1 in
                let item = getId res.0 in
                let cbl = snoc acc.3 item in
                let ql = cons q acc.4 in
                (acc.0,res.2,res.3,cbl,ql)
              else never) (qDist,g,m,[],[]) l.items in
            let g = res.1 in
            let qDist = res.0 in
            let m = res.2 in
            let lid = nameSym "params" in
            let parameterList = ListNode {ident=lid,items=res.3,dist=None ()} in
            let m = mapInsert lid parameterList m in
            let g = digraphAddVertex parameterList g in
            let muxid = nameSym "muxParam" in
            let muxList = MultiplexerNode {ident=muxid, index=p.index} in
            let m = mapInsert muxid muxList m in
            let g = digraphAddVertex muxList g in
            let g = match indexMultiplexer g parent with Some i then
              digraphAddEdge i muxList 0 g else g in
            let g = digraphAddEdge parameterList muxList 0 g in
            let g = digraphAddEdge muxList t 0 g in
            let q = listParam muxid (get res.4 0) in
            let qDist = mapInsert v.ident q qDist in
            (qDist,g,m)
          else -- there is at least one does not have cp relation so reorder all
            let res = foldl (lam acc. lam i.
                  let i = mapLookupOrElse (lam. error "Marginalize:Lookup failed") i m in
                  reorder acc.0 acc.1 acc.2 i) (qDist,g,m) l.items in
            (mapInsert v.ident v.dist res.0, res.1, res.2))
        else never)
    else never
    ))
  else never
end

-- DONE --
recursive
let prune = use PBNTransformer in
  lam qDist:Map Name Expr. lam g:Digraph Vertex Label. lam m:Map Name Vertex. lam t:Vertex.
  match t with RandomVarNode v then
    -- pruned RV should already be marginalized
    (match mapMem v.ident qDist with false then error "Prune: t is not marginalized"
    else
      -- get its marginalized child
      let child = filter (lam u. match u with RandomVarNode u then mapMem u.ident qDist else false) (digraphSuccessors t g) in
      -- if it does not have a marginalized child then reorder it.
      (if null child then reorder qDist g m t
      else
        match eqi (length child) 1 with false then error "Prune: t has more than one marginalized child" else
          -- if it has a marginalized child then prune it first.
          let res = prune qDist g m (get child 0) in
          reorder res.0 res.1 res.2 t))
  else error "Prune: only random variables can be pruned."
end


-- DONE --
recursive
let graft = use PBNTransformer in
  lam qDist:Map Name Expr. lam g:Digraph Vertex Label. lam m:Map Name Vertex. lam t:Vertex.
  -- if t is not a random variable then do not change
  match t with RandomVarNode v then
    (if debug then print (join ["Graft(", v2str t,")\n"]) else ());
    -- if t is marginalized
    if mapMem v.ident qDist then
      -- get its children
      let children = digraphSuccessors t g in
      -- get its marginalized random variable child if any
      let child = filter (lam u. match u with RandomVarNode u then mapMem u.ident qDist else false) children in
      -- if it does not have a marginalized child, then do nothing and return the graph
      (if null child then (qDist,g,m)
      else
        -- if it has more than one marginalized child, then there is something wrong
        (if not (eqi (length child) 1) then error "Graft: can only have one marginalized child"
         else -- if it has one marginalized child
           let child = get child 0 in -- get that child
          (if debug then print (join ["child node ", (v2str child), " to be pruned\n"]) else ());
           -- prune the child so t will become the terminal node on its marginalized path
          prune qDist g m child))
    -- if t is not marginalized
    else
      (if debug then print "Graft: RV t is not marginalized\n" else ());
      -- get its parents
      let parents = (digraphPredeccessors t g) in
      -- get its random variable or list parents
      let parent = filter (lam v. match v with RandomVarNode v then eqi v.color 0 else
                            match v with MultiplexerNode m then
                              match inputMultiplexer g v with ListNode l then true
                              else false
                            else false) parents in
      -- if it does not have a random variable parent or a list node consists of valid list, then directly marginalize it
      if null parent then marginalize qDist g m t
      -- otherwise
      else
      let rvParents = filter (lam p. match p with RandomVarNode _ then true else false) parents in
      let muxParents = filter (lam p. match p with MultiplexerNode _ then true else false) parents in
      let res =
        (if null rvParents then
            if gti (length muxParents) 1 then
              foldl (lam acc. lam e.
                match e with MultiplexerNode mux then
                  let lst = inputMultiplexer g e in
                  match lst with ListNode l then
                    foldl (lam acc. lam e.
                          let e = mapLookupOrElse (lam. error "Reorder:Lookup failed") e acc.2 in
                          let res = reorder acc.0 acc.1 acc.2 e in
                          (res.0,res.1,res.2,acc.3)) acc l.items
                  else never
                else never) (qDist, g, m, get muxParents 0) (tail muxParents)
            else (qDist, g, m, get muxParents 0)
        else
          let res =
            if gti (length rvParents) 1 then
            (if debug then print "Graft: RV t has more than one parents\n" else ());

              foldl (lam acc. lam p.
                let res = graft acc.0 acc.1 acc.2 p in
                let res = reorder res.0 res.1 res.2 p in (res.0,res.1,res.2,acc.3))
                (qDist, g, m, get rvParents 0) (tail rvParents)
            else (qDist, g, m,get rvParents 0) in
          if null muxParents then (res.0,res.1,res.2,get rvParents 0) else
            foldl (lam acc. lam e.
                  match e with MultiplexerNode mux then
                    let lst = inputMultiplexer g e in
                    match lst with ListNode l then
                      foldl (lam acc. lam i.
                            let e = mapLookupOrElse (lam. error "Reorder:Lookup failed") i acc.2 in
                            let res = graft acc.0 acc.1 acc.2 e in
                            let e = mapLookupOrElse (lam. error "Reorder:Lookup failed") i res.2 in
                            let res = reorder res.0 res.1 res.2 e in
                            (res.0,res.1,res.2,acc.3)) acc l.items
                    else never
                  else never) (res.0,res.1,res.2,res.3) muxParents) in
          let qDist = res.0 in
          let g = res.1 in
          let m = res.2 in
          let parent = res.3 in
          -- if its parent is a random variable then graft the parent first, and then marginalize t
          match parent with RandomVarNode _ then
            (if debug then print (join ["Graft:parent ",v2str parent,"\nchild ",v2str t ,"\n"]) else ());
             (if debug then print "Graft: parent of t is a rv\n" else ());
              let res = graft qDist g m parent in
              marginalize res.0 res.1 res.2 t
          else -- if its parent is from a list then
            match parent with MultiplexerNode _ then
              (if debug then print "Graft: t's parent comes from a list\n" else ());
              let l = inputMultiplexer g parent in
              let items = match l with ListNode l then
                filter (lam e. let e = mapLookupOrElse (lam. error "Marginalize:Lookup failed") e m in
                                match e with RandomVarNode r then eqi r.color 0 else false) l.items
              else never in
              let res =
                match l with ListNode l then
                  foldl (lam acc. lam e.
                    let e = mapLookupOrElse (lam. error "Marginalize:Lookup failed") e m in
                    graft acc.0 acc.1 acc.2 e
                   ) (qDist,g,m) items
                else never in
              marginalize res.0 res.1 res.2 t
            else never -- no other case
  else error "t is not a random variable node\n"

end

let getRoots = lam g:Digraph Vertex Label.
  let vertices = digraphVertices g in
  filter (lam v. null (digraphEdgesTo v g)) vertices

let transformModel = lam g:Digraph Vertex Label. lam targets:[Vertex]. lam m:Map Name Vertex.
  use ConjugatePrior in
  let qDist = mapEmpty nameCmp in
  if null targets then (qDist, g, m)
  else
    let roots = getRoots g in
    (if debug then print "Root nodes:\n";iter (lam r. print (join [(v2str r),"\n"])) roots else ());
(if debug then print "Target nodes:\n";iter (lam r. print (join [(v2str r),"\n"])) targets else ());

  -- fold the graph over targets
  -- res.0 : marginalized distributions
  -- res.1 : transformed graph
  let res:(Map Name Expr, Digraph Vertex Label,Map Name Vertex) = foldl (lam acc:(Map Name Expr,Digraph Vertex Label,Map Name Vertex). lam t.
  let qDist = acc.0 in
  let g = acc.1 in
  let m = acc.2 in
  let t = get (filter (lam v. digraphEqv g v t) (digraphVertices g)) 0 in
  let graftRes:(Map Name Expr, Digraph Vertex Label, Map Name Vertex) = graft qDist g m t in
  let qDist = graftRes.0 in
  /-print "\n qDIST\n";
  iter (lam b. print (join [let id= b.0 in id.0," ",  (expr2str b.1), "\n"])) (mapBindings qDist);
  print "\n GRAPH\n";
  digraphPrintDot graftRes.1 v2str int2string;
  print "\n\n";-/
 let t = get (filter (lam v. digraphEqv graftRes.1 v t) (digraphVertices graftRes.1)) 0 in
  let reorderRes:(Map Name Expr, Digraph Vertex Label, Map Name Vertex) = reorder graftRes.0 graftRes.1 graftRes.2 t in
  reorderRes
  ) (qDist, g, m) targets in
  res

let modifyGraph = use StaticAnalyzer in
  lam g:Digraph Vertex Label. lam m:Map Name Vertex.
  let lists = filter (lam v. match v with ListNode _ then true else false) (digraphVertices g) in
  foldl (lam g. lam l.
          match l with ListNode r then
            foldl (lam g:Digraph Vertex Label. lam id:Name.
                    let i = mapLookupOrElse (lam. error "Lookup failed") id m in
                    let edges = digraphEdgesTo i g in
                    digraphMaybeAddEdges (map (lam e. (e.0,l,e.2)) edges) g) g r.items else never) g lists

let analyze = lam prog.
  use StaticAnalyzer in
  let emptyG = digraphEmpty cmprVertex eqi in
  let emptyM = lam. mapEmpty nameCmp in
  createPBN emptyG (setEmpty nameCmp) (emptyM ()) (emptyM ()) (emptyM ()) (None ()) prog

let recreate = lam g:Digraph Vertex Label. lam m:Map Name Vertex.
  use StaticAnalyzer in
  let g = modifyGraph g m in
  let order = digraphTopologicalOrder g in
  let vRet = filter (lam v. match v with CodeBlockNode c then c.ret else false) order in
  let order = filter (lam v. match v with CodeBlockNode c then not c.ret else true) order in
  recreateVertex (Some (get vRet 0)) g m order

let transformM = lam model.
  use ConjugatePrior in
  let g = analyze model in
  --digraphPrintDot g.0 v2str int2string;
  --print "\n";
  --digraphPrintVertices g.0 v2str;
  --digraphPrintDot g.0 v2str int2string;
  --print "\n";
  let targetVertices = map (lam i. mapLookupOrElse (lam. error "Lookup failed") i g.1)  (setToSeq g.2) in
  let targetObserves = filter (lam v. match v with RandomVarNode v then (match v.val with Some _ then true else false) else false) targetVertices in
  let targets = filter (lam v. match v with RandomVarNode v then (match v.val with Some _ then false else true) else true) targetVertices in
  let res = transformModel g.0 (concat targetObserves targets) g.1 in
  --let qDist = res.0 in
  --print "\n qDIST\n";
  --iter (lam b. print (join [let id= b.0 in id.0," ", match b.1 with Some d then (expr2str d) else "no dist", "\n"])) (mapBindings qDist);
  let rg = res.1 in
  let m = res.2 in
  --digraphPrintDot rg v2str int2string;
  --print "\n\n";
  --digraphPrintVertices rg v2str;
  let rProg = recreate rg m in-- m in
  rProg

lang Transformation = ConjugatePrior

  sem isAstLetBinding =
    | TmLet t -> match t.inexpr with TmLet t2 then
                      (print (expr2str (TmLet t)));isLastLetBinding t2.inexpr
                    else false
    | TmRecLets t -> false
    | TmConDef t -> false
    | TmType t -> false
    | TmExt t -> false
    | _ -> true

  sem isLastLetBinding =
    | TmLet t -> false
    | TmRecLets t -> false
    | TmConDef t -> false
    | TmType t -> false
    | TmExt t -> false
    | _ -> true

  sem transform =
  | prog -> transformH prog
 -- | prog -> transformH (normalizeTerm prog)

  sem transformH =
    | TmLet t -> match isLastLetBinding t.inexpr with true then
                  match t.body with TmLam l then
                    TmLet {t with body=(TmLam {l with body=transformM l.body})}
                  else error "astvar not found"
                else TmLet {t with inexpr=transformH t.inexpr}
    | TmRecLets t -> TmRecLets {t with inexpr=transformH t.inexpr}
    | TmConDef t -> TmConDef {t with inexpr=transformH t.inexpr}
    | TmType t -> TmType {t with inexpr=transformH t.inexpr}
    | TmExt t -> TmExt {t with inexpr=transformH t.inexpr}
    | t -> t
end

mexpr
use Transformation in

let list1 =
  bindall_
  [ ulet_ "l1" (seq_ [(assume_ (gaussian_ (float_ 0.0) (float_ 1.0)))
  , (assume_ (gaussian_ (float_ 2.0) (float_ 1.0)))])
  , ulet_ "mu" (get_ (var_ "l1") (int_ 0))
  , var_ "mu"
  ] in

let list2 =
  bindall_
  [ ulet_ "l1" (seq_ [(assume_ (gaussian_ (float_ 0.0) (float_ 1.0)))
  , (assume_ (beta_ (float_ 2.0) (float_ 1.0)))])
  , ulet_ "mu" (get_ (var_ "l1") (int_ 0))
  , var_ "mu"
  ] in

let case1 =
  bindall_
  [ ulet_ "x" (assume_ (beta_ (float_ 10.0) (float_ 5.0)))
  , ulet_ "" (observe_ true_ (bern_ (var_ "x")))
  , var_ "x"
  ] in

let case11 =
  bindall_
  [ ulet_ "x" (assume_ (beta_ (float_ 10.0) (float_ 5.0)))
  , ulet_ "y" (assume_ (bern_ (var_ "x")))
  , var_ "y"
  ] in

let case2 =
  bindall_
  [  ulet_ "z" (float_ 3.0)
  , ulet_ "f" (ulam_ "x" (addi_  (var_ "x") (float_ 10.0)))

   ,ulet_ "a" (assume_ (beta_  (var_ "z") (float_ 10.0)))
  , ulet_ "" (observe_ true_ (bern_ (var_ "a")))
  , var_ "a"
  ] in

let case3 =
  bindall_
  [ ulet_ "x" (assume_ (beta_ (float_ 10.0) (float_ 10.0)))
  , ulet_ "y" (assume_ (bern_ (var_ "x")))
  , var_ "y"
  ] in

let case4 =
  bindall_
  [ --next_ ("externalSqrt", gensym()) (false_) tyunknown_
   ulet_ "z" (float_ 10.0)
  , ulet_ "x" (assume_ (beta_ (var_ "z") (var_ "z")))
  , ulet_ "y" (assume_ (bern_ (var_ "x")))
  , var_ "y"
  ] in
let case4_1 = bindall_
  [ --next_ ("externalSqrt", gensym()) (false_) tyunknown_
   ulet_ "x" (assume_ (gaussian_ (float_ 0.0) (float_ 1.0)))
  , ulet_ "" (observe_ (float_ 1.0) (gaussian_ (var_ "x") (float_ 1.0)))
  , var_ "x"
  ] in

let case5 =
  bindall_
  [ ulet_ "z" (float_ 10.0)
  , ulet_ "x" (assume_ (gaussian_ (float_ 0.0) (var_ "z")))
  , ulet_ "y" (assume_ (gaussian_ (var_ "x") (var_ "z")))
  , var_ "y"
  ] in

let case6 =
  bindall_
  [ ulet_ "z" (float_ 10.0)
  , ulet_ "x" (assume_ (beta_ (var_ "z") (var_ "z")))
  , ulet_ "y" (assume_ (bern_ (var_ "x")))
  , var_ "y"
  ] in

let case7 =
  bindall_
  [ --ulet_ "a" (assume_ (gaussian_ (float_ 0.0) (float_ 1.0)))
  ulet_ "a" (float_ 2.0)
  , ulet_ "lst" (seq_ [(assume_ (gaussian_ (var_ "a") (float_ 1.0)))
  ,  (assume_ (gaussian_ (var_ "b") (float_ 1.0)))
  , (assume_ (gaussian_ (var_ "c") (float_ 1.0)))])
  , var_ "d"
  ] in
let case7_2 =
  bindall_
  [  ulet_ "x" (seq_ [(assume_ (gaussian_ (float_ 1.0) (float_ 1.0)))
  ,  (assume_ (gaussian_ (float_ 2.0) (float_ 1.0)))
  ])
  , get_ (var_ "x") (int_ 0)
  ] in
let case7_3 =
  bindall_
  [ ulet_ "a" (float_ 2.0)
  , ulet_ "x" (seq_ [(assume_ (gaussian_ (var_ "a") (float_ 1.0)))
  , (assume_ (gaussian_ (float_ 2.0) (float_ 1.0)))
  ])
  --, get_ (var_ "x") (int_ 0)
  , var_ "a"
  ] in

let case7_4 =
  bindall_
  [ ulet_ "a" (float_ 2.0)
  , ulet_ "x" (seq_ [(assume_ (gaussian_ (var_ "a") (float_ 1.0)))
  , (assume_ (gaussian_ (float_ 2.0) (float_ 1.0)))
  ])
  , ulet_ "b" (get_ (var_ "x") (int_ 0))
  , var_ "b"
  ] in

let case8_1 =
  bindall_
  [ ulet_ "a" (assume_ (beta_ (float_ 1.0) (float_ 1.0)))
  , ulet_ "" (observe_ true_ (bern_ (var_ "a")))
  , ulet_ "" (observe_ true_ (bern_ (var_ "a")))

  , ulet_ "" (observe_ true_ (bern_ (var_ "a")))

  , ulet_ "" (observe_ true_ (bern_ (var_ "a")))

  , ulet_ "" (observe_ true_ (bern_ (var_ "a")))

  , ulet_ "" (observe_ true_ (bern_ (var_ "a")))

  , ulet_ "" (observe_ true_ (bern_ (var_ "a")))
  , ulet_ "" (observe_ true_ (bern_ (var_ "a")))
  , ulet_ "" (observe_ true_ (bern_ (var_ "a")))

  , ulet_ "" (observe_ true_ (bern_ (var_ "a")))

  , ulet_ "" (observe_ true_ (bern_ (var_ "a")))

  , ulet_ "" (observe_ true_ (bern_ (var_ "a")))

  , ulet_ "" (observe_ true_ (bern_ (var_ "a")))

  , ulet_ "" (observe_ true_ (bern_ (var_ "a")))

  , ulet_ "" (observe_ true_ (bern_ (var_ "a")))
  , ulet_ "" (observe_ true_ (bern_ (var_ "a")))

  , ulet_ "" (observe_ true_ (bern_ (var_ "a")))

  , ulet_ "" (observe_ true_ (bern_ (var_ "a")))

  , ulet_ "" (observe_ true_ (bern_ (var_ "a")))

  , ulet_ "" (observe_ true_ (bern_ (var_ "a")))

  , ulet_ "" (observe_ true_ (bern_ (var_ "a")))
 , ulet_ "" (observe_ true_ (bern_ (var_ "a")))

  , ulet_ "" (observe_ true_ (bern_ (var_ "a")))

  , ulet_ "" (observe_ true_ (bern_ (var_ "a")))

  , ulet_ "" (observe_ true_ (bern_ (var_ "a")))

  , ulet_ "" (observe_ true_ (bern_ (var_ "a")))

  , ulet_ "" (observe_ true_ (bern_ (var_ "a")))
  , ulet_ "" (observe_ true_ (bern_ (var_ "a")))

  , ulet_ "" (observe_ true_ (bern_ (var_ "a")))

  , ulet_ "" (observe_ true_ (bern_ (var_ "a")))

  , ulet_ "" (observe_ true_ (bern_ (var_ "a")))

  , ulet_ "" (observe_ true_ (bern_ (var_ "a")))

  , ulet_ "" (observe_ true_ (bern_ (var_ "a")))

  , var_ "a"
  ] in

let case9 =
  bindall_
  [ ulet_ "a" (assume_ (beta_ (float_ 1.0) (float_ 1.0)))
  , ulet_ "b" (assume_ (beta_ (var_ "a") (float_ 1.0)))
  , ulet_ "c" (assume_ (beta_ (var_ "b") (float_ 1.0)))
  , ulet_ "d" (assume_ (beta_ (var_ "c") (float_ 1.0)))
  , var_ "d"
  ] in
let case10 =
  bindall_
  [ ulet_ "a" (assume_ (beta_ (float_ 1.0) (float_ 1.0)))
  , ulet_ "b" (assume_ (gaussian_ (var_ "a") (float_ 1.0)))
  , var_ "b"
  ] in


let case8_2 =
  bindall_
  [ ulet_ "a" (assume_ (beta_ (float_ 1.0) (float_ 1.0)))
  , ulet_ "" (observe_ (float_ 1.0) (gaussian_ (var_ "a") (float_ 1.0)))
  , var_ "a"
  ] in
let cases = [case1,case2,case3,case4,case5,case6,case7,case9,case10] in

let model1 =
 bindall_
  [ ulet_ "x" (assume_ (beta_ (float_ 10.0) (float_ 10.0)))
  , ulet_ "y" (assume_ (bern_ (var_ "x")))
--  , ulet_ "a" (addf_ (var_ "x") (var_ "y"))
 -- , ulet_ "obs" (true_)
 -- , ulet_ "" (observe_ (var_ "obs") (bern_ (var_ "x")))
  , ulet_ "" (observe_ true_ (bern_ (var_ "x")))
 -- , ulet_ "z" (addf_ (var_ "x") (var_ "y"))
 -- , ulet_ "t" (addf_ (var_ "z") (var_ "z"))
  , var_ "x"
  ] in
/-
let emptyGraph = digraphEmpty subi eqsym in
let l1 = gensym () in
let l2 = gensym () in
let l3 = gensym () in
let g = digraphAddVertices [1,2,3] emptyGraph in
let g = digraphAddEdges [(1,2,l1),(1,3,l2),(2,3,l3)] g in
utest modifiedBFS 1 2 g with true in
utest modifiedBFS 1 3 g with false in
utest modifiedBFS 2 3 g with true in  -/
--map transformModel cases;
--print (expr2str (transformModel list1));
--print (expr2str (transformModel list2));

--transformModel case1;
--transformModel list2;

()
