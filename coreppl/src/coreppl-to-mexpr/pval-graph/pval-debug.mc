include "pval-interface.mc"
include "json.mc"
include "common.mc"


-- === Visualizations for PVal graphs ===

-- This implementation isn't really a proper implementation of the
-- graph, it just constructs a graph in one shot for visualization
-- purposes; it does not support most of the interfaces required to
-- actually run inference.

lang PValVisiGraph = PValInterface
  type NodeID = Int

  syn PValNode =
  | PVNPure {id : NodeID}
  | PVNMap {id : NodeID, a : NodeID}
  | PVNApply {id : NodeID, f : NodeID, a : NodeID}
  | PVNSelect {id : NodeID, a : NodeID, sub : NodeID}
  | PVNBind {id : NodeID, a : NodeID, sub : NodeID}
  | PVNCache {id : NodeID, a : NodeID}
  | PVNWeight {id : NodeID, a : NodeID}
  | PVNAssume {id : NodeID, a : NodeID}
  | PVNExport {id : NodeID, a : NodeID}

  sem nodeKind : PValNode -> String
  sem nodeKind =
  | PVNPure _ -> "pure"
  | PVNMap _ -> "map"
  | PVNApply _ -> "apply"
  | PVNSelect _ -> "select"
  | PVNBind _ -> "bind"
  | PVNCache _ -> "cache"
  | PVNWeight _ -> "weight"
  | PVNAssume _ -> "assume"
  | PVNExport _ -> "export"

  sem normalInputs : PValNode -> [NodeID]
  sem normalInputs =
  | PVNApply {f = f, a = a} -> [f, a]
  | PVNMap {a = a}
  | PVNSelect {a = a}
  | PVNBind {a = a}
  | PVNCache {a = a}
  | PVNWeight {a = a}
  | PVNAssume {a = a}
  | PVNExport {a = a} -> [a]
  | PVNPure _ -> []

  sem subInput : PValNode -> Option NodeID
  sem subInput =
  | PVNSelect x | PVNBind x -> Some x.sub
  | _ -> None ()

  syn PVal a = | PVal {val : a, node : PValNode}

  syn PValState st =
  | PVS [PValNode]

  syn PValInstance complete st =
  | PVI [PValNode]

  sem instantiate f = | _ ->
    match f (PVS []) with PVS nodes in
    PVI nodes

  sem getID : PValNode -> NodeID
  sem getID =
  | PVNPure {id = id}
  | PVNMap {id = id}
  | PVNApply {id = id}
  | PVNSelect {id = id}
  | PVNBind {id = id}
  | PVNCache {id = id}
  | PVNWeight {id = id}
  | PVNAssume {id = id}
  | PVNExport {id = id} -> id

  sem p_cache st eq = | pval & PVal x ->
    match st with PVS nodes in
    let node = PVNCache {id = length nodes, a = getID x.node} in
    (PVS (snoc nodes node), PVal {val = x.val, node = node})

  sem p_export st store = | PVal x ->
    match st with PVS nodes in
    let node = PVNExport {id = length nodes, a = getID x.node} in
    PVS (snoc nodes node)

  sem p_pure = | a ->
    let node = PVNPure {id = negi 1} in
    PVal {val = a, node = node}

  sem p_map st f = | PVal x ->
    match st with PVS nodes in
    let node = PVNMap {id = length nodes, a = getID x.node} in
    (PVS (snoc nodes node), PVal {val = f x.val, node = node})

  sem p_apply st pval1 = | pval2 ->
    match st with PVS nodes in
    match pval1 with PVal {val = f, node = fnode} in
    match pval2 with PVal {val = a, node = anode} in
    let node = PVNApply {id = length nodes, f = getID fnode, a = getID anode} in
    (PVS (snoc nodes node), PVal {val = f a, node = node})

  sem p_bind st store initSt2 f = | PVal x ->
    match st with PVS nodes in
    match f (PVS nodes) x.val with (PVS nodes, PVal res) in
    let node = PVNBind {id = length nodes, a = getID x.node, sub = getID res.node} in
    (PVS (snoc nodes node), PVal {val = res.val, node = node})

  sem p_select st f = | PVal x ->
    match st with PVS nodes in
    match f x.val with PVal res in
    let node = PVNSelect {id = length nodes, a = getID x.node, sub = getID res.node} in
    (PVS (snoc nodes node), PVal {val = res.val, node = node})

  sem p_weight st store f = | PVal x ->
    match st with PVS nodes in
    let node = PVNWeight {id = length nodes, a = getID x.node} in
    PVS (snoc nodes node)

  sem p_assume st store = | PVal x ->
    match st with PVS nodes in
    let res = sample x.val in
    let node = PVNAssume {id = length nodes, a = getID x.node} in
    (PVS (snoc nodes node), PVal {val = res, node = node})

  sem nodeToJson : PValNode -> [JsonValue]
  sem nodeToJson = | node ->
    let id = getID node in
    if geqi id 0 then
      let self = JsonObject (mapFromSeq cmpString
        [ ("kind", JsonString (nodeKind node))
        , ("label", JsonString (nodeKind node))
        , ("id", JsonInt id)
        ]) in
      let mkEdge = lam target. lam id. if geqi id 0
        then Some (JsonObject (mapFromSeq cmpString [("from", JsonInt id), ("to", target)]))
        else None () in
      let normals = mapOption (mkEdge (JsonInt id)) (normalInputs node) in
      let subs =
        match subInput node with Some sub then
          let target = JsonString (concat "sub_" (int2string id)) in
          let targetNode = JsonObject (mapFromSeq cmpString [("id", target), ("kind", JsonString "submodel")]) in
          let edge2 = JsonObject (mapFromSeq cmpString [("from", target), ("to", JsonInt id)]) in
          optionMapOr [targetNode, edge2] (lam edge. [targetNode, edge, edge2]) (mkEdge target sub)
        else [] in
      join [[self], normals, subs]
    else []

  sem graphToJson : all complete. all st. PValInstance complete st -> JsonValue
  sem graphToJson = | PVI nodes ->
    JsonArray (join (map nodeToJson nodes))
end
