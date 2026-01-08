include "ext/arr-ext.mc"
include "ext/dist-ext.mc"
include "parray.mc"

include "json.mc"
include "set.mc"

let _cmpSym = lam a. lam b. subi (sym2hash a) (sym2hash b)
let eqb = lam a. lam b.
  if a then b else not b
type Never
type Never2


-- === Distribution types ===

type PDist a =
  { sample : () -> a
  , logObserve : a -> Float
  }
let p_sample : all a. PDist a -> a = lam dist. dist.sample ()
let p_logObserve : all a. PDist a -> a -> Float = lam dist. lam val. dist.logObserve val


-- === Finally tagless representation of piece-wise static PVal models ===

type Complete
type Partial

lang PValInterface
  syn PVal a =
  syn PValState st =

  syn Prune a =

  syn PWeightRef =
  syn PAssumeRef a =
  syn PExportRef a =
  syn PSubmodelRef st =


  syn PValInstance complete st =


  -- === Working with an instance of a model ===

  -- Create a new instance of a model (completely evaluated).
  sem instantiate : all st. all st2. (PValState st -> PValState st2) -> st -> PValInstance Complete st2
  -- Get the state in the instance (that contains references to
  -- internals of the model).
  sem getSt : all complete. all st. PValInstance complete st -> st
  -- Start accumulating actions to perform in a single update step.
  sem startStep : all st. PValInstance Complete st -> PValInstance Partial st
  -- Takes a predicate, typically probabilistic, that decides whether
  -- to return the model instance as it was before the step (if false)
  -- or after (if true). The predicate is given the post-step instance
  -- and an mcmc acceptance (log-)ratio computed from the step itself.
  sem finalizeStep : all st. (PValInstance Complete st -> Float -> Bool) -> PValInstance Partial st -> (Bool, PValInstance Complete st)
  -- Mark a given assume to be resampled in the upcoming step with the
  -- given drift kernel. The first argument is the distribution we
  -- *should* have drawn from, to make it easy to use no drift kernel.
  sem resampleAssume : all st. all a. (PDist a -> a -> PDist a) -> PAssumeRef a -> PValInstance Partial st -> PValInstance Partial st
  -- Read the current value produced by the given assume. In a
  -- `Partial` instance this returns the value before the step was
  -- initiated.
  sem readPreviousAssume : all complete. all st. all a. PAssumeRef a -> PValInstance complete st -> a
  -- Read the current value produced by the given export. In a
  -- `Partial` instance this returns the value before the step was
  -- initiated.
  sem readPreviousExport : all complete. all st. all a. PExportRef a -> PValInstance complete st -> a
  -- Read the current state of the given sub-model. In a `Partial`
  -- instance this returns the value before the step was initiated.
  sem readPreviousSubmodel : all complete. all st. all st2. PSubmodelRef st2 -> PValInstance complete st -> st2


  -- === Building a model ===

  -- Insert a dynamic check for if a given value has changed, removing
  -- the need to update later portions of the model if the newly
  -- produced value is the same as the previous.
  sem p_cache : all st. all a. PValState st
    -> (a -> a -> Bool)
    -> PVal a
    -> (PValState st, PVal a)
  -- Make a probabilistic value available to read outside the model,
  -- via a model instance.
  sem p_export : all st. all st2. all a. PValState st
    -> (st -> PExportRef a -> st2)
    -> PVal a
    -> PValState st2

  -- Lift a deterministic value to a probabilistic one. (from the
  -- Pointed type class)
  sem p_pure : all st. all a. PValState st
    -> a
    -> (PValState st, PVal a)
  -- Make a deterministic "update" to a probabilistic value. (from the
  -- Functor type class)
  sem p_map : all st. all a. all b. PValState st
    -> (a -> b)
    -> PVal a
    -> (PValState st, PVal b)
  -- Combine two probabilistic values. Often used in conjunction with
  -- `p_map`. (from the Applicative type class)
  sem p_apply : all st. all a. all b. PValState st
  -> PVal (a -> b)
    -> PVal a
    -> (PValState st, PVal b)
  -- Create a sub-model whose shape depends on a probabilistic
  -- value. (mostly equivalent with bind from Monad)
  sem p_bind : all st. all ist. all ist2. all st2. all a. all b. PValState st
    -> (st -> PSubmodelRef ist2 -> st2)
    -> ist
    -> (PValState ist -> a -> (PValState ist2, PVal b))
    -> PVal a
    -> (PValState st2, PVal b)
  -- Select which probabilistic value to propagate based on the value
  -- of another probabilistic value. Weaker version of bind that
  -- sometimes admits more efficient implementation. (mostly
  -- equivalent with select from Selective)
  sem p_select : all st. all a. all b. PValState st
    -> (a -> PVal b)
    -> PVal a
    -> (PValState st, PVal b)

  -- Pick one of two sub-models via a _deterministic_ function of the
  -- input. This is more in spirit with Selective than p_select.
  sem p_match : all st. all ist. all ist2. all st2. all a. all b. all c. PValState st
    -> (st -> PSubmodelRef ist2 -> st2)
    -> ist
    -> PVal a
    -> (a -> Option b)
    -> (PValState ist -> Option (PVal b) -> (PValState ist2, PVal c))
    -> (PValState st2, PVal c)

  -- Create a black-box node that can do anything. Can read inputs
  -- from the surrounding graph, and will re-run when any read input
  -- changes.
  sem p_chunk : all st. all b. PValState st
    -> (all x. PChunkState x -> b)
    -> (PValState st, PVal b)
  -- API detail, needed to actually interact with the graph from
  -- within a chunk.
  syn PChunkState x =
  -- Read a value from a PVal when inside a chunk.
  sem p_readPVal : all x. all a. PChunkState x -> PVal a -> a
  -- Add an accumulating weight to a chunk.
  sem p_weightChunk : all x. PChunkState x -> Float -> ()

  -- Create a sub-model without extracting a value from it. This plus
  -- p_join is equivalent with p_bind.
  sem p_subMap : all st. all st2. all ist. all ist2. all a. all b. PValState st
    -> (st -> PSubmodelRef ist2 -> st2)
    -> ist
    -> (PValState ist -> a -> (PValState ist2, b))
    -> PVal a
    -> (PValState st2, PVal b)

  -- Flatten nesting of PVals.
  sem p_join : all st. all a. PValState st
    -> PVal (PVal a)
    -> (PValState st, PVal a)

  -- Create a composite value from many inputs, then incrementally
  -- compute updated versions when inputs change. The first function
  -- is the initial creation, the second is the incremental
  -- update. The list passed to the latter will contain one element
  -- per input that has changed, and will never be empty.
  sem p_incremental : all st. all a. all b. PValState st
    -> ([a] -> b)
    -> ([{prev : a, new : a}] -> b -> b)
    -> [PVal a]
    -> (PValState st, PVal b)

  sem p_traverseSeq : all st. all a. all b. PValState st
    -> (PValState st -> a -> (PValState st, PVal b))
    -> [a]
    -> (PValState st, PVal [b])
  sem p_traverseSeq st f =
  | [] -> p_pure st []
  | [a] ++ as ->
    match f st a with (st, a) in
    match p_traverseSeq st f as with (st, as) in
    match p_map st cons a with (st, tmp) in
    match p_apply st tmp as with (st, res) in
    (st, res)

  -- Introduce a weight.
  sem p_weight : all st. all st2. all a. PValState st
    -> (st -> PWeightRef -> st2)
    -> (a -> Float)
    -> PVal a
    -> PValState st2
  -- Draw a value from a distribution.
  sem p_assume : all st. all st2. all a. PValState st
    -> (st -> PAssumeRef a -> st2)
    -> PVal (PDist a)
    -> (PValState st2, PVal a)

  -- Versions that don't record their reference in the state
  sem p_bind_ : all st. all a. all as. all b. PValState st
    -> (PValState () -> a -> (PValState (), PVal b))
    -> PVal a
    -> (PValState st, PVal b)
  sem p_bind_ st f = | a -> p_bind st (lam st. lam. st) () f a

  sem p_match_ : all st. all a. all b. all c. PValState st
    -> PVal a
    -> (a -> Option b)
    -> (PValState () -> Option (PVal b) -> (PValState (), PVal c))
    -> (PValState st, PVal c)
  sem p_match_ st a pick = | build -> p_match st (lam st. lam. st) () a pick build

  sem p_weight_ : all st. all a. PValState st
    -> (a -> Float)
    -> PVal a
    -> PValState st
  sem p_weight_ st f = | a -> p_weight st (lam st. lam. st) f a

  sem p_assume_ : all st. all a. PValState st
    -> PVal (PDist a)
    -> (PValState st, PVal a)
  sem p_assume_ st = | dist -> p_assume st (lam st. lam. st) dist

  sem p_subMap_ : all st. all a. all b. PValState st
    -> (PValState () -> a -> (PValState (), b))
    -> PVal a
    -> (PValState st, PVal b)
  sem p_subMap_ st f = | a -> p_subMap st (lam st. lam. st) () f a

  -- Introduce a pruned variable. Note that this is a low-level
  -- primitive; it introduces the "superposition" by taking the
  -- support of the corresponding distribution, but assumes the user
  -- will later use `p_pruneWeight` to connect the superposition to
  -- the PMF of the distribution.
  sem p_prune : all st. all a. PValState st
    -> PVal [a]
    -> (PValState st, PVal (Prune a))
  -- Introduce a weight based on pruned variables.
  sem p_pruneWeight : all st. PValState st
    -> PVal (Prune Float)
    -> PValState st

  sem pp_pure : all a. a -> Prune a
  sem pp_map : all a. all b. (a -> b) -> Prune a -> Prune b
  sem pp_apply : all a. all b. Prune (a -> b) -> Prune a -> Prune b
end



lang PruneBasics = PValInterface
  syn Prune a =
  | Prune {vars : Map Symbol Int, values : [a]}

  sem pp_debugPrune : all a. [a] -> Prune a
  sem pp_debugPrune = | values ->
    Prune {vars = mapSingleton _cmpSym (gensym ()) (length values), values = values}

  sem pp_pure = | a -> Prune {vars = mapEmpty _cmpSym, values = [a]}
  sem pp_map f = | Prune x -> Prune {vars = x.vars, values = map f x.values}
  sem pp_apply f = | Prune x ->
    match f with Prune f in
    let vars = mapUnion f.vars x.vars in
    let numValues = mapFoldWithKey (lam acc. lam. lam count. muli acc count) 1 vars in
    -- NOTE(vipa, 2025-11-06): Conceptually, for each `Prune` value we
    -- want a mapping between index in `values` and position in the
    -- tensor, i.e., one index per entry in `vars`. Implementation
    -- could go directly via that mapping (which is probably simpler
    -- to implement, and a good first step, but probably not the most
    -- efficient). It might also be possible to "step" through the two
    -- input sequences and the output in sync, which might be more
    -- efficient, but harder to get right.
    let values = never in -- TODO(vipa, 2025-11-06): Do the thing
    Prune {vars = vars, values = values}

  syn Graph =
  sem emptyGraph : () -> Graph
  sem pickVar : Graph -> (Option Symbol, [Prune Float], Graph)
  sem addWeights : Prune Float -> Graph -> Graph

  sem marginalizeAllPruned : [Prune Float] -> Float
  sem marginalizeAllPruned = | weights ->
    let graph = foldl (lam acc. lam p. addWeights p acc) (emptyGraph ()) weights in
    recursive let work = lam graph.
      switch pickVar graph
      case (Some var, [p] ++ prunes, graph) then
        let merged = foldl (lam acc. pp_apply (pp_map addf acc)) p prunes in
        match merged with Prune x in
        let values = never in  -- TODO(vipa, 2025-11-06): marginalize out var
        let marginalized = Prune {vars = mapRemove var x.vars, values = values} in
        work (addWeights marginalized graph)
      case (None _, [Prune {values = [w]}], _) then w
      case (None _, [], _) then 0.0
      end in
    work graph
end
