include "../runtime-dists.mc"

-- === Finally tagless representation of piece-wise static PVal models ===

type Complete
type Partial

lang PValInterface = RuntimeDistBase
  syn PVal a =
  syn PValState st =

  syn PWeightRef =
  syn PAssumeRef a =
  syn PSomeAssumeRef = | PSomeAssumeRef (all st. PValInstance Partial st -> PValInstance Partial st)
  syn PExportRef a =
  syn PSubmodelRef st =


  syn PValInstance complete st =


  -- === Working with an instance of a model ===

  -- Create a new instance of a model (completely evaluated).
  sem instantiate : all st. all st2. (PValState st -> PValState st2) -> st -> PValInstance Complete st2
  -- Get the state in the instance (that contains references to
  -- internals of the model).
  sem getSt : all complete. all st. PValInstance complete st -> st
  -- Get the weight of the current instance of the model.
  sem getWeight : all complete. all st. PValInstance complete st -> Float
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
  sem resampleAssume : all st. all a. (Dist a -> a -> Dist a) -> PAssumeRef a -> PValInstance Partial st -> PValInstance Partial st
  -- Read the current value produced by the given assume. In a
  -- `Partial` instance this returns the value before the step was
  -- initiated.
  sem readPreviousAssume : all complete. all st. all a. PAssumeRef a -> PValInstance complete st -> a
  -- Type-erased version of an AssumeRef.
  sem asSomeAssume : all a. (Dist a -> a -> Dist a) -> PAssumeRef a -> PSomeAssumeRef
  sem asSomeAssume drift = | ref ->
    let f : all st. PValInstance Partial st -> PValInstance Partial st = lam st. resampleAssume drift ref st in
    PSomeAssumeRef #frozen"f"
  -- Resampling of a type-erased AssumeRef.
  sem resampleSomeAssume : all st. PSomeAssumeRef -> PValInstance Partial st -> PValInstance Partial st
  sem resampleSomeAssume = | PSomeAssumeRef f -> f
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
  sem p_pure : all st. all a. a -> PVal a
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
    -> PVal (Dist a)
    -> (PValState st2, PVal a)

  -- Versions that don't record their reference in the state
  sem p_bind_ : all st. all a. all as. all b. PValState st
    -> (PValState () -> a -> (PValState (), PVal b))
    -> PVal a
    -> (PValState st, PVal b)
  sem p_bind_ st f = | a -> p_bind st (lam st. lam. st) () f a

  sem p_weight_ : all st. all a. PValState st
    -> (a -> Float)
    -> PVal a
    -> PValState st
  sem p_weight_ st f = | a -> p_weight st (lam st. lam. st) f a

  sem p_assume_ : all st. all a. PValState st
    -> PVal (Dist a)
    -> (PValState st, PVal a)
  sem p_assume_ st = | dist -> p_assume st (lam st. lam. st) dist
end
