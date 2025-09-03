-- This is the result I'd hope for from PEval, ignoring the library
-- functions used. It wouldn't surprise me if I've missed renaming
-- something small when copying code, but I did try to get things
-- unique and renamed.

type PDist a =
  { sample : () -> a
  , logObserve : a -> Float
  }
let p_sample : all a. PDist a -> a = lam dist. dist.sample ()
let p_logObserve : all a. PDist a -> a -> Float = lam dist. lam val. dist.logObserve val

let p_bernoulli : Float -> PDist Bool
  = lam p.
    { sample = lam. bernoulliSample p
    , logObserve = lam v. bernoulliLogPmf p v
    }

mexpr

let dist = p_bernoulli 0.5 in

let aValue = ref (p_sample dist) in
let aWeight = ref (p_logObserve dist (deref aValue)) in
let aDrift = ref (None ()) in

let bValue = ref (p_sample dist) in
let bWeight = ref (p_logObserve dist (deref aValue)) in
let bDrift = ref (None ()) in

let resValue = ref (and (deref aValue) (deref bValue)) in

modref aDrift (Some (lam. dist));
modref bDrift (Some (lam. dist));

let resampleA = lam st.
  -- === From update for `a`, use suffix `_a` for uniqueness ===
  let prevWeight_a = deref aWeight in
  let prevValue_a = deref aValue in
  -- NOTE(vipa, 2025-09-04): This bit is inside the `if` in
  -- `deltaWeight`, we need to lift the `let`s to preserve side-effect
  -- order of the reference stuff
  let drift_a = optionGetOrElse (lam. error "Impossible") (deref aDrift) in
  let kernel_a = drift_a prevValue_a in
  let proposal_a = p_sample kernel_a in
  let reverseKernel_a = drift_a proposal_a in
  let newWeight_a = p_logObserve dist proposal_a in
  let prevToProposalProb_a = p_logObserve kernel_a proposal_a in
  let proposalToPrevProb_a = p_logObserve reverseKernel_a prevValue_a in
  modref aWeight newWeight_a;

  -- === From update for `b` ===
  -- NOTE(vipa, 2025-09-04): These are unused, but have side-effects,
  -- so I assume we can't easily remove them. Shouldn't matter, I
  -- suspect OCaml will easily remove them later.
  let prevWeight_b = deref bWeight in
  let prevValue_b = deref bValue in

  -- === From update for `res` ===
  let prevValue_res = deref resValue in
  let value_res = and (deref aValue) (deref bValue) in
  modref resValue value_res;

  ( { permanentWeight = st.permanentWeight
    , temporaryWeight = addf (subf newWeight_a prevWeight_a) (subf proposalToPrevProb_a prevToProposalProb_a)
    }
  , lam.
    -- === Reset from update for `a`
    modref aValue prevValue_a;
    modref aWeight prevWeight_a;

    -- === Reset from update for `b`

    -- === Reset from update for `res`
    modref resValue prevValue_res
  ) in

let resampleB = lam st.
  -- === From update for `a` ===
  let prevWeight_a = deref aWeight in
  let prevValue_a = deref aValue in

  -- === From update for `b` ===
  let prevWeight_b = deref bWeight in
  let prevValue_b = deref bValue in
  -- NOTE(vipa, 2025-09-04): This bit is inside the `if` in
  -- `deltaWeight`, we need to lift the `let`s to preserve side-effect
  -- order of the reference stuff
  let drift_b = optionGetOrElse (lam. error "Impossible") (deref aDrift) in
  let kernel_b = drift_b prevValue_b in
  let proposal_b = p_sample kernel_b in
  let reverseKernel_b = drift_b proposal_b in
  let newWeight_b = p_logObserve dist proposal_b in
  let prevToProposalProb_b = p_logObserve kernel_b proposal_b in
  let proposalToPrevProb_b = p_logObserve reverseKernel_b prevValue_b in
  modref aWeight newWeight_b;

  -- === From update for `res` ===
  let prevValue_res = deref resValue in
  let value_res = and (deref aValue) (deref bValue) in
  modref resValue value_res;

  ( { permanentWeight = st.permanentWeight
    , temporaryWeight = addf (subf newWeight_b prevWeight_b) (subf proposalToPrevProb_b prevToProposalProb_b)
    }
  , lam.
    -- === Reset from update for `a`

    -- === Reset from update for `b`
    modref bValue prevValue_b;
    modref bWeight prevWeight_b;

    -- === Reset from update for `res`
    modref resValue prevValue_res
  ) in

let resampleBoth = lam st.
  -- === From update for `a`, use suffix `_a` for uniqueness ===
  let prevWeight_a = deref aWeight in
  let prevValue_a = deref aValue in
  -- NOTE(vipa, 2025-09-04): This bit is inside the `if` in
  -- `deltaWeight`, we need to lift the `let`s to preserve side-effect
  -- order of the reference stuff
  let drift_a = optionGetOrElse (lam. error "Impossible") (deref aDrift) in
  let kernel_a = drift_a prevValue_a in
  let proposal_a = p_sample kernel_a in
  let reverseKernel_a = drift_a proposal_a in
  let newWeight_a = p_logObserve dist proposal_a in
  let prevToProposalProb_a = p_logObserve kernel_a proposal_a in
  let proposalToPrevProb_a = p_logObserve reverseKernel_a prevValue_a in
  modref aWeight newWeight_a;

  -- === From update for `b` ===
  let prevWeight_b = deref bWeight in
  let prevValue_b = deref bValue in
  -- NOTE(vipa, 2025-09-04): This bit is inside the `if` in
  -- `deltaWeight`, we need to lift the `let`s to preserve side-effect
  -- order of the reference stuff
  let drift_b = optionGetOrElse (lam. error "Impossible") (deref aDrift) in
  let kernel_b = drift_b prevValue_b in
  let proposal_b = p_sample kernel_b in
  let reverseKernel_b = drift_b proposal_b in
  let newWeight_b = p_logObserve dist proposal_b in
  let prevToProposalProb_b = p_logObserve kernel_b proposal_b in
  let proposalToPrevProb_b = p_logObserve reverseKernel_b prevValue_b in
  modref aWeight newWeight_b;

  -- === From update for `res` ===
  let prevValue_res = deref resValue in
  let value_res = and (deref aValue) (deref bValue) in
  modref resValue value_res;

  ( { permanentWeight = st.permanentWeight
    , temporaryWeight = addf
      (addf (subf newWeight_a prevWeight_a) (subf proposalToPrevProb_a prevToProposalProb_a))
      (addf (subf newWeight_b prevWeight_b) (subf proposalToPrevProb_b prevToProposalProb_b))
    }
  , lam.
    -- === Reset from update for `a`
    modref aValue prevValue_a;
    modref aWeight prevWeight_a;

    -- === Reset from update for `b`
    modref bValue prevValue_b;
    modref bWeight prevWeight_b;

    -- === Reset from update for `res`
    modref resValue prevValue_res
  ) in

()
