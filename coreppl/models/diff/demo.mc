-- This is a determinstic model that illustrates the `diff` construct

let model : () -> () = lam.

  let eqSeq = lam eq. lam seq1. lam seq2.
    foldl (lam x. lam y. if x then y else false) true
      (mapi (lam i. lam x. eq x (get seq2 i)) seq1)
  in

  let assert = lam msg. lam x.
    if x then () else error (concat "Assertion failed: " msg)
  in

  -- ┌────────────────────┐
  -- │ Scalar Derivatives │
  -- └────────────────────┘

  -- Total derivative for scalar functions
  let d : (Float -> Float) -> Float -> (Float -> Float) =
    lam f. lam x. diff f x
  in

  let f = lam x. mulf (sin x) (cos x) in
  -- Analytic total derivative
  let df = lam x. lam v.
    mulf v (subf (mulf (cos x) (cos x)) (mulf (sin x) (sin x)))
  in
  let x = 2. in                 -- We compute the derivative in this point
  assert "scalar derivative" (eqf (d f x 1.) (df x 1.));


  -- ┌────────────────────────────────┐
  -- │ Partial Derivatives, Example 1 │
  -- └────────────────────────────────┘

  let d : ([Float] -> [Float]) -> [Float] -> ([Float] -> [Float]) =
    lam f. lam x. let t = diff f x in t
  in

  let g = lam x. [addf (f (get x 0)) (f (get x 1)), f (get x 1)] in
  -- Analytic total derivative
  let dg = lam x. lam v. [
    addf (df (get x 0) (get v 0)) (df (get x 1) (get v 1)),
    df (get x 1) (get v 1)
  ] in
  let xs = [2., 3.] in             -- We compute the derivative in this point

  -- First partial derivative
  let v = [1., 0.] in
  assert "partial derivatives 1.1" (eqSeq eqf (d g xs v) (dg xs v));

  -- Second partial derivative
  let v = [0., 1.] in
  assert "partial derivatives 1.2" (eqSeq eqf (d g xs v) (dg xs v));

  -- Jacobian-vector-product
  let v = [2., 3.] in
  assert "partial derivatives 1.JvP" (eqSeq eqf (d g xs v) (dg xs v));


  -- ┌────────────────────────────────┐
  -- │ Partial Derivatives, Example 2 │
  -- └────────────────────────────────┘

  let d :
    ((Float, [Float]) -> {a : [Float]}) -> (Float, [Float]) ->
      ((Float, [Float]) -> {a : [Float]})
    = lam f. lam x. let t = diff f x in t
  in

  let h = lam x. match x with (x0, [x1, x2]) in { a = [f x0, f x1, f x2] } in
  -- Analytic total derivative
  let dh = lam x. lam v.
    match (x, v) with ((x0, [x1, x2]), (v0, [v1, v2])) in
    { a = [df x0 v0, df x1 v1, df x2 v2] }
  in
  let x = (1., [2., 3.]) in    -- We compute the derivative in this point

  -- First partial derivative
  let v = (1., [0., 0.]) in
  assert "partial derivatives 2.1"
    (eqSeq eqf (d h x v).a (dh x v).a);

  -- Second partial derivative
  let v = (0., [1., 0.]) in
  assert "partial derivatives 2.2"
    (eqSeq eqf (d h x v).a (dh x v).a);

  -- Third partial derivative
  let v = (0., [0., 1.]) in
  assert "partial derivatives 2.3"
    (eqSeq eqf (d h x v).a (dh x v).a);

  ()

mexpr
model ()
