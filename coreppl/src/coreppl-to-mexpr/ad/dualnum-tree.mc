include "dual-tree.mc"

type DualNum = Dual Float

let dualnumGenEpsilon : () -> Eps = lam. dualGenEpsilon ()
let dualnumLtEpsilon : Eps -> Eps -> Bool = lam l. lam r. dualLtE l r
let dualnumIsDualNum : DualNum -> Bool = lam x. dualIsDual x
let dualnumEpsilon : DualNum -> Eps = lam x. dualEpsilon x
let dualnumCreatePrimal : Float -> DualNum = lam x. Primal x
let dualnumCreateDual : Eps -> DualNum -> DualNum -> DualNum =
  lam e. lam x. lam xp. dualCreateDual (eqf 0.) e x xp
let dualnumPrimal : Eps -> DualNum -> DualNum = lam e. lam x. dualPrimal e x
let dualnumPrimalRec : DualNum -> Float = lam x. dualPrimalRec x
let dualnumPertubation : Eps -> DualNum -> DualNum =
  lam e. lam x. dualPertubation 0. e x
let dualnumEq = lam l. lam r. dualEq l r

-----------------
-- FOR BREVITY --
-----------------

let _dnum = dualnumCreateDual
let _ltE = dualLtE

mexpr

let eq = dualnumEq eqf in

let e0 = 0 in
let e1 = 1 in
let e2 = 2 in
let e3 = 3 in

let num0 = Primal 0. in
let num1 = Primal 1. in
let num2 = Primal 2. in
let num3 = Primal 3. in
let num4 = Primal 4. in
let num6 = Primal 6. in
let num8 = Primal 8. in
let dnum112 = _dnum e1 num1 num2 in
let dnum212 = _dnum e2 num3 num4 in
let dnum0 = _dnum e0 in
let dnum1 = _dnum e1 in
let dnum134 = dnum1 num3 num4 in
let dnum036 = dnum0 num3 num6 in
let dnum048 = dnum0 num4 num8 in

utest dualPrimalRec num0 with 0. using eqf in
utest dualPrimalRec dnum134 with 3. using eqf in
utest dualPrimalRec (dnum1 dnum036 dnum048) with 3. using eqf in
utest dualIsDual num1 with false in
utest dualIsDual dnum112 with true in
utest dualEpsilon dnum112 with e1 in
utest dualEpsilon (_dnum e3 dnum112 dnum212) with e3 in
utest dualPrimal e1 dnum112 with num1 using eq in
utest dualnumPertubation e1 dnum112 with num2 using eq in
utest dualPrimal e2 dnum112 with dnum112 using eq in
utest dualnumPertubation e2 dnum112 with num0 using eq in

()
