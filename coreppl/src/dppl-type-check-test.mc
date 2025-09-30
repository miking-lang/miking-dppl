/-

  Extended testing of new DPPL type-checker and its parser additions

 -/

include "result.mc"
include "either.mc"
include "./dppl-type-check.mc"
include "./parser.mc"

lang TestLang = DTCTypeOf + MExprPPL end

mexpr

use TestLang in

let _D = ModD () in
let _R = ModR () in
let _A = ModA () in
let _P = ModP () in
let _C = ModC () in

let arrce = lam ps. lam ret. foldr (lam t. lam to. tyarrowXe_ t.0 to t.1 t.2) ret ps in
let arrc = lam ps. arrce (map (lam p. (p.0, p.1, _D)) ps) in
let arre = lam ps. arrce (map (lam p. (p.0, [], p.1)) ps) in
let arr = lam ps. arrce (map (lam p. (p, [], _D)) ps) in
let flt = tyfloatc_ in
let fltX = tyfloatX_ in

let _typeOf = lam env. lam prog.
  (result.consume
     (typeOf (dtcEnvOfSeq env)
        (use DPPLParser in
         decorateTypesExn
           (symbolizeAllowFree
              (makeKeywords
                 (parseMExprStringExn
                    ({
                      keywords = pplKeywords,
                      allowFree = true,
                      builtin = cpplBuiltin
                    }) prog)))))).1 in

let fi = lam row1. lam col1. lam row2. lam col2.
  Info { filename = "", row1 = row1, col1 = col1, row2 = row2, col2 = col2 } in

let eq =
  eitherEq
    (lam l. lam r.
      forAll (lam x. x)
        (zipWith (lam l. lam r.
          and
            ( switch (typeErrorInfo l, typeErrorInfo r)
              case (Info l, Info r) then
              let seq = lam r. [r.row1, r.row2, r.col1, r.col2] in
              let eq = lam l. lam r. allb (zipWith eqi (seq l) (seq r)) in
              eq l r
              case (NoInfo _, NoInfo _) then true
              case _ then false
              end )
            (eqi (constructorTag l) (constructorTag r)))
           l r))
    (tupleEq2 dtcEqe eqType)
in

let toString =
  eitherEither
    (lam errs.
      strJoin "\n"
        (map (lam err. match typeErrorToMsg err with (info, msg) in
                     strJoin "\n" [msg, info2str (typeErrorInfo err)])
           errs))
    (lam t. join [":", dtcEffectToString t.0, " ", type2str t.1])
in
let onFail = utestDefaultToString toString toString in

let env = [] in
utest _typeOf env (strJoin "\n" [
  "let a : FloatA = 0. in",
  "let b : FloatP = 0. in",
  "let c : FloatC = 0. in",
  "(a, b, c)" ])
  with Right (_D, tytuple_ (map fltX [[], [], []]) )
  using eq else onFail in

let env = [] in
utest _typeOf env (strJoin "\n" [
  "(lam a : FloatA. (lam b : Float. (a, b)) 0.) 0." ])
  with Right (_D, tytuple_ (map fltX [[], []]) )
  using eq else onFail in

let env = [
  (nameNoSym "test", arr [tytuple_ (map flt [_A, _P, _C])] tyunit_)
] in
utest _typeOf env (strJoin "\n" [
  "let a : FloatA = 0. in",
  "let b : FloatP = 0. in",
  "let c : FloatC = 0. in",
  "test (a, b, c)" ])
  with Right (_D, tyunit_ )
  using eq else onFail in

let env = [
  (nameNoSym "test", arr [tytuple_ (map flt [_C, _P, _A])] tyunit_)
] in
utest _typeOf env (strJoin "\n" [
  "let a : FloatA = 0. in",
  "let b : FloatP = 0. in",
  "let c : FloatC = 0. in",
  "test (a, b, c)" ])
  with Left [DTCArgError (fi 4 5 4 14, None ())]
  using eq else onFail in

let env = [] in
utest _typeOf env "lam x : FloatA. addf 1. 1."
  with Right (_D, arrc [(flt _A, [])] (fltX []))
  using eq else onFail in

let env = [] in
utest _typeOf env "lam x : FloatP. if gtf x 0. then 1. else 2."
  with Right (_D, arrc [(flt _P, [])] (flt _P))
  using eq else onFail in

let env = [ (nameNoSym "y", flt _C) ] in
utest _typeOf env "lam x : FloatP. if gtf x 0. then x else y"
  with Right (_D, arrc [(flt _P, [_C])] (fltX dtcPC))
  using eq else onFail in

let env = [] in
utest _typeOf env "lam x : FloatPC. if gtf x 0. then x else 2."
  with Left [DTCArgError (fi 1 20 1 28, None ())]
  using eq else onFail in

let env = [] in
utest _typeOf env "lam x : Float. if gtf x 0. then x else 2."
  with Right (_D, arrc [(fltX [], [])] (fltX []))
  using eq else onFail in

let env = [
  (nameNoSym "z", arrc [(fltX [], [])] (tyseq_ (flt _A))),
  (nameNoSym "d", tyseq_ (fltX []))
] in
utest _typeOf env (strJoin "\n" [
  "let regressionModel =",
  "  lam d : [Float].",
  "  lam z : Float -> (ModR [FloatA]).",
  "  lam t : ().",
  "    match (assume (Gamma 1. 1.), assume (Gamma 1. 1.))",
  "    with (theta, nu) in",
  "    iter",
  "      (lam t : (Float, Float).",
  "         match t with (x, y) in observe x (Gaussian y nu))",
  "         (create (length d) (lam i : Int. (get d i, get (z theta) i)));",
  "      theta in",
  "infer (Default ()) (regressionModel d z)" ])
  with Right (_D, tydist_ (fltX []) )
  using eq else onFail in

let env = [
  (nameNoSym "r", fltX [])      -- represents arbritrary float literal which we
                                -- can type with any coeffect modifier.
] in
utest _typeOf env (strJoin "\n" [
  "let y = lam x : FloatA. addf (mulf x x) x in",
  "diff y r 1." ])
  with Right (_D, fltX [])
  using eq else onFail in

let env = [] in
utest _typeOf env "lam x : FloatA. if ltf x 0. then x else subf 0. x"
  with Left [DTCArgError (fi 1 19 1 27, None ())]
  using eq else onFail in

let env = [
  (nameNoSym "r", fltX [])
] in
utest _typeOf env (strJoin "\n" [
  "let y = lam x : FloatP. if ltf x 0. then x else subf 0. x in",
  "diff y r 1." ])
  with Right (_D, fltX [])
  using eq else onFail in

let env = [
  (nameNoSym "t", tydist_ (flt _A)),
  (nameNoSym "r", fltX [])
] in
utest _typeOf env (strJoin "\n" [
  "let y = assume t in",
  "diff (lam x : FloatA. x) r 1." ])
  with Right (_R, fltX [])
  using eq else onFail in

let env = [
  (nameNoSym "r", fltX [])
] in
utest _typeOf env (strJoin "\n" [
  "let w = assume (Wiener ()) in",
  "let z = lam xy: (FloatC, FloatA).",
  "  match xy with (x, y) in addf (w x) y in",
  "diff (lam u : FloatA. z (r, u)) r 1." ])
  with Right (_R, fltX [])
  using eq else onFail in

let env = [
  (nameNoSym "wiener", arrc [(flt _C, [])] (flt _A)),
  (nameNoSym "x", flt _C),
  (nameNoSym "y", flt _A)
] in
utest _typeOf env "addf (wiener (addf x 1.)) y"
  with Right (_D, flt _A)
  using eq else onFail in

let env = [
  (nameNoSym "wiener", arrc [(flt _C, [])] (flt _A))
] in
utest _typeOf env (strJoin "\n" [
  "lam y : FloatA. lam x : FloatC. ",
  "  addf (wiener (addf x 1.)) y" ])
  with Right (_D, arrc [(flt _A, []), (flt _C, [_A])] (flt _A))
  using eq else onFail in

let env = [
  (nameNoSym "wiener", arrc [(flt _C, [])] (flt _A))
] in
utest _typeOf env (strJoin "\n" [
  "lam x : FloatC. lam y : FloatA. ",
  "  addf (wiener (addf x 1.)) y" ])
  with Right (_D, arrc [(flt _C, []), (flt _A, [_C])] (flt _A))
  using eq else onFail in

let env = [
  (nameNoSym "wiener", arrc [(flt _C, [])] (flt _A))
] in
utest _typeOf env (strJoin "\n" [
  "lam z : FloatA. diff (lam y : FloatA.",
  "  (lam x : (FloatC, FloatA). addf (wiener x.0) x.1) (0., y))",
  "  1. z" ])
  with Right (_D, arrc [(flt _A, [])] (flt _A))
  using eq else onFail in

-- let env = [
--   (nameNoSym "times", tyseq_ (fltX [])),
--   (nameNoSym "xy0", tytuple_ [fltX [], fltX []])
-- ] in
-- utest _typeOf env (strJoin "\n" [
--   "let rode = lam t : ().",
--   "  let w = assume (Wiener ()) in",
--   "  let f = lam x : FloatC. lam y : FloatA. subf (sin (w x)) y in",
--   "  map (lam x1 : FloatC. solveode (Default ()) f xy0 x1) times",
--   "  in infer (Default ()) rode"])
--   with Right (_D,  tydist_ (tyseq_ (tytuple_ [fltX [], fltX []])))
--   using eq else onFail in

()
