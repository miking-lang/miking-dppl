include "../../test.mc"

include "seq.mc"
include "sys.mc"
include "string.mc"
include "common.mc"

let resStaticDelay: CpplRes -> Float = lam cpplRes.
  match cpplRes.extra with Some extra then extra else
  error "cannot compare without log-likelihood"

let eqStaticDelay = lam eps. eqfApprox eps
mexpr
let s = 2e-2 in
let e = eqStaticDelay s in
let r = resStaticDelay in

--- TESTS FOR FLAT PROGRAM, NO LOOPS, NO PLATES ---
let t1 = testCpplMExpr "static-delay/test1.dppl" 10000 in
let t1s = testCpplMExpr "static-delay/test1.dppl" 1 in
utest r (t1s 0   "-m is-lw --static-delay --no-print-samples --extract-simplification inline --cps none") with r (t1 0   "-m is-lw --no-print-samples") using e in

-- no conj --
let t2 = testCpplMExpr "static-delay/test2.dppl" 1 in
utest r (t2 0   "-m is-lw --static-delay --no-print-samples --extract-simplification inline --cps none") with r (t2 0   "-m is-lw --no-print-samples") using e in

-- affine transformation --
let t3 = testCpplMExpr "static-delay/test3.dppl" 10000 in
utest r (t3 0   "-m is-lw --static-delay --no-print-samples --extract-simplification inline --cps none") with r (t3 0   "-m is-lw --no-print-samples") using e in

-- no conj because of cycle --
let t4 = testCpplMExpr "static-delay/test4.dppl" 1 in
utest r (t4 0   "-m is-lw --static-delay --no-print-samples --extract-simplification inline --cps none") with r (t4 0   "-m is-lw --no-print-samples") using e in

-- chain --
let t5 = testCpplMExpr "static-delay/test5.dppl" 30000 in
let t5s = testCpplMExpr "static-delay/test5.dppl" 1 in
utest r (t5s 0   "-m is-lw --static-delay --no-print-samples --extract-simplification inline --cps none") with r (t5 0   "-m is-lw --no-print-samples") using e in

-- tree --
let et = eqStaticDelay 2e-1 in
let t6 = testCpplMExpr "static-delay/test6.dppl" 1000000 in
let t6s = testCpplMExpr "static-delay/test6.dppl" 10000 in
utest r (t6s 0   "-m is-lw --static-delay --no-print-samples --extract-simplification inline --cps none") with r (t6 0   "-m is-lw --no-print-samples") using et in

-- lambda --
let t7 = testCpplMExpr "static-delay/test7.dppl" 10000 in
let t7s = testCpplMExpr "static-delay/test7.dppl" 1 in
utest r (t7s 0   "-m is-lw --static-delay --no-print-samples --extract-simplification inline --cps none") with r (t7 0   "-m is-lw --no-print-samples") using e in

--- TESTS FOR LISTS ---
let t8 = testCpplMExpr "static-delay/test8.dppl" 10000 in
let t8s = testCpplMExpr "static-delay/test8.dppl" 1 in
utest r (t8s 0   "-m is-lw --static-delay --no-print-samples --extract-simplification inline --cps none") with r (t8 0   "-m is-lw --no-print-samples") using e in

let t9 = testCpplMExpr "static-delay/test9.dppl" 100000 in
let t9s = testCpplMExpr "static-delay/test9.dppl" 100 in
utest r (t9s 0   "-m is-lw --static-delay --no-print-samples --extract-simplification inline --cps none") with r (t9 0   "-m is-lw --no-print-samples") using e in

-- TESTS FOR PLATES --

let t10 = testCpplMExpr "static-delay/test10.dppl" 10000 in
let t10s = testCpplMExpr "static-delay/test10.dppl" 1000 in
utest r (t10s 0   "-m is-lw --static-delay --no-print-samples --extract-simplification inline --cps none") with r (t10 0   "-m is-lw --no-print-samples") using e in

let t11 = testCpplMExpr "static-delay/test11.dppl" 1000000 in
let t11s = testCpplMExpr "static-delay/test11.dppl" 1000 in
let et = eqStaticDelay 2e-1 in
utest r (t11s 0   "-m is-lw --static-delay --no-print-samples --extract-simplification inline --cps none") with r (t11 0   "-m is-lw --no-print-samples --extract-simplification inline") using et in

let t12 = testCpplMExpr "static-delay/test12.dppl" 10000 in
let t12s = testCpplMExpr "static-delay/test12.dppl" 1 in
utest r (t12s 0   "-m is-lw --static-delay --no-print-samples --extract-simplification inline --cps none") with r (t12 0   "-m is-lw --no-print-samples") using e in
-- lists with plates 

let t13 = testCpplMExpr "static-delay/test13.dppl" 10000 in
let t13s = testCpplMExpr "static-delay/test13.dppl" 1 in
utest r (t13s 0   "-m is-lw --static-delay --no-print-samples --extract-simplification inline --cps none") with r (t13 0   "-m is-lw --no-print-samples") using e in

let t14 = testCpplMExpr "static-delay/test14.dppl" 10000 in
let t14s = testCpplMExpr "static-delay/test14.dppl" 1 in
utest r (t14s 0   "-m is-lw --static-delay --no-print-samples --extract-simplification inline --cps none") with r (t14 0   "-m is-lw --no-print-samples") using e in

let t15 = testCpplMExpr "static-delay/test15.dppl" 10000 in
let t15s = testCpplMExpr "static-delay/test15.dppl" 1000 in
utest r (t15s 0   "-m is-lw --static-delay --no-print-samples --extract-simplification inline --cps none") with r (t15 0   "-m is-lw --no-print-samples") using e in

let t16 = testCpplMExpr "static-delay/test16.dppl" 10000 in
let t16s = testCpplMExpr "static-delay/test16.dppl" 1 in
utest r (t16s 0   "-m is-lw --static-delay --no-print-samples --extract-simplification inline --cps none") with r (t16 0   "-m is-lw --no-print-samples") using e in

let t17 = testCpplMExpr "static-delay/test17.dppl" 100000 in
let t17s = testCpplMExpr "static-delay/test17.dppl" 1 in
utest r (t17s 0   "-m is-lw --static-delay --no-print-samples --extract-simplification inline --cps none") with r (t17 0   "-m is-lw --no-print-samples") using e in

let t18 = testCpplMExpr "static-delay/test18.dppl" 100000 in
let t18s = testCpplMExpr "static-delay/test18.dppl" 100 in
utest r (t18s 0   "-m is-lw --static-delay --no-print-samples --extract-simplification inline --cps none") with r (t18 0   "-m is-lw --no-print-samples") using e in

()