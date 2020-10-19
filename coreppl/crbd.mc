-- Temporary implementation of CRBD example model, directly creating the AST.
-- TODO Parse this as a regular program when support is added for this in
-- Miking.

include "math.mc"

include "ast.mc"
include "symbolize.mc"
include "ast-builder.mc"
include "pprint.mc"
include "anf.mc"

mexpr
use PPLCore in

let leaf_ = lam age.
  conapp_ "Leaf" (record_ [("age", float_ age)])
in

let node_ = lam age. lam left. lam right.
  conapp_ "Node" (record_ [("age", float_ age),
                           ("l", left),
                           ("r", right)])
in

let crbdGoesUndetected =
  reclet_ "crbdGoesUndetected"
    (ulams_ ["startTime", "lambda", "mu"]
      (bindall_ [
        let_ "t" (sampleExp_ (addi_ (var_ "lambda") (var_ "mu"))),
        let_ "currentTime" (subf_ (var_ "startTime") (var_ "t")),
        if_ (ltf_ (var_ "currentTime") (float_ 0.0))
          false_
          (bindall_ [
            let_ "speciation"
              (sampleBern_
                (divf_ (var_ "lambda") (addf_ (var_ "lambda") (var_ "mu")))),
            if_ (not_ (var_ "speciation"))
              true_
              (and_
                 (appf3_ (var_ "crbdGoesUndetected")
                    (var_ "currentTime") (var_ "lambda") (var_ "mu"))
                 (appf3_ (var_ "crbdGoesUndetected")
                    (var_ "currentTime") (var_ "lambda") (var_ "mu"))
              )
          ])
      ]))
in

let simBranch =
  reclet_ "simBranch"
    (ulams_ ["startTime", "stopTime", "lambda", "mu"]
      (bindall_ [
        let_ "t" (sampleExp_ (var_ "lambda")),
        let_ "currentTime" (subf_ (var_ "startTime") (var_ "t")),
        if_ (ltf_ (var_ "currentTime") (float_ 0.0))
          unit_
          (bind_
            (let_ "_" (weight_ (app_ (var_ "log") (float_ 2.0))))
            (if_ (not_ (appf3_ (var_ "crbdGoesUndetected") (var_ "currentTime")
                          (var_ "lambda") (var_ "mu")))
              (weight_ (app_ (var_ "log") (float_ 0.0)))
              (appf4_ (var_ "simBranch")
                 (var_ "currentTime") (var_ "stopTime")
                 (var_ "lambda") (var_ "mu"))))
      ]))
in

let getAge = lam tree.
  match_ tree (pcon_ "Leaf" (prec_ [("age",(pvar_ "age"))]))
    (var_ "age")
    (match_ tree (pcon_ "Node" (prec_ [("age",(pvar_ "age"))]))
       (var_ "age") never_)
in

let simTree =
  reclet_ "simTree"
    (ulams_ ["tree", "parent", "lambda", "mu"]
      (bindall_ [
         let_ "pAge" (getAge (var_ "parent")),
         let_ "tAge" (getAge (var_ "tree")),
         let_ "_"
           (weight_
             (mulf_ (negf_ (var_ "mu"))
                (subf_ (var_ "pAge") (var_ "tAge")))),
         let_ "_"
           (appf4_ (var_ "simBranch")
                 (var_ "pAge") (var_ "tAge")
                 (var_ "lambda") (var_ "mu")),
         match_ (var_ "tree")
           (pcon_ "Node" (prec_ [("l",(pvar_ "left")),("r",(pvar_ "right"))]))
           (bindall_ [
             let_ "_" (weight_ (app_ (var_ "log") (var_ "lambda"))),
             let_ "_"
               (appf4_ (var_ "simTree") (var_ "left")
                  (var_ "tree") (var_ "lambda") (var_ "mu")),
             (appf4_ (var_ "simTree") (var_ "right")
                (var_ "tree") (var_ "lambda") (var_ "mu"))
           ])
           unit_
    ]))
in

let crbd =
  bindall_ [
    let_ "log" unit_, -- TODO Need log implementation?

    ucondef_ "Leaf",
    ucondef_ "Node",

    let_ "tree" (node_ 1.0 (leaf_ 0.0) (leaf_ 0.0)),

    crbdGoesUndetected,
    simBranch,
    simTree,

    let_ "lambda" (float_ 0.2),
    let_ "mu" (float_ 0.1),

    let_ "_" (weight_ (app_ (var_ "log") (float_ 2.0))),

    match_ (var_ "tree")
      (pcon_ "Node" (prec_ [("l",(pvar_ "left")),("r",(pvar_ "right"))]))
      (bindall_ [
         let_ "_" (appf4_ (var_ "simTree") (var_ "left")
                     (var_ "tree") (var_ "lambda") (var_ "mu")),
         let_ "_" (appf4_ (var_ "simTree") (var_ "right")
                     (var_ "tree") (var_ "lambda") (var_ "mu")),
         tuple_ [(var_ "lambda"), (var_ "mu")]
      ])
      never_
  ]
in

-- let t0 = wallTimeMs () in
-- let r1 = symbolize crbd in
-- let t1 = wallTimeMs () in
-- let r2 = normalizeTerm r1 in
-- let t2 = wallTimeMs () in
-- let r3 = expr2str r2 in
-- let t3 = wallTimeMs () in

-- let _ = print "\nSymbolize time: " in
-- let _ = printLn (float2string (subf t1 t0)) in
-- let _ = print "ANF time: " in
-- let _ = printLn (float2string (subf t2 t1)) in
-- let _ = print "expr2str time: " in
-- let _ = printLn (float2string (subf t3 t2)) in
-- expr2str is really slow (3.7 seconds)

let _ = printLn "--- BEFORE ANF ---" in
let _ = printLn (expr2str crbd) in
let _ = printLn "\n--- AFTER SYMBOLIZE AND ANF ---" in
let _ = printLn (expr2str (normalizeTerm (symbolize crbd))) in
()
