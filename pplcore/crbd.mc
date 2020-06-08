-- Temporary implementation of CRBD example model, directly creating the AST.
-- TODO Parse this as a regular program when support is added for this in
-- Miking.

include "ast.mc"
include "pprint.mc"
include "ast-builder.mc"
include "mexpr/ast-builder.mc"
include "math.mc"

mexpr
use PPLCorePrettyPrint in

let leaf_ = lam age.
  app_ (var_ "Leaf") (record_ [{key = "age", value = float_ age}])
in

let node_ = lam age. lam left. lam right.
  app_ (var_ "Node") (record_ [{key = "age", value = float_ age},
                               {key = "l",   value = left},
                               {key = "r",   value = right}])
in

let crbdGoesUndetected =
  ureclet_ "crbdGoesUndetected"
    (ulams_ ["startTime", "lambda", "mu"]
      (bindall_ [
        ulet_ "t" (sampleExp_ (addi_ (var_ "lambda") (var_ "mu"))),
        ulet_ "currentTime" (subf_ (var_ "startTime") (var_ "t")),
        if_ (ltf_ (var_ "currentTime") (float_ 0.0))
          false_
          (bindall_ [
            ulet_ "speciation"
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
  ureclet_ "simBranch"
    (ulams_ ["startTime", "stopTime", "lambda", "mu"]
      (bindall_ [
        ulet_ "t" (sampleExp_ (var_ "lambda")),
        ulet_ "currentTime" (subf_ (var_ "startTime") (var_ "t")),
        if_ (ltf_ (var_ "currentTime") (float_ 0.0))
          unit_
          (bind_
            (ulet_ "_" (weight_ (float_ 0.3))) -- weight(log(2))
            (if_ (not_ (appf3_ (var_ "crbdGoesUndetected") (var_ "currentTime")
                          (var_ "lambda") (var_ "mu")))
              (weight_ (negf_ (float_ inf)))
              (appf4_ (var_ "simBranch")
                 (var_ "currentTime") (var_ "stopTime")
                 (var_ "lambda") (var_ "mu"))))
      ]))
in

-- TODO We need pattern matching on Leaf{age:float} and
-- Node{age:float,l:Tree,r:Tree} here. Specifically, we need it for tree.age
-- and parent.age, and for checking tree.type == 'node' in WebPPL script.
let simTree =
  ureclet_ "simBranch"
    (ulams_ ["tree", "parent", "lambda", "mu"]
      (bindall_ [
         ulet_ "_"
           (weight_
             (mulf_ (negf_ (var_ "mu"))
                    (subf_ (var_ "TODO") (var_ "TODO"))))
    ]))
in

let crbd =
  bindall_ [
    condef_ "Leaf" tydyn_,
    condef_ "Node" tydyn_,
    ulet_ "tree" (node_ 1.0 (leaf_ 0.0) (leaf_ 0.0)),
    crbdGoesUndetected,
    simBranch,
    simTree,
    ulet_ "lambda" (float_ 0.2),
    ulet_ "mu" (float_ 0.1),
    ulet_ "_" (weight_ (float_ 0.3)), -- weight(log(2))
    -- TODO Add pattern matching to deconstruct tree to tree.left and tree.right
    ulet_ "_" (appf4_ (var_ "simTree") (var_ "TODO")
                     (var_ "tree") (var_ "lambda") (var_ "mu")),
    ulet_ "_" (appf4_ (var_ "simTree") (var_ "TODO")
                      (var_ "tree") (var_ "lambda") (var_ "mu")),
    (record_ [{key = "1", value = (var_ "lambda")},
              {key = "2", value = (var_ "mu")}])
  ]
in

printLn (pprintCode 0 crbd)
