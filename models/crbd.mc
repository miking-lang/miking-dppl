-- Temporary implementation of CRBD example model, directly creating the AST.
-- TODO(dlunde,2020-10-19) Parse this as a regular program when support is
-- added for this in Miking.

include "math.mc"

-- TODO(dlunde,2020-10-19): I very much dislike using ".." in includes. I guess
-- we can fix this (by, e.g., adding the root of the repo to the path) when the
-- build procedure gets more mature.
include "../coreppl/coreppl.mc"

let crbd = use MExprPPL in

  let tytree_ = tyvar_ "Tree" in

  let tyleafrec_ = tyrecord_ [("age", tyfloat_)] in
  let leaf_ = lam age.
      conapp_ "Leaf" (record_ [("age", float_ age)])
  in

  let tynoderec_ =
    tyrecord_ [("age", tyfloat_), ("l", tytree_), ("r", tytree_)] in
  let node_ = lam age. lam left. lam right.
      conapp_ "Node" (record_ [("age", float_ age),
                               ("l", left),
                               ("r", right)])
  in

  let tree =
    node_ 1.0 (leaf_ 0.0) (leaf_ 0.0)
  in

  let crbdGoesUndetected = reclet_ "crbdGoesUndetected"
    (tyarrows_ [tyfloat_, tyfloat_, tyfloat_, tybool_])
    (lams_ [
      ("startTime", tyfloat_),
      ("lambda", tyfloat_),
      ("mu", tyfloat_)
    ]
      (bindall_ [
        ulet_ "t" (assume_ (exp_ (addf_ (var_ "lambda") (var_ "mu")))),
        ulet_ "currentTime" (subf_ (var_ "startTime") (var_ "t")),
        if_ (ltf_ (var_ "currentTime") (float_ 0.0))
          false_
          (bindall_ [
            ulet_ "speciation"
              (assume_ (bern_
                (divf_ (var_ "lambda") (addf_ (var_ "lambda") (var_ "mu"))))),
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

  let simBranch = reclet_ "simBranch"
    (tyarrows_ [tyfloat_, tyfloat_, tyfloat_, tyfloat_, tyunit_])
    (lams_ [
      ("startTime", tyfloat_),
      ("stopTime", tyfloat_),
      ("lambda", tyfloat_),
      ("mu", tyfloat_)
    ]
     (bindall_ [
       ulet_ "t" (assume_ (exp_ (var_ "lambda"))),
       ulet_ "currentTime" (subf_ (var_ "startTime") (var_ "t")),
       if_ (ltf_ (var_ "currentTime") (float_ 0.0))
         unit_
         (bind_
           (ulet_ "TMP" (weight_ (app_ (var_ "log") (float_ 2.0))))
           (if_ (not_ (appf3_ (var_ "crbdGoesUndetected") (var_ "currentTime")
                         (var_ "lambda") (var_ "mu")))
             (weight_ (app_ (var_ "log") (float_ 0.0)))
             (appf4_ (var_ "simBranch")
                (var_ "currentTime") (var_ "stopTime")
                (var_ "lambda") (var_ "mu"))))
     ]))
  in

  let simTree =
    let getAge = lam tree.
      match_ tree (pcon_ "Leaf" (prec_ [("age",(pvar_ "age"))]))
        (var_ "age")
        (match_ tree (pcon_ "Node" (prec_ [("age",(pvar_ "age"))]))
           (var_ "age") never_)
    in
    reclet_ "simTree"
      (tyarrows_ [tyvar_ "Tree", tyvar_ "Tree", tyfloat_, tyfloat_, tyunit_])
      (lams_ [
        ("tree", tyvar_ "Tree"),
        ("parent", tyvar_ "Tree"),
        ("lambda", tyfloat_ ),
        ("mu", tyfloat_)
      ]
        (bindall_ [
           ulet_ "pAge" (getAge (var_ "parent")),
           ulet_ "tAge" (getAge (var_ "tree")),
           ulet_ "TMP"
             (weight_
               (mulf_ (negf_ (var_ "mu"))
                  (subf_ (var_ "pAge") (var_ "tAge")))),
           -- ulet_ "TMP" (resample_),
           ulet_ "TMP"
             (appf4_ (var_ "simBranch")
                   (var_ "pAge") (var_ "tAge")
                   (var_ "lambda") (var_ "mu")),
           match_ (var_ "tree")
             (pcon_ "Node" (prec_ [("l",(pvar_ "left")),("r",(pvar_ "right"))]))
             (bindall_ [
               ulet_ "TMP" (weight_ (app_ (var_ "log") (var_ "lambda"))),
               -- ulet_ "TMP" (resample_),
               ulet_ "TMP"
                 (appf4_ (var_ "simTree") (var_ "left")
                    (var_ "tree") (var_ "lambda") (var_ "mu")),
               (appf4_ (var_ "simTree") (var_ "right")
                  (var_ "tree") (var_ "lambda") (var_ "mu"))
             ])
             unit_
         ]))
  in

  bindall_ [
    ulet_ "log" (lam_ "t" tyfloat_ (float_ 0.0)), -- TODO Add actual log implementation

    type_ "Tree" tyunknown_, -- TODO Should not be unit

    condef_ "Leaf" (tyarrow_ tyleafrec_ tytree_),
    condef_ "Node" (tyarrow_ tynoderec_ tytree_),

    ulet_ "tree" tree,

    crbdGoesUndetected,

    simBranch,
    simTree,

    ulet_ "lambda" (float_ 0.2),
    ulet_ "mu" (float_ 0.1),

    ulet_ "TMP" (weight_ (app_ (var_ "log") (float_ 2.0))),

    match_ (var_ "tree")
      (pcon_ "Node" (prec_ [("l",(pvar_ "left")),("r",(pvar_ "right"))]))
      (bindall_ [
         ulet_ "TMP" (appf4_ (var_ "simTree") (var_ "left")
                     (var_ "tree") (var_ "lambda") (var_ "mu")),
         ulet_ "TMP" (appf4_ (var_ "simTree") (var_ "right")
                     (var_ "tree") (var_ "lambda") (var_ "mu")),
         tuple_ [(var_ "lambda"), (var_ "mu")]
      ])
      (tuple_ [(var_ "lambda"), (var_ "mu")])
  ]

mexpr

use MExprPPL in

-- print (expr2str crbd)

utest expr2str crbd with () using (lam. lam. true) in

()

