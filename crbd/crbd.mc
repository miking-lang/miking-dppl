-- Temporary implementation of CRBD example model, directly creating the AST.
-- TODO(dlunde,2020-10-19) Parse this as a regular program when support is
-- added for this in Miking.

include "math.mc"

-- TODO(dlunde,2020-10-19): I very much dislike using ".." in includes. I guess
-- we can fix this (by, e.g., adding the root of the repo to the path) when the
-- build procedure gets more mature.
include "../coreppl/ast.mc"
include "../coreppl/ast-builder.mc"

-- TODO(dlunde,2020-11-11): Type annotate everything manually. This can later
-- be done by the type checker.

let crbd = use CorePPL in

  let tytree_ = tyvar_ "Tree" in

  let tyleafrec_ = tyrecord_ [("age", tyfloat_)] in
  let leaf_ = lam age.
    asc_ tytree_
      (conapp_ "Leaf" (asc_ tyleafrec_ (record_ [("age", float_ age)])))
  in

  let tynoderec_ =
    tyrecord_ [("age", tyfloat_), ("l", tytree_), ("r", tytree_)] in
  let node_ = lam age. lam left. lam right.
    asc_ tytree_
      (conapp_ "Node" (asc_ tynoderec_ (record_ [("age", float_ age),
                                                 ("l", left),
                                                 ("r", right)])))
  in

  let tree =
    node_ 1.0 (leaf_ 0.0) (leaf_ 0.0)
  in

  let crbdGoesUndetected =
    lams_ [("startTime", tyfloat_), ("lambda", tyfloat_), ("mu", tyfloat_)]
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
      ])
  in

  let simBranch =
    ulams_ ["startTime", "stopTime", "lambda", "mu"]
     (bindall_ [
       ulet_ "t" (sampleExp_ (var_ "lambda")),
       ulet_ "currentTime" (subf_ (var_ "startTime") (var_ "t")),
       if_ (ltf_ (var_ "currentTime") (float_ 0.0))
         unit_
         (bind_
           (ulet_ "_" (weight_ (app_ (var_ "log") (float_ 2.0))))
           (if_ (not_ (appf3_ (var_ "crbdGoesUndetected") (var_ "currentTime")
                         (var_ "lambda") (var_ "mu")))
             (weight_ (app_ (var_ "log") (float_ 0.0)))
             (appf4_ (var_ "simBranch")
                (var_ "currentTime") (var_ "stopTime")
                (var_ "lambda") (var_ "mu"))))
     ])
  in

  let simTree =
    let getAge = lam tree.
      match_ tree (pcon_ "Leaf" (prec_ [("age",(pvar_ "age"))]))
        (var_ "age")
        (match_ tree (pcon_ "Node" (prec_ [("age",(pvar_ "age"))]))
           (var_ "age") never_)
    in
    ulams_ ["tree", "parent", "lambda", "mu"]
      (bindall_ [
         ulet_ "pAge" (getAge (var_ "parent")),
         ulet_ "tAge" (getAge (var_ "tree")),
         ulet_ "_"
           (weight_
             (mulf_ (negf_ (var_ "mu"))
                (subf_ (var_ "pAge") (var_ "tAge")))),
         ulet_ "_" (resample_),
         ulet_ "_"
           (appf4_ (var_ "simBranch")
                 (var_ "pAge") (var_ "tAge")
                 (var_ "lambda") (var_ "mu")),
         match_ (var_ "tree")
           (pcon_ "Node" (prec_ [("l",(pvar_ "left")),("r",(pvar_ "right"))]))
           (bindall_ [
             ulet_ "_" (weight_ (app_ (var_ "log") (var_ "lambda"))),
             ulet_ "_" (resample_),
             ulet_ "_"
               (appf4_ (var_ "simTree") (var_ "left")
                  (var_ "tree") (var_ "lambda") (var_ "mu")),
             (appf4_ (var_ "simTree") (var_ "right")
                (var_ "tree") (var_ "lambda") (var_ "mu"))
           ])
           unit_
       ])
  in

  bindall_ [
    let_ "log" tyunit_ unit_, -- TODO Need log implementation?

    type_ "Tree" tyunit_, -- TODO Should not be unit

    condef_ "Leaf" (tyarrow_ tyleafrec_ tytree_),
    condef_ "Node" (tyarrow_ tynoderec_ tytree_),

    ulet_ "tree" tree,

    reclet_ "crbdGoesUndetected"
      (tyarrows_ [tyfloat_, tyfloat_, tyfloat_, tybool_]) crbdGoesUndetected,

    ureclet_ "simBranch" simBranch,
    ureclet_ "simTree" simTree,

    ulet_ "lambda" (float_ 0.2),
    ulet_ "mu" (float_ 0.1),

    ulet_ "_" (weight_ (app_ (var_ "log") (float_ 2.0))),

    match_ (var_ "tree")
      (pcon_ "Node" (prec_ [("l",(pvar_ "left")),("r",(pvar_ "right"))]))
      (bindall_ [
         ulet_ "_" (appf4_ (var_ "simTree") (var_ "left")
                     (var_ "tree") (var_ "lambda") (var_ "mu")),
         ulet_ "_" (appf4_ (var_ "simTree") (var_ "right")
                     (var_ "tree") (var_ "lambda") (var_ "mu")),
         tuple_ [(var_ "lambda"), (var_ "mu")]
      ])
      unit_
  ]

