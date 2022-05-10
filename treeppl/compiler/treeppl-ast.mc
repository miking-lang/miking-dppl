include "seq.mc"

include "parser/ll1.mc"

include "parser/breakable.mc"

include "tppl-lexer-extensions.mc"

lang TreePPLBaseAst

  syn FileTppl =
  syn DeclTppl =
  syn TypeTppl =
  syn StmtTppl =
  syn ExprTppl =

  sem smapAccumL_FileTppl_FileTppl : ((a) -> ((FileTppl) -> ((a, FileTppl)))) -> ((a) -> ((FileTppl) -> ((a, FileTppl))))
  sem smapAccumL_FileTppl_FileTppl f acc =
  | x ->
    (acc, x)

  sem smap_FileTppl_FileTppl : ((FileTppl) -> (FileTppl)) -> ((FileTppl) -> (FileTppl))
  sem smap_FileTppl_FileTppl f =
  | x ->
    (smapAccumL_FileTppl_FileTppl
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_FileTppl_FileTppl : ((a) -> ((FileTppl) -> (a))) -> ((a) -> ((FileTppl) -> (a)))
  sem sfold_FileTppl_FileTppl f acc =
  | x ->
    (smapAccumL_FileTppl_FileTppl
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_FileTppl_DeclTppl : ((a) -> ((DeclTppl) -> ((a, DeclTppl)))) -> ((a) -> ((FileTppl) -> ((a, FileTppl))))
  sem smapAccumL_FileTppl_DeclTppl f acc =
  | x ->
    (acc, x)

  sem smap_FileTppl_DeclTppl : ((DeclTppl) -> (DeclTppl)) -> ((FileTppl) -> (FileTppl))
  sem smap_FileTppl_DeclTppl f =
  | x ->
    (smapAccumL_FileTppl_DeclTppl
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_FileTppl_DeclTppl : ((a) -> ((DeclTppl) -> (a))) -> ((a) -> ((FileTppl) -> (a)))
  sem sfold_FileTppl_DeclTppl f acc =
  | x ->
    (smapAccumL_FileTppl_DeclTppl
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_FileTppl_TypeTppl : ((a) -> ((TypeTppl) -> ((a, TypeTppl)))) -> ((a) -> ((FileTppl) -> ((a, FileTppl))))
  sem smapAccumL_FileTppl_TypeTppl f acc =
  | x ->
    (acc, x)

  sem smap_FileTppl_TypeTppl : ((TypeTppl) -> (TypeTppl)) -> ((FileTppl) -> (FileTppl))
  sem smap_FileTppl_TypeTppl f =
  | x ->
    (smapAccumL_FileTppl_TypeTppl
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_FileTppl_TypeTppl : ((a) -> ((TypeTppl) -> (a))) -> ((a) -> ((FileTppl) -> (a)))
  sem sfold_FileTppl_TypeTppl f acc =
  | x ->
    (smapAccumL_FileTppl_TypeTppl
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_FileTppl_StmtTppl : ((a) -> ((StmtTppl) -> ((a, StmtTppl)))) -> ((a) -> ((FileTppl) -> ((a, FileTppl))))
  sem smapAccumL_FileTppl_StmtTppl f acc =
  | x ->
    (acc, x)

  sem smap_FileTppl_StmtTppl : ((StmtTppl) -> (StmtTppl)) -> ((FileTppl) -> (FileTppl))
  sem smap_FileTppl_StmtTppl f =
  | x ->
    (smapAccumL_FileTppl_StmtTppl
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_FileTppl_StmtTppl : ((a) -> ((StmtTppl) -> (a))) -> ((a) -> ((FileTppl) -> (a)))
  sem sfold_FileTppl_StmtTppl f acc =
  | x ->
    (smapAccumL_FileTppl_StmtTppl
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_FileTppl_ExprTppl : ((a) -> ((ExprTppl) -> ((a, ExprTppl)))) -> ((a) -> ((FileTppl) -> ((a, FileTppl))))
  sem smapAccumL_FileTppl_ExprTppl f acc =
  | x ->
    (acc, x)

  sem smap_FileTppl_ExprTppl : ((ExprTppl) -> (ExprTppl)) -> ((FileTppl) -> (FileTppl))
  sem smap_FileTppl_ExprTppl f =
  | x ->
    (smapAccumL_FileTppl_ExprTppl
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_FileTppl_ExprTppl : ((a) -> ((ExprTppl) -> (a))) -> ((a) -> ((FileTppl) -> (a)))
  sem sfold_FileTppl_ExprTppl f acc =
  | x ->
    (smapAccumL_FileTppl_ExprTppl
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_DeclTppl_FileTppl : ((a) -> ((FileTppl) -> ((a, FileTppl)))) -> ((a) -> ((DeclTppl) -> ((a, DeclTppl))))
  sem smapAccumL_DeclTppl_FileTppl f acc =
  | x ->
    (acc, x)

  sem smap_DeclTppl_FileTppl : ((FileTppl) -> (FileTppl)) -> ((DeclTppl) -> (DeclTppl))
  sem smap_DeclTppl_FileTppl f =
  | x ->
    (smapAccumL_DeclTppl_FileTppl
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_DeclTppl_FileTppl : ((a) -> ((FileTppl) -> (a))) -> ((a) -> ((DeclTppl) -> (a)))
  sem sfold_DeclTppl_FileTppl f acc =
  | x ->
    (smapAccumL_DeclTppl_FileTppl
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_DeclTppl_DeclTppl : ((a) -> ((DeclTppl) -> ((a, DeclTppl)))) -> ((a) -> ((DeclTppl) -> ((a, DeclTppl))))
  sem smapAccumL_DeclTppl_DeclTppl f acc =
  | x ->
    (acc, x)

  sem smap_DeclTppl_DeclTppl : ((DeclTppl) -> (DeclTppl)) -> ((DeclTppl) -> (DeclTppl))
  sem smap_DeclTppl_DeclTppl f =
  | x ->
    (smapAccumL_DeclTppl_DeclTppl
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_DeclTppl_DeclTppl : ((a) -> ((DeclTppl) -> (a))) -> ((a) -> ((DeclTppl) -> (a)))
  sem sfold_DeclTppl_DeclTppl f acc =
  | x ->
    (smapAccumL_DeclTppl_DeclTppl
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_DeclTppl_TypeTppl : ((a) -> ((TypeTppl) -> ((a, TypeTppl)))) -> ((a) -> ((DeclTppl) -> ((a, DeclTppl))))
  sem smapAccumL_DeclTppl_TypeTppl f acc =
  | x ->
    (acc, x)

  sem smap_DeclTppl_TypeTppl : ((TypeTppl) -> (TypeTppl)) -> ((DeclTppl) -> (DeclTppl))
  sem smap_DeclTppl_TypeTppl f =
  | x ->
    (smapAccumL_DeclTppl_TypeTppl
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_DeclTppl_TypeTppl : ((a) -> ((TypeTppl) -> (a))) -> ((a) -> ((DeclTppl) -> (a)))
  sem sfold_DeclTppl_TypeTppl f acc =
  | x ->
    (smapAccumL_DeclTppl_TypeTppl
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_DeclTppl_StmtTppl : ((a) -> ((StmtTppl) -> ((a, StmtTppl)))) -> ((a) -> ((DeclTppl) -> ((a, DeclTppl))))
  sem smapAccumL_DeclTppl_StmtTppl f acc =
  | x ->
    (acc, x)

  sem smap_DeclTppl_StmtTppl : ((StmtTppl) -> (StmtTppl)) -> ((DeclTppl) -> (DeclTppl))
  sem smap_DeclTppl_StmtTppl f =
  | x ->
    (smapAccumL_DeclTppl_StmtTppl
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_DeclTppl_StmtTppl : ((a) -> ((StmtTppl) -> (a))) -> ((a) -> ((DeclTppl) -> (a)))
  sem sfold_DeclTppl_StmtTppl f acc =
  | x ->
    (smapAccumL_DeclTppl_StmtTppl
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_DeclTppl_ExprTppl : ((a) -> ((ExprTppl) -> ((a, ExprTppl)))) -> ((a) -> ((DeclTppl) -> ((a, DeclTppl))))
  sem smapAccumL_DeclTppl_ExprTppl f acc =
  | x ->
    (acc, x)

  sem smap_DeclTppl_ExprTppl : ((ExprTppl) -> (ExprTppl)) -> ((DeclTppl) -> (DeclTppl))
  sem smap_DeclTppl_ExprTppl f =
  | x ->
    (smapAccumL_DeclTppl_ExprTppl
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_DeclTppl_ExprTppl : ((a) -> ((ExprTppl) -> (a))) -> ((a) -> ((DeclTppl) -> (a)))
  sem sfold_DeclTppl_ExprTppl f acc =
  | x ->
    (smapAccumL_DeclTppl_ExprTppl
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_TypeTppl_FileTppl : ((a) -> ((FileTppl) -> ((a, FileTppl)))) -> ((a) -> ((TypeTppl) -> ((a, TypeTppl))))
  sem smapAccumL_TypeTppl_FileTppl f acc =
  | x ->
    (acc, x)

  sem smap_TypeTppl_FileTppl : ((FileTppl) -> (FileTppl)) -> ((TypeTppl) -> (TypeTppl))
  sem smap_TypeTppl_FileTppl f =
  | x ->
    (smapAccumL_TypeTppl_FileTppl
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_TypeTppl_FileTppl : ((a) -> ((FileTppl) -> (a))) -> ((a) -> ((TypeTppl) -> (a)))
  sem sfold_TypeTppl_FileTppl f acc =
  | x ->
    (smapAccumL_TypeTppl_FileTppl
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_TypeTppl_DeclTppl : ((a) -> ((DeclTppl) -> ((a, DeclTppl)))) -> ((a) -> ((TypeTppl) -> ((a, TypeTppl))))
  sem smapAccumL_TypeTppl_DeclTppl f acc =
  | x ->
    (acc, x)

  sem smap_TypeTppl_DeclTppl : ((DeclTppl) -> (DeclTppl)) -> ((TypeTppl) -> (TypeTppl))
  sem smap_TypeTppl_DeclTppl f =
  | x ->
    (smapAccumL_TypeTppl_DeclTppl
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_TypeTppl_DeclTppl : ((a) -> ((DeclTppl) -> (a))) -> ((a) -> ((TypeTppl) -> (a)))
  sem sfold_TypeTppl_DeclTppl f acc =
  | x ->
    (smapAccumL_TypeTppl_DeclTppl
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_TypeTppl_TypeTppl : ((a) -> ((TypeTppl) -> ((a, TypeTppl)))) -> ((a) -> ((TypeTppl) -> ((a, TypeTppl))))
  sem smapAccumL_TypeTppl_TypeTppl f acc =
  | x ->
    (acc, x)

  sem smap_TypeTppl_TypeTppl : ((TypeTppl) -> (TypeTppl)) -> ((TypeTppl) -> (TypeTppl))
  sem smap_TypeTppl_TypeTppl f =
  | x ->
    (smapAccumL_TypeTppl_TypeTppl
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_TypeTppl_TypeTppl : ((a) -> ((TypeTppl) -> (a))) -> ((a) -> ((TypeTppl) -> (a)))
  sem sfold_TypeTppl_TypeTppl f acc =
  | x ->
    (smapAccumL_TypeTppl_TypeTppl
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_TypeTppl_StmtTppl : ((a) -> ((StmtTppl) -> ((a, StmtTppl)))) -> ((a) -> ((TypeTppl) -> ((a, TypeTppl))))
  sem smapAccumL_TypeTppl_StmtTppl f acc =
  | x ->
    (acc, x)

  sem smap_TypeTppl_StmtTppl : ((StmtTppl) -> (StmtTppl)) -> ((TypeTppl) -> (TypeTppl))
  sem smap_TypeTppl_StmtTppl f =
  | x ->
    (smapAccumL_TypeTppl_StmtTppl
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_TypeTppl_StmtTppl : ((a) -> ((StmtTppl) -> (a))) -> ((a) -> ((TypeTppl) -> (a)))
  sem sfold_TypeTppl_StmtTppl f acc =
  | x ->
    (smapAccumL_TypeTppl_StmtTppl
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_TypeTppl_ExprTppl : ((a) -> ((ExprTppl) -> ((a, ExprTppl)))) -> ((a) -> ((TypeTppl) -> ((a, TypeTppl))))
  sem smapAccumL_TypeTppl_ExprTppl f acc =
  | x ->
    (acc, x)

  sem smap_TypeTppl_ExprTppl : ((ExprTppl) -> (ExprTppl)) -> ((TypeTppl) -> (TypeTppl))
  sem smap_TypeTppl_ExprTppl f =
  | x ->
    (smapAccumL_TypeTppl_ExprTppl
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_TypeTppl_ExprTppl : ((a) -> ((ExprTppl) -> (a))) -> ((a) -> ((TypeTppl) -> (a)))
  sem sfold_TypeTppl_ExprTppl f acc =
  | x ->
    (smapAccumL_TypeTppl_ExprTppl
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_StmtTppl_FileTppl : ((a) -> ((FileTppl) -> ((a, FileTppl)))) -> ((a) -> ((StmtTppl) -> ((a, StmtTppl))))
  sem smapAccumL_StmtTppl_FileTppl f acc =
  | x ->
    (acc, x)

  sem smap_StmtTppl_FileTppl : ((FileTppl) -> (FileTppl)) -> ((StmtTppl) -> (StmtTppl))
  sem smap_StmtTppl_FileTppl f =
  | x ->
    (smapAccumL_StmtTppl_FileTppl
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_StmtTppl_FileTppl : ((a) -> ((FileTppl) -> (a))) -> ((a) -> ((StmtTppl) -> (a)))
  sem sfold_StmtTppl_FileTppl f acc =
  | x ->
    (smapAccumL_StmtTppl_FileTppl
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_StmtTppl_DeclTppl : ((a) -> ((DeclTppl) -> ((a, DeclTppl)))) -> ((a) -> ((StmtTppl) -> ((a, StmtTppl))))
  sem smapAccumL_StmtTppl_DeclTppl f acc =
  | x ->
    (acc, x)

  sem smap_StmtTppl_DeclTppl : ((DeclTppl) -> (DeclTppl)) -> ((StmtTppl) -> (StmtTppl))
  sem smap_StmtTppl_DeclTppl f =
  | x ->
    (smapAccumL_StmtTppl_DeclTppl
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_StmtTppl_DeclTppl : ((a) -> ((DeclTppl) -> (a))) -> ((a) -> ((StmtTppl) -> (a)))
  sem sfold_StmtTppl_DeclTppl f acc =
  | x ->
    (smapAccumL_StmtTppl_DeclTppl
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_StmtTppl_TypeTppl : ((a) -> ((TypeTppl) -> ((a, TypeTppl)))) -> ((a) -> ((StmtTppl) -> ((a, StmtTppl))))
  sem smapAccumL_StmtTppl_TypeTppl f acc =
  | x ->
    (acc, x)

  sem smap_StmtTppl_TypeTppl : ((TypeTppl) -> (TypeTppl)) -> ((StmtTppl) -> (StmtTppl))
  sem smap_StmtTppl_TypeTppl f =
  | x ->
    (smapAccumL_StmtTppl_TypeTppl
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_StmtTppl_TypeTppl : ((a) -> ((TypeTppl) -> (a))) -> ((a) -> ((StmtTppl) -> (a)))
  sem sfold_StmtTppl_TypeTppl f acc =
  | x ->
    (smapAccumL_StmtTppl_TypeTppl
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_StmtTppl_StmtTppl : ((a) -> ((StmtTppl) -> ((a, StmtTppl)))) -> ((a) -> ((StmtTppl) -> ((a, StmtTppl))))
  sem smapAccumL_StmtTppl_StmtTppl f acc =
  | x ->
    (acc, x)

  sem smap_StmtTppl_StmtTppl : ((StmtTppl) -> (StmtTppl)) -> ((StmtTppl) -> (StmtTppl))
  sem smap_StmtTppl_StmtTppl f =
  | x ->
    (smapAccumL_StmtTppl_StmtTppl
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_StmtTppl_StmtTppl : ((a) -> ((StmtTppl) -> (a))) -> ((a) -> ((StmtTppl) -> (a)))
  sem sfold_StmtTppl_StmtTppl f acc =
  | x ->
    (smapAccumL_StmtTppl_StmtTppl
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_StmtTppl_ExprTppl : ((a) -> ((ExprTppl) -> ((a, ExprTppl)))) -> ((a) -> ((StmtTppl) -> ((a, StmtTppl))))
  sem smapAccumL_StmtTppl_ExprTppl f acc =
  | x ->
    (acc, x)

  sem smap_StmtTppl_ExprTppl : ((ExprTppl) -> (ExprTppl)) -> ((StmtTppl) -> (StmtTppl))
  sem smap_StmtTppl_ExprTppl f =
  | x ->
    (smapAccumL_StmtTppl_ExprTppl
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_StmtTppl_ExprTppl : ((a) -> ((ExprTppl) -> (a))) -> ((a) -> ((StmtTppl) -> (a)))
  sem sfold_StmtTppl_ExprTppl f acc =
  | x ->
    (smapAccumL_StmtTppl_ExprTppl
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_ExprTppl_FileTppl : ((a) -> ((FileTppl) -> ((a, FileTppl)))) -> ((a) -> ((ExprTppl) -> ((a, ExprTppl))))
  sem smapAccumL_ExprTppl_FileTppl f acc =
  | x ->
    (acc, x)

  sem smap_ExprTppl_FileTppl : ((FileTppl) -> (FileTppl)) -> ((ExprTppl) -> (ExprTppl))
  sem smap_ExprTppl_FileTppl f =
  | x ->
    (smapAccumL_ExprTppl_FileTppl
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_ExprTppl_FileTppl : ((a) -> ((FileTppl) -> (a))) -> ((a) -> ((ExprTppl) -> (a)))
  sem sfold_ExprTppl_FileTppl f acc =
  | x ->
    (smapAccumL_ExprTppl_FileTppl
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_ExprTppl_DeclTppl : ((a) -> ((DeclTppl) -> ((a, DeclTppl)))) -> ((a) -> ((ExprTppl) -> ((a, ExprTppl))))
  sem smapAccumL_ExprTppl_DeclTppl f acc =
  | x ->
    (acc, x)

  sem smap_ExprTppl_DeclTppl : ((DeclTppl) -> (DeclTppl)) -> ((ExprTppl) -> (ExprTppl))
  sem smap_ExprTppl_DeclTppl f =
  | x ->
    (smapAccumL_ExprTppl_DeclTppl
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_ExprTppl_DeclTppl : ((a) -> ((DeclTppl) -> (a))) -> ((a) -> ((ExprTppl) -> (a)))
  sem sfold_ExprTppl_DeclTppl f acc =
  | x ->
    (smapAccumL_ExprTppl_DeclTppl
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_ExprTppl_TypeTppl : ((a) -> ((TypeTppl) -> ((a, TypeTppl)))) -> ((a) -> ((ExprTppl) -> ((a, ExprTppl))))
  sem smapAccumL_ExprTppl_TypeTppl f acc =
  | x ->
    (acc, x)

  sem smap_ExprTppl_TypeTppl : ((TypeTppl) -> (TypeTppl)) -> ((ExprTppl) -> (ExprTppl))
  sem smap_ExprTppl_TypeTppl f =
  | x ->
    (smapAccumL_ExprTppl_TypeTppl
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_ExprTppl_TypeTppl : ((a) -> ((TypeTppl) -> (a))) -> ((a) -> ((ExprTppl) -> (a)))
  sem sfold_ExprTppl_TypeTppl f acc =
  | x ->
    (smapAccumL_ExprTppl_TypeTppl
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_ExprTppl_StmtTppl : ((a) -> ((StmtTppl) -> ((a, StmtTppl)))) -> ((a) -> ((ExprTppl) -> ((a, ExprTppl))))
  sem smapAccumL_ExprTppl_StmtTppl f acc =
  | x ->
    (acc, x)

  sem smap_ExprTppl_StmtTppl : ((StmtTppl) -> (StmtTppl)) -> ((ExprTppl) -> (ExprTppl))
  sem smap_ExprTppl_StmtTppl f =
  | x ->
    (smapAccumL_ExprTppl_StmtTppl
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_ExprTppl_StmtTppl : ((a) -> ((StmtTppl) -> (a))) -> ((a) -> ((ExprTppl) -> (a)))
  sem sfold_ExprTppl_StmtTppl f acc =
  | x ->
    (smapAccumL_ExprTppl_StmtTppl
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_ExprTppl_ExprTppl : ((a) -> ((ExprTppl) -> ((a, ExprTppl)))) -> ((a) -> ((ExprTppl) -> ((a, ExprTppl))))
  sem smapAccumL_ExprTppl_ExprTppl f acc =
  | x ->
    (acc, x)

  sem smap_ExprTppl_ExprTppl : ((ExprTppl) -> (ExprTppl)) -> ((ExprTppl) -> (ExprTppl))
  sem smap_ExprTppl_ExprTppl f =
  | x ->
    (smapAccumL_ExprTppl_ExprTppl
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_ExprTppl_ExprTppl : ((a) -> ((ExprTppl) -> (a))) -> ((a) -> ((ExprTppl) -> (a)))
  sem sfold_ExprTppl_ExprTppl f acc =
  | x ->
    (smapAccumL_ExprTppl_ExprTppl
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem get_FileTppl_info : (FileTppl) -> (Info)

  sem set_FileTppl_info : (Info) -> ((FileTppl) -> (FileTppl))

  sem mapAccum_FileTppl_info : ((a) -> ((Info) -> ((a, Info)))) -> ((a) -> ((FileTppl) -> ((a, FileTppl))))
  sem mapAccum_FileTppl_info f acc =
  | target ->
    match
      f
        acc
        (get_FileTppl_info
           target)
    with
      (acc, val)
    then
      (acc, set_FileTppl_info
        val
        target)
    else
      never

  sem map_FileTppl_info : ((Info) -> (Info)) -> ((FileTppl) -> (FileTppl))
  sem map_FileTppl_info f =
  | target ->
    set_FileTppl_info
      (f
         (get_FileTppl_info
            target))
      target

  sem get_DeclTppl_info : (DeclTppl) -> (Info)

  sem set_DeclTppl_info : (Info) -> ((DeclTppl) -> (DeclTppl))

  sem mapAccum_DeclTppl_info : ((a) -> ((Info) -> ((a, Info)))) -> ((a) -> ((DeclTppl) -> ((a, DeclTppl))))
  sem mapAccum_DeclTppl_info f acc =
  | target ->
    match
      f
        acc
        (get_DeclTppl_info
           target)
    with
      (acc, val)
    then
      (acc, set_DeclTppl_info
        val
        target)
    else
      never

  sem map_DeclTppl_info : ((Info) -> (Info)) -> ((DeclTppl) -> (DeclTppl))
  sem map_DeclTppl_info f =
  | target ->
    set_DeclTppl_info
      (f
         (get_DeclTppl_info
            target))
      target

  sem get_TypeTppl_info : (TypeTppl) -> (Info)

  sem set_TypeTppl_info : (Info) -> ((TypeTppl) -> (TypeTppl))

  sem mapAccum_TypeTppl_info : ((a) -> ((Info) -> ((a, Info)))) -> ((a) -> ((TypeTppl) -> ((a, TypeTppl))))
  sem mapAccum_TypeTppl_info f acc =
  | target ->
    match
      f
        acc
        (get_TypeTppl_info
           target)
    with
      (acc, val)
    then
      (acc, set_TypeTppl_info
        val
        target)
    else
      never

  sem map_TypeTppl_info : ((Info) -> (Info)) -> ((TypeTppl) -> (TypeTppl))
  sem map_TypeTppl_info f =
  | target ->
    set_TypeTppl_info
      (f
         (get_TypeTppl_info
            target))
      target

  sem get_StmtTppl_info : (StmtTppl) -> (Info)

  sem set_StmtTppl_info : (Info) -> ((StmtTppl) -> (StmtTppl))

  sem mapAccum_StmtTppl_info : ((a) -> ((Info) -> ((a, Info)))) -> ((a) -> ((StmtTppl) -> ((a, StmtTppl))))
  sem mapAccum_StmtTppl_info f acc =
  | target ->
    match
      f
        acc
        (get_StmtTppl_info
           target)
    with
      (acc, val)
    then
      (acc, set_StmtTppl_info
        val
        target)
    else
      never

  sem map_StmtTppl_info : ((Info) -> (Info)) -> ((StmtTppl) -> (StmtTppl))
  sem map_StmtTppl_info f =
  | target ->
    set_StmtTppl_info
      (f
         (get_StmtTppl_info
            target))
      target

  sem get_ExprTppl_info : (ExprTppl) -> (Info)

  sem set_ExprTppl_info : (Info) -> ((ExprTppl) -> (ExprTppl))

  sem mapAccum_ExprTppl_info : ((a) -> ((Info) -> ((a, Info)))) -> ((a) -> ((ExprTppl) -> ((a, ExprTppl))))
  sem mapAccum_ExprTppl_info f acc =
  | target ->
    match
      f
        acc
        (get_ExprTppl_info
           target)
    with
      (acc, val)
    then
      (acc, set_ExprTppl_info
        val
        target)
    else
      never

  sem map_ExprTppl_info : ((Info) -> (Info)) -> ((ExprTppl) -> (ExprTppl))
  sem map_ExprTppl_info f =
  | target ->
    set_ExprTppl_info
      (f
         (get_ExprTppl_info
            target))
      target

end

lang FileTpplAst = TreePPLBaseAst
  type FileTpplRecord = {decl: [DeclTppl], info: Info}

  syn FileTppl =
  | FileTppl FileTpplRecord

  sem smapAccumL_FileTppl_DeclTppl f acc =
  | FileTppl x ->
    match
      match
        let decl =
          x.decl
        in
        mapAccumL
          (lam acc1.
             lam x1: DeclTppl.
               f
                 acc1
                 x1)
          acc
          decl
      with
        (acc, decl)
      then
        (acc, { x
          with
          decl =
            decl })
      else
        never
    with
      (acc, x)
    then
      (acc, FileTppl
        x)
    else
      never

  sem get_FileTppl_info =
  | FileTppl target ->
    target.info

  sem set_FileTppl_info val =
  | FileTppl target ->
    FileTppl
      { target
        with
        info =
          val }

end

lang TypeTpplAst = TreePPLBaseAst
  type TypeTpplRecord = {info: Info, name: {v: Name, i: Info}}

  syn TypeTppl =
  | TypeTppl TypeTpplRecord

  sem get_TypeTppl_info =
  | TypeTppl target ->
    target.info

  sem set_TypeTppl_info val =
  | TypeTppl target ->
    TypeTppl
      { target
        with
        info =
          val }

end

lang SequenceTypeTpplAst = TreePPLBaseAst
  type SequenceTypeTpplRecord = {info: Info, size: (Option) ({v: Int, i: Info}), ty: TypeTppl}

  syn TypeTppl =
  | SequenceTypeTppl SequenceTypeTpplRecord

  sem smapAccumL_TypeTppl_TypeTppl f acc =
  | SequenceTypeTppl x ->
    match
      match
        let ty =
          x.ty
        in
        f
          acc
          ty
      with
        (acc, ty)
      then
        (acc, { x
          with
          ty =
            ty })
      else
        never
    with
      (acc, x)
    then
      (acc, SequenceTypeTppl
        x)
    else
      never

  sem get_TypeTppl_info =
  | SequenceTypeTppl target ->
    target.info

  sem set_TypeTppl_info val =
  | SequenceTypeTppl target ->
    SequenceTypeTppl
      { target
        with
        info =
          val }

end

lang AtomicRealTypeTpplAst = TreePPLBaseAst
  type AtomicRealTypeTpplRecord = {info: Info}

  syn TypeTppl =
  | AtomicRealTypeTppl AtomicRealTypeTpplRecord

  sem get_TypeTppl_info =
  | AtomicRealTypeTppl target ->
    target.info

  sem set_TypeTppl_info val =
  | AtomicRealTypeTppl target ->
    AtomicRealTypeTppl
      { target
        with
        info =
          val }

end

lang FunDeclTpplAst = TreePPLBaseAst
  type FunDeclTpplRecord = {info: Info, name: {v: Name, i: Info}, model: (Option) (Info), args: [{name: {v: Name, i: Info}, ty: TypeTppl}], returnTy: (Option) (TypeTppl), body: [StmtTppl]}

  syn DeclTppl =
  | FunDeclTppl FunDeclTpplRecord

  sem smapAccumL_DeclTppl_TypeTppl f acc =
  | FunDeclTppl x ->
    match
      match
        let args =
          x.args
        in
        mapAccumL
          (lam acc1.
             lam x1: {name: {v: Name, i: Info}, ty: TypeTppl}.
               match
                 let ty =
                   x1.ty
                 in
                 f
                   acc1
                   ty
               with
                 (acc1, ty)
               then
                 (acc1, { x1
                   with
                   ty =
                     ty })
               else
                 never)
          acc
          args
      with
        (acc, args)
      then
        match
          let returnTy =
            x.returnTy
          in
          optionMapAccum
            (lam acc2.
               lam x2.
                 f
                   acc2
                   x2)
            acc
            returnTy
        with
          (acc, returnTy)
        then
          (acc, { { x
              with
              args =
                args }
            with
            returnTy =
              returnTy })
        else
          never
      else
        never
    with
      (acc, x)
    then
      (acc, FunDeclTppl
        x)
    else
      never

  sem smapAccumL_DeclTppl_StmtTppl f acc =
  | FunDeclTppl x ->
    match
      match
        let body =
          x.body
        in
        mapAccumL
          (lam acc1.
             lam x1: StmtTppl.
               f
                 acc1
                 x1)
          acc
          body
      with
        (acc, body)
      then
        (acc, { x
          with
          body =
            body })
      else
        never
    with
      (acc, x)
    then
      (acc, FunDeclTppl
        x)
    else
      never

  sem get_DeclTppl_info =
  | FunDeclTppl target ->
    target.info

  sem set_DeclTppl_info val =
  | FunDeclTppl target ->
    FunDeclTppl
      { target
        with
        info =
          val }

end

lang IntegerExprTpplAst = TreePPLBaseAst
  type IntegerExprTpplRecord = {info: Info, val: {v: Int, i: Info}}

  syn ExprTppl =
  | IntegerExprTppl IntegerExprTpplRecord

  sem get_ExprTppl_info =
  | IntegerExprTppl target ->
    target.info

  sem set_ExprTppl_info val =
  | IntegerExprTppl target ->
    IntegerExprTppl
      { target
        with
        info =
          val }

end

lang StringExprTpplAst = TreePPLBaseAst
  type StringExprTpplRecord = {info: Info, val: {v: String, i: Info}}

  syn ExprTppl =
  | StringExprTppl StringExprTpplRecord

  sem get_ExprTppl_info =
  | StringExprTppl target ->
    target.info

  sem set_ExprTppl_info val =
  | StringExprTppl target ->
    StringExprTppl
      { target
        with
        info =
          val }

end

lang RealExprTpplAst = TreePPLBaseAst
  type RealExprTpplRecord = {info: Info, val: {v: Float, i: Info}}

  syn ExprTppl =
  | RealExprTppl RealExprTpplRecord

  sem get_ExprTppl_info =
  | RealExprTppl target ->
    target.info

  sem set_ExprTppl_info val =
  | RealExprTppl target ->
    RealExprTppl
      { target
        with
        info =
          val }

end

lang VariableExprTpplAst = TreePPLBaseAst
  type VariableExprTpplRecord = {info: Info, ident: {v: Name, i: Info}}

  syn ExprTppl =
  | VariableExprTppl VariableExprTpplRecord

  sem get_ExprTppl_info =
  | VariableExprTppl target ->
    target.info

  sem set_ExprTppl_info val =
  | VariableExprTppl target ->
    VariableExprTppl
      { target
        with
        info =
          val }

end

lang AddExprTpplAst = TreePPLBaseAst
  type AddExprTpplRecord = {info: Info, left: ExprTppl, right: ExprTppl}

  syn ExprTppl =
  | AddExprTppl AddExprTpplRecord

  sem smapAccumL_ExprTppl_ExprTppl f acc =
  | AddExprTppl x ->
    match
      match
        let left =
          x.left
        in
        f
          acc
          left
      with
        (acc, left)
      then
        match
          let right =
            x.right
          in
          f
            acc
            right
        with
          (acc, right)
        then
          (acc, { { x
              with
              left =
                left }
            with
            right =
              right })
        else
          never
      else
        never
    with
      (acc, x)
    then
      (acc, AddExprTppl
        x)
    else
      never

  sem get_ExprTppl_info =
  | AddExprTppl target ->
    target.info

  sem set_ExprTppl_info val =
  | AddExprTppl target ->
    AddExprTppl
      { target
        with
        info =
          val }

end

lang MulExprTpplAst = TreePPLBaseAst
  type MulExprTpplRecord = {info: Info, left: ExprTppl, right: ExprTppl}

  syn ExprTppl =
  | MulExprTppl MulExprTpplRecord

  sem smapAccumL_ExprTppl_ExprTppl f acc =
  | MulExprTppl x ->
    match
      match
        let left =
          x.left
        in
        f
          acc
          left
      with
        (acc, left)
      then
        match
          let right =
            x.right
          in
          f
            acc
            right
        with
          (acc, right)
        then
          (acc, { { x
              with
              left =
                left }
            with
            right =
              right })
        else
          never
      else
        never
    with
      (acc, x)
    then
      (acc, MulExprTppl
        x)
    else
      never

  sem get_ExprTppl_info =
  | MulExprTppl target ->
    target.info

  sem set_ExprTppl_info val =
  | MulExprTppl target ->
    MulExprTppl
      { target
        with
        info =
          val }

end

lang SubExprTpplAst = TreePPLBaseAst
  type SubExprTpplRecord = {info: Info, left: ExprTppl, right: ExprTppl}

  syn ExprTppl =
  | SubExprTppl SubExprTpplRecord

  sem smapAccumL_ExprTppl_ExprTppl f acc =
  | SubExprTppl x ->
    match
      match
        let left =
          x.left
        in
        f
          acc
          left
      with
        (acc, left)
      then
        match
          let right =
            x.right
          in
          f
            acc
            right
        with
          (acc, right)
        then
          (acc, { { x
              with
              left =
                left }
            with
            right =
              right })
        else
          never
      else
        never
    with
      (acc, x)
    then
      (acc, SubExprTppl
        x)
    else
      never

  sem get_ExprTppl_info =
  | SubExprTppl target ->
    target.info

  sem set_ExprTppl_info val =
  | SubExprTppl target ->
    SubExprTppl
      { target
        with
        info =
          val }

end

lang DivExprTpplAst = TreePPLBaseAst
  type DivExprTpplRecord = {info: Info, left: ExprTppl, right: ExprTppl}

  syn ExprTppl =
  | DivExprTppl DivExprTpplRecord

  sem smapAccumL_ExprTppl_ExprTppl f acc =
  | DivExprTppl x ->
    match
      match
        let left =
          x.left
        in
        f
          acc
          left
      with
        (acc, left)
      then
        match
          let right =
            x.right
          in
          f
            acc
            right
        with
          (acc, right)
        then
          (acc, { { x
              with
              left =
                left }
            with
            right =
              right })
        else
          never
      else
        never
    with
      (acc, x)
    then
      (acc, DivExprTppl
        x)
    else
      never

  sem get_ExprTppl_info =
  | DivExprTppl target ->
    target.info

  sem set_ExprTppl_info val =
  | DivExprTppl target ->
    DivExprTppl
      { target
        with
        info =
          val }

end

lang IsExprTpplAst = TreePPLBaseAst
  type IsExprTpplRecord = {info: Info, constructor: {v: Name, i: Info}, thing: ExprTppl}

  syn ExprTppl =
  | IsExprTppl IsExprTpplRecord

  sem smapAccumL_ExprTppl_ExprTppl f acc =
  | IsExprTppl x ->
    match
      match
        let thing =
          x.thing
        in
        f
          acc
          thing
      with
        (acc, thing)
      then
        (acc, { x
          with
          thing =
            thing })
      else
        never
    with
      (acc, x)
    then
      (acc, IsExprTppl
        x)
    else
      never

  sem get_ExprTppl_info =
  | IsExprTppl target ->
    target.info

  sem set_ExprTppl_info val =
  | IsExprTppl target ->
    IsExprTppl
      { target
        with
        info =
          val }

end

lang ToExprTpplAst = TreePPLBaseAst
  type ToExprTpplRecord = {info: Info, endVal: ExprTppl, beginVal: ExprTppl}

  syn ExprTppl =
  | ToExprTppl ToExprTpplRecord

  sem smapAccumL_ExprTppl_ExprTppl f acc =
  | ToExprTppl x ->
    match
      match
        let endVal =
          x.endVal
        in
        f
          acc
          endVal
      with
        (acc, endVal)
      then
        match
          let beginVal =
            x.beginVal
          in
          f
            acc
            beginVal
        with
          (acc, beginVal)
        then
          (acc, { { x
              with
              endVal =
                endVal }
            with
            beginVal =
              beginVal })
        else
          never
      else
        never
    with
      (acc, x)
    then
      (acc, ToExprTppl
        x)
    else
      never

  sem get_ExprTppl_info =
  | ToExprTppl target ->
    target.info

  sem set_ExprTppl_info val =
  | ToExprTppl target ->
    ToExprTppl
      { target
        with
        info =
          val }

end

lang FunCallExprTpplAst = TreePPLBaseAst
  type FunCallExprTpplRecord = {info: Info, args: [ExprTppl], f: ExprTppl}

  syn ExprTppl =
  | FunCallExprTppl FunCallExprTpplRecord

  sem smapAccumL_ExprTppl_ExprTppl f acc =
  | FunCallExprTppl x ->
    match
      match
        let f1 =
          x.f
        in
        f
          acc
          f1
      with
        (acc, f1)
      then
        match
          let args =
            x.args
          in
          mapAccumL
            (lam acc1.
               lam x1: ExprTppl.
                 f
                   acc1
                   x1)
            acc
            args
        with
          (acc, args)
        then
          (acc, { { x
              with
              f =
                f1 }
            with
            args =
              args })
        else
          never
      else
        never
    with
      (acc, x)
    then
      (acc, FunCallExprTppl
        x)
    else
      never

  sem get_ExprTppl_info =
  | FunCallExprTppl target ->
    target.info

  sem set_ExprTppl_info val =
  | FunCallExprTppl target ->
    FunCallExprTppl
      { target
        with
        info =
          val }

end

lang RecordExprTpplAst = TreePPLBaseAst
  type RecordExprTpplRecord = {info: Info, value: [ExprTppl], key: [{v: Name, i: Info}]}

  syn ExprTppl =
  | RecordExprTppl RecordExprTpplRecord

  sem smapAccumL_ExprTppl_ExprTppl f acc =
  | RecordExprTppl x ->
    match
      match
        let value =
          x.value
        in
        mapAccumL
          (lam acc1.
             lam x1: ExprTppl.
               f
                 acc1
                 x1)
          acc
          value
      with
        (acc, value)
      then
        (acc, { x
          with
          value =
            value })
      else
        never
    with
      (acc, x)
    then
      (acc, RecordExprTppl
        x)
    else
      never

  sem get_ExprTppl_info =
  | RecordExprTppl target ->
    target.info

  sem set_ExprTppl_info val =
  | RecordExprTppl target ->
    RecordExprTppl
      { target
        with
        info =
          val }

end

lang AssumeStmtTpplAst = TreePPLBaseAst
  type AssumeStmtTpplRecord = {info: Info, dist: ExprTppl, randomVar: {v: Name, i: Info}}

  syn StmtTppl =
  | AssumeStmtTppl AssumeStmtTpplRecord

  sem smapAccumL_StmtTppl_ExprTppl f acc =
  | AssumeStmtTppl x ->
    match
      match
        let dist =
          x.dist
        in
        f
          acc
          dist
      with
        (acc, dist)
      then
        (acc, { x
          with
          dist =
            dist })
      else
        never
    with
      (acc, x)
    then
      (acc, AssumeStmtTppl
        x)
    else
      never

  sem get_StmtTppl_info =
  | AssumeStmtTppl target ->
    target.info

  sem set_StmtTppl_info val =
  | AssumeStmtTppl target ->
    AssumeStmtTppl
      { target
        with
        info =
          val }

end

lang BernoulliExprTpplAst = TreePPLBaseAst
  type BernoulliExprTpplRecord = {info: Info, prob: ExprTppl}

  syn ExprTppl =
  | BernoulliExprTppl BernoulliExprTpplRecord

  sem smapAccumL_ExprTppl_ExprTppl f acc =
  | BernoulliExprTppl x ->
    match
      match
        let prob =
          x.prob
        in
        f
          acc
          prob
      with
        (acc, prob)
      then
        (acc, { x
          with
          prob =
            prob })
      else
        never
    with
      (acc, x)
    then
      (acc, BernoulliExprTppl
        x)
    else
      never

  sem get_ExprTppl_info =
  | BernoulliExprTppl target ->
    target.info

  sem set_ExprTppl_info val =
  | BernoulliExprTppl target ->
    BernoulliExprTppl
      { target
        with
        info =
          val }

end

lang ObserveStmtTpplAst = TreePPLBaseAst
  type ObserveStmtTpplRecord = {info: Info, args: [ExprTppl], value: ExprTppl, distribution: {v: Name, i: Info}}

  syn StmtTppl =
  | ObserveStmtTppl ObserveStmtTpplRecord

  sem smapAccumL_StmtTppl_ExprTppl f acc =
  | ObserveStmtTppl x ->
    match
      match
        let args =
          x.args
        in
        mapAccumL
          (lam acc1.
             lam x1: ExprTppl.
               f
                 acc1
                 x1)
          acc
          args
      with
        (acc, args)
      then
        match
          let value =
            x.value
          in
          f
            acc
            value
        with
          (acc, value)
        then
          (acc, { { x
              with
              args =
                args }
            with
            value =
              value })
        else
          never
      else
        never
    with
      (acc, x)
    then
      (acc, ObserveStmtTppl
        x)
    else
      never

  sem get_StmtTppl_info =
  | ObserveStmtTppl target ->
    target.info

  sem set_StmtTppl_info val =
  | ObserveStmtTppl target ->
    ObserveStmtTppl
      { target
        with
        info =
          val }

end

lang IfStmtTpplAst = TreePPLBaseAst
  type IfStmtTpplRecord = {info: Info, ifTrueStmts: [StmtTppl], ifFalseStmts: [StmtTppl], condition: ExprTppl}

  syn StmtTppl =
  | IfStmtTppl IfStmtTpplRecord

  sem smapAccumL_StmtTppl_StmtTppl f acc =
  | IfStmtTppl x ->
    match
      match
        let ifTrueStmts =
          x.ifTrueStmts
        in
        mapAccumL
          (lam acc1.
             lam x1: StmtTppl.
               f
                 acc1
                 x1)
          acc
          ifTrueStmts
      with
        (acc, ifTrueStmts)
      then
        match
          let ifFalseStmts =
            x.ifFalseStmts
          in
          mapAccumL
            (lam acc2.
               lam x2: StmtTppl.
                 f
                   acc2
                   x2)
            acc
            ifFalseStmts
        with
          (acc, ifFalseStmts)
        then
          (acc, { { x
              with
              ifTrueStmts =
                ifTrueStmts }
            with
            ifFalseStmts =
              ifFalseStmts })
        else
          never
      else
        never
    with
      (acc, x)
    then
      (acc, IfStmtTppl
        x)
    else
      never

  sem smapAccumL_StmtTppl_ExprTppl f acc =
  | IfStmtTppl x ->
    match
      match
        let condition =
          x.condition
        in
        f
          acc
          condition
      with
        (acc, condition)
      then
        (acc, { x
          with
          condition =
            condition })
      else
        never
    with
      (acc, x)
    then
      (acc, IfStmtTppl
        x)
    else
      never

  sem get_StmtTppl_info =
  | IfStmtTppl target ->
    target.info

  sem set_StmtTppl_info val =
  | IfStmtTppl target ->
    IfStmtTppl
      { target
        with
        info =
          val }

end

lang ForLoopStmtTpplAst = TreePPLBaseAst
  type ForLoopStmtTpplRecord = {info: Info, forStmts: [StmtTppl], range: ExprTppl, iterator: {v: Name, i: Info}}

  syn StmtTppl =
  | ForLoopStmtTppl ForLoopStmtTpplRecord

  sem smapAccumL_StmtTppl_StmtTppl f acc =
  | ForLoopStmtTppl x ->
    match
      match
        let forStmts =
          x.forStmts
        in
        mapAccumL
          (lam acc1.
             lam x1: StmtTppl.
               f
                 acc1
                 x1)
          acc
          forStmts
      with
        (acc, forStmts)
      then
        (acc, { x
          with
          forStmts =
            forStmts })
      else
        never
    with
      (acc, x)
    then
      (acc, ForLoopStmtTppl
        x)
    else
      never

  sem smapAccumL_StmtTppl_ExprTppl f acc =
  | ForLoopStmtTppl x ->
    match
      match
        let range =
          x.range
        in
        f
          acc
          range
      with
        (acc, range)
      then
        (acc, { x
          with
          range =
            range })
      else
        never
    with
      (acc, x)
    then
      (acc, ForLoopStmtTppl
        x)
    else
      never

  sem get_StmtTppl_info =
  | ForLoopStmtTppl target ->
    target.info

  sem set_StmtTppl_info val =
  | ForLoopStmtTppl target ->
    ForLoopStmtTppl
      { target
        with
        info =
          val }

end

lang ReturnStmtTpplAst = TreePPLBaseAst
  type ReturnStmtTpplRecord = {info: Info, return: ExprTppl}

  syn StmtTppl =
  | ReturnStmtTppl ReturnStmtTpplRecord

  sem smapAccumL_StmtTppl_ExprTppl f acc =
  | ReturnStmtTppl x ->
    match
      match
        let return =
          x.return
        in
        f
          acc
          return
      with
        (acc, return)
      then
        (acc, { x
          with
          return =
            return })
      else
        never
    with
      (acc, x)
    then
      (acc, ReturnStmtTppl
        x)
    else
      never

  sem get_StmtTppl_info =
  | ReturnStmtTppl target ->
    target.info

  sem set_StmtTppl_info val =
  | ReturnStmtTppl target ->
    ReturnStmtTppl
      { target
        with
        info =
          val }

end

lang BadFileTpplAst = TreePPLBaseAst
  type BadFileTpplRecord = {info: Info}

  syn FileTppl =
  | BadFileTppl BadFileTpplRecord

  sem get_FileTppl_info =
  | BadFileTppl target ->
    target.info

  sem set_FileTppl_info val =
  | BadFileTppl target ->
    BadFileTppl
      { target
        with
        info =
          val }

end

lang BadDeclTpplAst = TreePPLBaseAst
  type BadDeclTpplRecord = {info: Info}

  syn DeclTppl =
  | BadDeclTppl BadDeclTpplRecord

  sem get_DeclTppl_info =
  | BadDeclTppl target ->
    target.info

  sem set_DeclTppl_info val =
  | BadDeclTppl target ->
    BadDeclTppl
      { target
        with
        info =
          val }

end

lang BadTypeTpplAst = TreePPLBaseAst
  type BadTypeTpplRecord = {info: Info}

  syn TypeTppl =
  | BadTypeTppl BadTypeTpplRecord

  sem get_TypeTppl_info =
  | BadTypeTppl target ->
    target.info

  sem set_TypeTppl_info val =
  | BadTypeTppl target ->
    BadTypeTppl
      { target
        with
        info =
          val }

end

lang BadStmtTpplAst = TreePPLBaseAst
  type BadStmtTpplRecord = {info: Info}

  syn StmtTppl =
  | BadStmtTppl BadStmtTpplRecord

  sem get_StmtTppl_info =
  | BadStmtTppl target ->
    target.info

  sem set_StmtTppl_info val =
  | BadStmtTppl target ->
    BadStmtTppl
      { target
        with
        info =
          val }

end

lang BadExprTpplAst = TreePPLBaseAst
  type BadExprTpplRecord = {info: Info}

  syn ExprTppl =
  | BadExprTppl BadExprTpplRecord

  sem get_ExprTppl_info =
  | BadExprTppl target ->
    target.info

  sem set_ExprTppl_info val =
  | BadExprTppl target ->
    BadExprTppl
      { target
        with
        info =
          val }

end

lang TreePPLAst = FileTpplAst + TypeTpplAst + SequenceTypeTpplAst + AtomicRealTypeTpplAst + FunDeclTpplAst + IntegerExprTpplAst + StringExprTpplAst + RealExprTpplAst + VariableExprTpplAst + AddExprTpplAst + MulExprTpplAst + SubExprTpplAst + DivExprTpplAst + IsExprTpplAst + ToExprTpplAst + FunCallExprTpplAst + RecordExprTpplAst + AssumeStmtTpplAst + BernoulliExprTpplAst + ObserveStmtTpplAst + IfStmtTpplAst + ForLoopStmtTpplAst + ReturnStmtTpplAst + BadFileTpplAst + BadDeclTpplAst + BadTypeTpplAst + BadStmtTpplAst + BadExprTpplAst



end

lang FileTpplOpBase = TreePPLBaseAst

  syn FileTpplOp =

  sem topAllowed_FileTpplOp : (FileTpplOp) -> (Bool)
  sem topAllowed_FileTpplOp =
  | _ ->
    true

  sem leftAllowed_FileTpplOp : ({parent: FileTpplOp, child: FileTpplOp}) -> (Bool)
  sem leftAllowed_FileTpplOp =
  | _ ->
    true

  sem rightAllowed_FileTpplOp : ({parent: FileTpplOp, child: FileTpplOp}) -> (Bool)
  sem rightAllowed_FileTpplOp =
  | _ ->
    true

  sem groupingsAllowed_FileTpplOp : (FileTpplOp) -> (AllowedDirection)
  sem groupingsAllowed_FileTpplOp =
  | _ ->
    GEither
      {}

  sem parenAllowed_FileTpplOp : (FileTpplOp) -> (AllowedDirection)
  sem parenAllowed_FileTpplOp =
  | _ ->
    GEither
      {}

  sem get_FileTpplOp_info : (FileTpplOp) -> (Info)

  sem get_FileTpplOp_terms : (FileTpplOp) -> ([Info])

  sem unsplit_FileTpplOp : ((PermanentNode) (FileTpplOp)) -> ((Info, FileTppl))

end

lang DeclTpplOpBase = TreePPLBaseAst

  syn DeclTpplOp =

  sem topAllowed_DeclTpplOp : (DeclTpplOp) -> (Bool)
  sem topAllowed_DeclTpplOp =
  | _ ->
    true

  sem leftAllowed_DeclTpplOp : ({parent: DeclTpplOp, child: DeclTpplOp}) -> (Bool)
  sem leftAllowed_DeclTpplOp =
  | _ ->
    true

  sem rightAllowed_DeclTpplOp : ({parent: DeclTpplOp, child: DeclTpplOp}) -> (Bool)
  sem rightAllowed_DeclTpplOp =
  | _ ->
    true

  sem groupingsAllowed_DeclTpplOp : (DeclTpplOp) -> (AllowedDirection)
  sem groupingsAllowed_DeclTpplOp =
  | _ ->
    GEither
      {}

  sem parenAllowed_DeclTpplOp : (DeclTpplOp) -> (AllowedDirection)
  sem parenAllowed_DeclTpplOp =
  | _ ->
    GEither
      {}

  sem get_DeclTpplOp_info : (DeclTpplOp) -> (Info)

  sem get_DeclTpplOp_terms : (DeclTpplOp) -> ([Info])

  sem unsplit_DeclTpplOp : ((PermanentNode) (DeclTpplOp)) -> ((Info, DeclTppl))

end

lang TypeTpplOpBase = TreePPLBaseAst

  syn TypeTpplOp =

  sem topAllowed_TypeTpplOp : (TypeTpplOp) -> (Bool)
  sem topAllowed_TypeTpplOp =
  | _ ->
    true

  sem leftAllowed_TypeTpplOp : ({parent: TypeTpplOp, child: TypeTpplOp}) -> (Bool)
  sem leftAllowed_TypeTpplOp =
  | _ ->
    true

  sem rightAllowed_TypeTpplOp : ({parent: TypeTpplOp, child: TypeTpplOp}) -> (Bool)
  sem rightAllowed_TypeTpplOp =
  | _ ->
    true

  sem groupingsAllowed_TypeTpplOp : (TypeTpplOp) -> (AllowedDirection)
  sem groupingsAllowed_TypeTpplOp =
  | _ ->
    GEither
      {}

  sem parenAllowed_TypeTpplOp : (TypeTpplOp) -> (AllowedDirection)
  sem parenAllowed_TypeTpplOp =
  | _ ->
    GEither
      {}

  sem get_TypeTpplOp_info : (TypeTpplOp) -> (Info)

  sem get_TypeTpplOp_terms : (TypeTpplOp) -> ([Info])

  sem unsplit_TypeTpplOp : ((PermanentNode) (TypeTpplOp)) -> ((Info, TypeTppl))

end

lang StmtTpplOpBase = TreePPLBaseAst

  syn StmtTpplOp =

  sem topAllowed_StmtTpplOp : (StmtTpplOp) -> (Bool)
  sem topAllowed_StmtTpplOp =
  | _ ->
    true

  sem leftAllowed_StmtTpplOp : ({parent: StmtTpplOp, child: StmtTpplOp}) -> (Bool)
  sem leftAllowed_StmtTpplOp =
  | _ ->
    true

  sem rightAllowed_StmtTpplOp : ({parent: StmtTpplOp, child: StmtTpplOp}) -> (Bool)
  sem rightAllowed_StmtTpplOp =
  | _ ->
    true

  sem groupingsAllowed_StmtTpplOp : (StmtTpplOp) -> (AllowedDirection)
  sem groupingsAllowed_StmtTpplOp =
  | _ ->
    GEither
      {}

  sem parenAllowed_StmtTpplOp : (StmtTpplOp) -> (AllowedDirection)
  sem parenAllowed_StmtTpplOp =
  | _ ->
    GEither
      {}

  sem get_StmtTpplOp_info : (StmtTpplOp) -> (Info)

  sem get_StmtTpplOp_terms : (StmtTpplOp) -> ([Info])

  sem unsplit_StmtTpplOp : ((PermanentNode) (StmtTpplOp)) -> ((Info, StmtTppl))

end

lang ExprTpplOpBase = TreePPLBaseAst

  syn ExprTpplOp =

  sem topAllowed_ExprTpplOp : (ExprTpplOp) -> (Bool)
  sem topAllowed_ExprTpplOp =
  | _ ->
    true

  sem leftAllowed_ExprTpplOp : ({parent: ExprTpplOp, child: ExprTpplOp}) -> (Bool)
  sem leftAllowed_ExprTpplOp =
  | _ ->
    true

  sem rightAllowed_ExprTpplOp : ({parent: ExprTpplOp, child: ExprTpplOp}) -> (Bool)
  sem rightAllowed_ExprTpplOp =
  | _ ->
    true

  sem groupingsAllowed_ExprTpplOp : (ExprTpplOp) -> (AllowedDirection)
  sem groupingsAllowed_ExprTpplOp =
  | _ ->
    GEither
      {}

  sem parenAllowed_ExprTpplOp : (ExprTpplOp) -> (AllowedDirection)
  sem parenAllowed_ExprTpplOp =
  | _ ->
    GEither
      {}

  sem get_ExprTpplOp_info : (ExprTpplOp) -> (Info)

  sem get_ExprTpplOp_terms : (ExprTpplOp) -> ([Info])

  sem unsplit_ExprTpplOp : ((PermanentNode) (ExprTpplOp)) -> ((Info, ExprTppl))

end

lang FileTpplOp = FileTpplOpBase + FileTpplAst

  syn FileTpplOp =
  | FileTpplOp {__br_terms: [Info], __br_info: Info, decl: [DeclTppl]}

  sem get_FileTpplOp_info =
  | FileTpplOp x ->
    x.__br_info

  sem get_FileTpplOp_terms =
  | FileTpplOp x ->
    x.__br_terms

  sem unsplit_FileTpplOp =
  | AtomP {self = FileTpplOp x} ->
    (x.__br_info, FileTppl
      { decl =
          x.decl,
        info =
          x.__br_info })

end

lang TypeTpplOp = TypeTpplOpBase + TypeTpplAst

  syn TypeTpplOp =
  | TypeTpplOp {__br_terms: [Info], __br_info: Info, name: [{v: Name, i: Info}]}

  sem get_TypeTpplOp_info =
  | TypeTpplOp x ->
    x.__br_info

  sem get_TypeTpplOp_terms =
  | TypeTpplOp x ->
    x.__br_terms

  sem unsplit_TypeTpplOp =
  | AtomP {self = TypeTpplOp x} ->
    (x.__br_info, TypeTppl
      { info =
          x.__br_info,
        name =
          match
            x.name
          with
            [ x1 ] ++ _ ++ ""
          then
            x1
          else
            never })

end

lang SequenceTypeTpplOp = TypeTpplOpBase + SequenceTypeTpplAst

  syn TypeTpplOp =
  | SequenceTypeTpplOp {__br_terms: [Info], __br_info: Info, size: [{v: Int, i: Info}]}

  sem get_TypeTpplOp_info =
  | SequenceTypeTpplOp x ->
    x.__br_info

  sem get_TypeTpplOp_terms =
  | SequenceTypeTpplOp x ->
    x.__br_terms

  sem unsplit_TypeTpplOp =
  | PostfixP {self = SequenceTypeTpplOp x, leftChildAlts = [ l ] ++ _ ++ ""} ->
    match
      unsplit_TypeTpplOp
        l
    with
      (linfo, l)
    then
      let info =
        mergeInfo
          linfo
          (x.__br_info)
      in
      (info, SequenceTypeTppl
        { info =
            info,
          size =
            match
              x.size
            with
              [ x1 ] ++ _ ++ ""
            then
              Some
                x1
            else
              None
                {},
          ty =
            match
              [ l ]
            with
              [ x2 ] ++ _ ++ ""
            then
              x2
            else
              never })
    else
      never

end

lang AtomicRealTypeTpplOp = TypeTpplOpBase + AtomicRealTypeTpplAst

  syn TypeTpplOp =
  | AtomicRealTypeTpplOp {__br_terms: [Info], __br_info: Info}

  sem get_TypeTpplOp_info =
  | AtomicRealTypeTpplOp x ->
    x.__br_info

  sem get_TypeTpplOp_terms =
  | AtomicRealTypeTpplOp x ->
    x.__br_terms

  sem unsplit_TypeTpplOp =
  | AtomP {self = AtomicRealTypeTpplOp x} ->
    (x.__br_info, AtomicRealTypeTppl
      { info =
          x.__br_info })

end

lang FunDeclTpplOp = DeclTpplOpBase + FunDeclTpplAst

  syn DeclTpplOp =
  | FunDeclTpplOp {__br_terms: [Info], __br_info: Info, name: [{v: Name, i: Info}], model: [Info], args: [{name: {v: Name, i: Info}, ty: TypeTppl}], returnTy: [TypeTppl], body: [StmtTppl]}

  sem get_DeclTpplOp_info =
  | FunDeclTpplOp x ->
    x.__br_info

  sem get_DeclTpplOp_terms =
  | FunDeclTpplOp x ->
    x.__br_terms

  sem unsplit_DeclTpplOp =
  | AtomP {self = FunDeclTpplOp x} ->
    (x.__br_info, FunDeclTppl
      { info =
          x.__br_info,
        name =
          match
            x.name
          with
            [ x1 ] ++ _ ++ ""
          then
            x1
          else
            never,
        model =
          match
            x.model
          with
            [ x2 ] ++ _ ++ ""
          then
            Some
              x2
          else
            None
              {},
        args =
          x.args,
        returnTy =
          match
            x.returnTy
          with
            [ x3 ] ++ _ ++ ""
          then
            Some
              x3
          else
            None
              {},
        body =
          x.body })

end

lang IntegerExprTpplOp = ExprTpplOpBase + IntegerExprTpplAst

  syn ExprTpplOp =
  | IntegerExprTpplOp {__br_terms: [Info], __br_info: Info, val: [{v: Int, i: Info}]}

  sem get_ExprTpplOp_info =
  | IntegerExprTpplOp x ->
    x.__br_info

  sem get_ExprTpplOp_terms =
  | IntegerExprTpplOp x ->
    x.__br_terms

  sem unsplit_ExprTpplOp =
  | AtomP {self = IntegerExprTpplOp x} ->
    (x.__br_info, IntegerExprTppl
      { info =
          x.__br_info,
        val =
          match
            x.val
          with
            [ x1 ] ++ _ ++ ""
          then
            x1
          else
            never })

end

lang StringExprTpplOp = ExprTpplOpBase + StringExprTpplAst

  syn ExprTpplOp =
  | StringExprTpplOp {__br_terms: [Info], __br_info: Info, val: [{v: String, i: Info}]}

  sem get_ExprTpplOp_info =
  | StringExprTpplOp x ->
    x.__br_info

  sem get_ExprTpplOp_terms =
  | StringExprTpplOp x ->
    x.__br_terms

  sem unsplit_ExprTpplOp =
  | AtomP {self = StringExprTpplOp x} ->
    (x.__br_info, StringExprTppl
      { info =
          x.__br_info,
        val =
          match
            x.val
          with
            [ x1 ] ++ _ ++ ""
          then
            x1
          else
            never })

end

lang RealExprTpplOp = ExprTpplOpBase + RealExprTpplAst

  syn ExprTpplOp =
  | RealExprTpplOp {__br_terms: [Info], __br_info: Info, val: [{v: Float, i: Info}]}

  sem get_ExprTpplOp_info =
  | RealExprTpplOp x ->
    x.__br_info

  sem get_ExprTpplOp_terms =
  | RealExprTpplOp x ->
    x.__br_terms

  sem unsplit_ExprTpplOp =
  | AtomP {self = RealExprTpplOp x} ->
    (x.__br_info, RealExprTppl
      { info =
          x.__br_info,
        val =
          match
            x.val
          with
            [ x1 ] ++ _ ++ ""
          then
            x1
          else
            never })

end

lang VariableExprTpplOp = ExprTpplOpBase + VariableExprTpplAst

  syn ExprTpplOp =
  | VariableExprTpplOp {__br_terms: [Info], __br_info: Info, ident: [{v: Name, i: Info}]}

  sem get_ExprTpplOp_info =
  | VariableExprTpplOp x ->
    x.__br_info

  sem get_ExprTpplOp_terms =
  | VariableExprTpplOp x ->
    x.__br_terms

  sem unsplit_ExprTpplOp =
  | AtomP {self = VariableExprTpplOp x} ->
    (x.__br_info, VariableExprTppl
      { info =
          x.__br_info,
        ident =
          match
            x.ident
          with
            [ x1 ] ++ _ ++ ""
          then
            x1
          else
            never })

end

lang AddExprTpplOp = ExprTpplOpBase + AddExprTpplAst

  syn ExprTpplOp =
  | AddExprTpplOp {__br_terms: [Info], __br_info: Info}

  sem get_ExprTpplOp_info =
  | AddExprTpplOp x ->
    x.__br_info

  sem get_ExprTpplOp_terms =
  | AddExprTpplOp x ->
    x.__br_terms

  sem unsplit_ExprTpplOp =
  | InfixP {self = AddExprTpplOp x, leftChildAlts = [ l ] ++ _ ++ "", rightChildAlts = [ r ] ++ _ ++ ""} ->
    match
      (unsplit_ExprTpplOp
        l, unsplit_ExprTpplOp
        r)
    with
      ((linfo, l), (rinfo, r))
    then
      let info =
        foldl
          mergeInfo
          linfo
          [ x.__br_info,
            rinfo ]
      in
      (info, AddExprTppl
        { info =
            info,
          left =
            match
              [ l ]
            with
              [ x1 ] ++ _ ++ ""
            then
              x1
            else
              never,
          right =
            match
              [ r ]
            with
              [ x2 ] ++ _ ++ ""
            then
              x2
            else
              never })
    else
      never

  sem groupingsAllowed_ExprTpplOp =
  | (AddExprTpplOp _, AddExprTpplOp _) ->
    GLeft
      {}

end

lang MulExprTpplOp = ExprTpplOpBase + MulExprTpplAst

  syn ExprTpplOp =
  | MulExprTpplOp {__br_terms: [Info], __br_info: Info}

  sem get_ExprTpplOp_info =
  | MulExprTpplOp x ->
    x.__br_info

  sem get_ExprTpplOp_terms =
  | MulExprTpplOp x ->
    x.__br_terms

  sem unsplit_ExprTpplOp =
  | InfixP {self = MulExprTpplOp x, leftChildAlts = [ l ] ++ _ ++ "", rightChildAlts = [ r ] ++ _ ++ ""} ->
    match
      (unsplit_ExprTpplOp
        l, unsplit_ExprTpplOp
        r)
    with
      ((linfo, l), (rinfo, r))
    then
      let info =
        foldl
          mergeInfo
          linfo
          [ x.__br_info,
            rinfo ]
      in
      (info, MulExprTppl
        { info =
            info,
          left =
            match
              [ l ]
            with
              [ x1 ] ++ _ ++ ""
            then
              x1
            else
              never,
          right =
            match
              [ r ]
            with
              [ x2 ] ++ _ ++ ""
            then
              x2
            else
              never })
    else
      never

  sem groupingsAllowed_ExprTpplOp =
  | (MulExprTpplOp _, MulExprTpplOp _) ->
    GLeft
      {}

end

lang SubExprTpplOp = ExprTpplOpBase + SubExprTpplAst

  syn ExprTpplOp =
  | SubExprTpplOp {__br_terms: [Info], __br_info: Info}

  sem get_ExprTpplOp_info =
  | SubExprTpplOp x ->
    x.__br_info

  sem get_ExprTpplOp_terms =
  | SubExprTpplOp x ->
    x.__br_terms

  sem unsplit_ExprTpplOp =
  | InfixP {self = SubExprTpplOp x, leftChildAlts = [ l ] ++ _ ++ "", rightChildAlts = [ r ] ++ _ ++ ""} ->
    match
      (unsplit_ExprTpplOp
        l, unsplit_ExprTpplOp
        r)
    with
      ((linfo, l), (rinfo, r))
    then
      let info =
        foldl
          mergeInfo
          linfo
          [ x.__br_info,
            rinfo ]
      in
      (info, SubExprTppl
        { info =
            info,
          left =
            match
              [ l ]
            with
              [ x1 ] ++ _ ++ ""
            then
              x1
            else
              never,
          right =
            match
              [ r ]
            with
              [ x2 ] ++ _ ++ ""
            then
              x2
            else
              never })
    else
      never

  sem groupingsAllowed_ExprTpplOp =
  | (SubExprTpplOp _, SubExprTpplOp _) ->
    GLeft
      {}

end

lang DivExprTpplOp = ExprTpplOpBase + DivExprTpplAst

  syn ExprTpplOp =
  | DivExprTpplOp {__br_terms: [Info], __br_info: Info}

  sem get_ExprTpplOp_info =
  | DivExprTpplOp x ->
    x.__br_info

  sem get_ExprTpplOp_terms =
  | DivExprTpplOp x ->
    x.__br_terms

  sem unsplit_ExprTpplOp =
  | InfixP {self = DivExprTpplOp x, leftChildAlts = [ l ] ++ _ ++ "", rightChildAlts = [ r ] ++ _ ++ ""} ->
    match
      (unsplit_ExprTpplOp
        l, unsplit_ExprTpplOp
        r)
    with
      ((linfo, l), (rinfo, r))
    then
      let info =
        foldl
          mergeInfo
          linfo
          [ x.__br_info,
            rinfo ]
      in
      (info, DivExprTppl
        { info =
            info,
          left =
            match
              [ l ]
            with
              [ x1 ] ++ _ ++ ""
            then
              x1
            else
              never,
          right =
            match
              [ r ]
            with
              [ x2 ] ++ _ ++ ""
            then
              x2
            else
              never })
    else
      never

  sem groupingsAllowed_ExprTpplOp =
  | (DivExprTpplOp _, DivExprTpplOp _) ->
    GLeft
      {}

end

lang IsExprTpplOp = ExprTpplOpBase + IsExprTpplAst

  syn ExprTpplOp =
  | IsExprTpplOp {__br_terms: [Info], __br_info: Info, constructor: [{v: Name, i: Info}]}

  sem get_ExprTpplOp_info =
  | IsExprTpplOp x ->
    x.__br_info

  sem get_ExprTpplOp_terms =
  | IsExprTpplOp x ->
    x.__br_terms

  sem unsplit_ExprTpplOp =
  | PostfixP {self = IsExprTpplOp x, leftChildAlts = [ l ] ++ _ ++ ""} ->
    match
      unsplit_ExprTpplOp
        l
    with
      (linfo, l)
    then
      let info =
        mergeInfo
          linfo
          (x.__br_info)
      in
      (info, IsExprTppl
        { info =
            info,
          constructor =
            match
              x.constructor
            with
              [ x1 ] ++ _ ++ ""
            then
              x1
            else
              never,
          thing =
            match
              [ l ]
            with
              [ x2 ] ++ _ ++ ""
            then
              x2
            else
              never })
    else
      never

end

lang ToExprTpplOp = ExprTpplOpBase + ToExprTpplAst

  syn ExprTpplOp =
  | ToExprTpplOp {__br_terms: [Info], __br_info: Info}

  sem get_ExprTpplOp_info =
  | ToExprTpplOp x ->
    x.__br_info

  sem get_ExprTpplOp_terms =
  | ToExprTpplOp x ->
    x.__br_terms

  sem unsplit_ExprTpplOp =
  | InfixP {self = ToExprTpplOp x, leftChildAlts = [ l ] ++ _ ++ "", rightChildAlts = [ r ] ++ _ ++ ""} ->
    match
      (unsplit_ExprTpplOp
        l, unsplit_ExprTpplOp
        r)
    with
      ((linfo, l), (rinfo, r))
    then
      let info =
        foldl
          mergeInfo
          linfo
          [ x.__br_info,
            rinfo ]
      in
      (info, ToExprTppl
        { info =
            info,
          endVal =
            match
              [ r ]
            with
              [ x1 ] ++ _ ++ ""
            then
              x1
            else
              never,
          beginVal =
            match
              [ l ]
            with
              [ x2 ] ++ _ ++ ""
            then
              x2
            else
              never })
    else
      never

end

lang FunCallExprTpplOp = ExprTpplOpBase + FunCallExprTpplAst

  syn ExprTpplOp =
  | FunCallExprTpplOp {__br_terms: [Info], __br_info: Info, args: [ExprTppl]}

  sem get_ExprTpplOp_info =
  | FunCallExprTpplOp x ->
    x.__br_info

  sem get_ExprTpplOp_terms =
  | FunCallExprTpplOp x ->
    x.__br_terms

  sem unsplit_ExprTpplOp =
  | PostfixP {self = FunCallExprTpplOp x, leftChildAlts = [ l ] ++ _ ++ ""} ->
    match
      unsplit_ExprTpplOp
        l
    with
      (linfo, l)
    then
      let info =
        mergeInfo
          linfo
          (x.__br_info)
      in
      (info, FunCallExprTppl
        { info =
            info,
          args =
            x.args,
          f =
            match
              [ l ]
            with
              [ x1 ] ++ _ ++ ""
            then
              x1
            else
              never })
    else
      never

end

lang RecordExprTpplOp = ExprTpplOpBase + RecordExprTpplAst

  syn ExprTpplOp =
  | RecordExprTpplOp {__br_terms: [Info], __br_info: Info, value: [ExprTppl], key: [{v: Name, i: Info}]}

  sem get_ExprTpplOp_info =
  | RecordExprTpplOp x ->
    x.__br_info

  sem get_ExprTpplOp_terms =
  | RecordExprTpplOp x ->
    x.__br_terms

  sem unsplit_ExprTpplOp =
  | AtomP {self = RecordExprTpplOp x} ->
    (x.__br_info, RecordExprTppl
      { info =
          x.__br_info,
        value =
          x.value,
        key =
          x.key })

end

lang AssumeStmtTpplOp = StmtTpplOpBase + AssumeStmtTpplAst

  syn StmtTpplOp =
  | AssumeStmtTpplOp {__br_terms: [Info], __br_info: Info, dist: [ExprTppl], randomVar: [{v: Name, i: Info}]}

  sem get_StmtTpplOp_info =
  | AssumeStmtTpplOp x ->
    x.__br_info

  sem get_StmtTpplOp_terms =
  | AssumeStmtTpplOp x ->
    x.__br_terms

  sem unsplit_StmtTpplOp =
  | AtomP {self = AssumeStmtTpplOp x} ->
    (x.__br_info, AssumeStmtTppl
      { info =
          x.__br_info,
        dist =
          match
            x.dist
          with
            [ x1 ] ++ _ ++ ""
          then
            x1
          else
            never,
        randomVar =
          match
            x.randomVar
          with
            [ x2 ] ++ _ ++ ""
          then
            x2
          else
            never })

end

lang BernoulliExprTpplOp = ExprTpplOpBase + BernoulliExprTpplAst

  syn ExprTpplOp =
  | BernoulliExprTpplOp {__br_terms: [Info], __br_info: Info, prob: [ExprTppl]}

  sem get_ExprTpplOp_info =
  | BernoulliExprTpplOp x ->
    x.__br_info

  sem get_ExprTpplOp_terms =
  | BernoulliExprTpplOp x ->
    x.__br_terms

  sem unsplit_ExprTpplOp =
  | AtomP {self = BernoulliExprTpplOp x} ->
    (x.__br_info, BernoulliExprTppl
      { info =
          x.__br_info,
        prob =
          match
            x.prob
          with
            [ x1 ] ++ _ ++ ""
          then
            x1
          else
            never })

end

lang ObserveStmtTpplOp = StmtTpplOpBase + ObserveStmtTpplAst

  syn StmtTpplOp =
  | ObserveStmtTpplOp {__br_terms: [Info], __br_info: Info, args: [ExprTppl], value: [ExprTppl], distribution: [{v: Name, i: Info}]}

  sem get_StmtTpplOp_info =
  | ObserveStmtTpplOp x ->
    x.__br_info

  sem get_StmtTpplOp_terms =
  | ObserveStmtTpplOp x ->
    x.__br_terms

  sem unsplit_StmtTpplOp =
  | AtomP {self = ObserveStmtTpplOp x} ->
    (x.__br_info, ObserveStmtTppl
      { info =
          x.__br_info,
        args =
          x.args,
        value =
          match
            x.value
          with
            [ x1 ] ++ _ ++ ""
          then
            x1
          else
            never,
        distribution =
          match
            x.distribution
          with
            [ x2 ] ++ _ ++ ""
          then
            x2
          else
            never })

end

lang IfStmtTpplOp = StmtTpplOpBase + IfStmtTpplAst

  syn StmtTpplOp =
  | IfStmtTpplOp {__br_terms: [Info], __br_info: Info, ifTrueStmts: [StmtTppl], ifFalseStmts: [StmtTppl], condition: [ExprTppl]}

  sem get_StmtTpplOp_info =
  | IfStmtTpplOp x ->
    x.__br_info

  sem get_StmtTpplOp_terms =
  | IfStmtTpplOp x ->
    x.__br_terms

  sem unsplit_StmtTpplOp =
  | AtomP {self = IfStmtTpplOp x} ->
    (x.__br_info, IfStmtTppl
      { info =
          x.__br_info,
        ifTrueStmts =
          x.ifTrueStmts,
        ifFalseStmts =
          x.ifFalseStmts,
        condition =
          match
            x.condition
          with
            [ x1 ] ++ _ ++ ""
          then
            x1
          else
            never })

end

lang ForLoopStmtTpplOp = StmtTpplOpBase + ForLoopStmtTpplAst

  syn StmtTpplOp =
  | ForLoopStmtTpplOp {__br_terms: [Info], __br_info: Info, forStmts: [StmtTppl], range: [ExprTppl], iterator: [{v: Name, i: Info}]}

  sem get_StmtTpplOp_info =
  | ForLoopStmtTpplOp x ->
    x.__br_info

  sem get_StmtTpplOp_terms =
  | ForLoopStmtTpplOp x ->
    x.__br_terms

  sem unsplit_StmtTpplOp =
  | AtomP {self = ForLoopStmtTpplOp x} ->
    (x.__br_info, ForLoopStmtTppl
      { info =
          x.__br_info,
        forStmts =
          x.forStmts,
        range =
          match
            x.range
          with
            [ x1 ] ++ _ ++ ""
          then
            x1
          else
            never,
        iterator =
          match
            x.iterator
          with
            [ x2 ] ++ _ ++ ""
          then
            x2
          else
            never })

end

lang ReturnStmtTpplOp = StmtTpplOpBase + ReturnStmtTpplAst

  syn StmtTpplOp =
  | ReturnStmtTpplOp {__br_terms: [Info], __br_info: Info, return: [ExprTppl]}

  sem get_StmtTpplOp_info =
  | ReturnStmtTpplOp x ->
    x.__br_info

  sem get_StmtTpplOp_terms =
  | ReturnStmtTpplOp x ->
    x.__br_terms

  sem unsplit_StmtTpplOp =
  | AtomP {self = ReturnStmtTpplOp x} ->
    (x.__br_info, ReturnStmtTppl
      { info =
          x.__br_info,
        return =
          match
            x.return
          with
            [ x1 ] ++ _ ++ ""
          then
            x1
          else
            never })

end

lang ParseTreePPL = FileTpplOp + TypeTpplOp + SequenceTypeTpplOp + AtomicRealTypeTpplOp + FunDeclTpplOp + IntegerExprTpplOp + StringExprTpplOp + RealExprTpplOp + VariableExprTpplOp + AddExprTpplOp + MulExprTpplOp + SubExprTpplOp + DivExprTpplOp + IsExprTpplOp + ToExprTpplOp + FunCallExprTpplOp + RecordExprTpplOp + AssumeStmtTpplOp + BernoulliExprTpplOp + ObserveStmtTpplOp + IfStmtTpplOp + ForLoopStmtTpplOp + ReturnStmtTpplOp + BadFileTpplAst + BadDeclTpplAst + BadTypeTpplAst + BadStmtTpplAst + BadExprTpplAst + LL1Parser + SemiTokenParser + UIntTokenParser + CommaTokenParser + WhitespaceParser + LIdentTokenParser + StringTokenParser + UFloatTokenParser + UIdentTokenParser + BracketTokenParser + OperatorTokenParser + TpplLineCommentParser + TpplMultilineCommentParser






  sem groupingsAllowed_ExprTpplOp =
  | (AddExprTpplOp _, MulExprTpplOp _) ->
    GRight
      {}
  | (MulExprTpplOp _, AddExprTpplOp _) ->
    GLeft
      {}
  | (AddExprTpplOp _, SubExprTpplOp _) ->
    GLeft
      {}
  | (SubExprTpplOp _, AddExprTpplOp _) ->
    GLeft
      {}
  | (AddExprTpplOp _, DivExprTpplOp _) ->
    GRight
      {}
  | (DivExprTpplOp _, AddExprTpplOp _) ->
    GLeft
      {}
  | (AddExprTpplOp _, FunCallExprTpplOp _) ->
    GRight
      {}
  | (MulExprTpplOp _, SubExprTpplOp _) ->
    GLeft
      {}
  | (SubExprTpplOp _, MulExprTpplOp _) ->
    GRight
      {}
  | (MulExprTpplOp _, DivExprTpplOp _) ->
    GLeft
      {}
  | (DivExprTpplOp _, MulExprTpplOp _) ->
    GLeft
      {}
  | (MulExprTpplOp _, FunCallExprTpplOp _) ->
    GRight
      {}
  | (SubExprTpplOp _, DivExprTpplOp _) ->
    GRight
      {}
  | (DivExprTpplOp _, SubExprTpplOp _) ->
    GLeft
      {}
  | (SubExprTpplOp _, FunCallExprTpplOp _) ->
    GRight
      {}
  | (DivExprTpplOp _, FunCallExprTpplOp _) ->
    GRight
      {}

end



let _table = use ParseTreePPL in let target =
  genParsingTable
    (let #var"FileTppl" =
       nameSym
         "FileTppl"
     in
     let #var"DeclTppl" =
       nameSym
         "DeclTppl"
     in
     let #var"TypeTppl" =
       nameSym
         "TypeTppl"
     in
     let #var"StmtTppl" =
       nameSym
         "StmtTppl"
     in
     let #var"ExprTppl" =
       nameSym
         "ExprTppl"
     in
     let #var"FileTpplAtom" =
       nameSym
         "FileTpplAtom"
     in
     let #var"FileTpplInfix" =
       nameSym
         "FileTpplInfix"
     in
     let #var"FileTpplPostfix" =
       nameSym
         "FileTpplPostfix"
     in
     let #var"FileTpplPrefix" =
       nameSym
         "FileTpplPrefix"
     in
     let #var"DeclTpplAtom" =
       nameSym
         "DeclTpplAtom"
     in
     let #var"DeclTpplInfix" =
       nameSym
         "DeclTpplInfix"
     in
     let #var"DeclTpplPostfix" =
       nameSym
         "DeclTpplPostfix"
     in
     let #var"DeclTpplPrefix" =
       nameSym
         "DeclTpplPrefix"
     in
     let #var"TypeTpplAtom" =
       nameSym
         "TypeTpplAtom"
     in
     let #var"TypeTpplInfix" =
       nameSym
         "TypeTpplInfix"
     in
     let #var"TypeTpplPostfix" =
       nameSym
         "TypeTpplPostfix"
     in
     let #var"TypeTpplPrefix" =
       nameSym
         "TypeTpplPrefix"
     in
     let #var"StmtTpplAtom" =
       nameSym
         "StmtTpplAtom"
     in
     let #var"StmtTpplInfix" =
       nameSym
         "StmtTpplInfix"
     in
     let #var"StmtTpplPostfix" =
       nameSym
         "StmtTpplPostfix"
     in
     let #var"StmtTpplPrefix" =
       nameSym
         "StmtTpplPrefix"
     in
     let #var"ExprTpplAtom" =
       nameSym
         "ExprTpplAtom"
     in
     let #var"ExprTpplInfix" =
       nameSym
         "ExprTpplInfix"
     in
     let #var"ExprTpplPostfix" =
       nameSym
         "ExprTpplPostfix"
     in
     let #var"ExprTpplPrefix" =
       nameSym
         "ExprTpplPrefix"
     in
     let kleene =
       nameSym
         "kleene"
     in
     let alt =
       nameSym
         "alt"
     in
     let alt1 =
       nameSym
         "alt"
     in
     let kleene1 =
       nameSym
         "kleene"
     in
     let alt2 =
       nameSym
         "alt"
     in
     let alt3 =
       nameSym
         "alt"
     in
     let kleene2 =
       nameSym
         "kleene"
     in
     let kleene3 =
       nameSym
         "kleene"
     in
     let alt4 =
       nameSym
         "alt"
     in
     let alt5 =
       nameSym
         "alt"
     in
     let alt6 =
       nameSym
         "alt"
     in
     let kleene4 =
       nameSym
         "kleene"
     in
     let alt7 =
       nameSym
         "alt"
     in
     let kleene5 =
       nameSym
         "kleene"
     in
     let alt8 =
       nameSym
         "alt"
     in
     let kleene6 =
       nameSym
         "kleene"
     in
     let kleene7 =
       nameSym
         "kleene"
     in
     let alt9 =
       nameSym
         "alt"
     in
     let kleene8 =
       nameSym
         "kleene"
     in
     let #var"FileTppl_lclosed" =
       nameSym
         "FileTppl_lclosed"
     in
     let #var"FileTppl_lopen" =
       nameSym
         "FileTppl_lopen"
     in
     let #var"DeclTppl_lclosed" =
       nameSym
         "DeclTppl_lclosed"
     in
     let #var"DeclTppl_lopen" =
       nameSym
         "DeclTppl_lopen"
     in
     let #var"TypeTppl_lclosed" =
       nameSym
         "TypeTppl_lclosed"
     in
     let #var"TypeTppl_lopen" =
       nameSym
         "TypeTppl_lopen"
     in
     let #var"StmtTppl_lclosed" =
       nameSym
         "StmtTppl_lclosed"
     in
     let #var"StmtTppl_lopen" =
       nameSym
         "StmtTppl_lopen"
     in
     let #var"ExprTppl_lclosed" =
       nameSym
         "ExprTppl_lclosed"
     in
     let #var"ExprTppl_lopen" =
       nameSym
         "ExprTppl_lopen"
     in
     { start =
         #var"FileTppl",
       productions =
         let config =
           { parenAllowed =
               parenAllowed_FileTpplOp,
             topAllowed =
               topAllowed_FileTpplOp,
             leftAllowed =
               leftAllowed_FileTpplOp,
             rightAllowed =
               rightAllowed_FileTpplOp,
             groupingsAllowed =
               groupingsAllowed_FileTpplOp }
         in
         let reportConfig =
           { parenAllowed =
               parenAllowed_FileTpplOp,
             topAllowed =
               topAllowed_FileTpplOp,
             terminalInfos =
               get_FileTpplOp_terms,
             getInfo =
               get_FileTpplOp_info,
             lpar =
               "(",
             rpar =
               ")" }
         in
         let addFileTpplOpAtom =
           lam #var"".
             lam x19.
               lam st.
                 optionMap
                   (breakableAddAtom
                      config
                      x19)
                   st
         in
         let addFileTpplOpInfix =
           lam p: {errors: (Ref) ([(Info, [Char])]), content: String}.
             lam x19.
               lam st.
                 match
                   st
                 with
                   Some st
                 then
                   let st =
                     breakableAddInfix
                       config
                       x19
                       st
                   in
                   (match
                        st
                      with
                        None _
                      then
                        modref
                          (p.errors)
                          (snoc
                             (deref
                                (p.errors))
                             (get_FileTpplOp_info
                               x19, "Invalid input"))
                      else
                        {})
                   ;st
                 else
                   st
         in
         let addFileTpplOpPrefix =
           lam #var"".
             lam x19.
               lam st.
                 optionMap
                   (breakableAddPrefix
                      config
                      x19)
                   st
         in
         let addFileTpplOpPostfix =
           lam p: {errors: (Ref) ([(Info, [Char])]), content: String}.
             lam x19.
               lam st.
                 match
                   st
                 with
                   Some st
                 then
                   let st =
                     breakableAddPostfix
                       config
                       x19
                       st
                   in
                   (match
                        st
                      with
                        None _
                      then
                        modref
                          (p.errors)
                          (snoc
                             (deref
                                (p.errors))
                             (get_FileTpplOp_info
                               x19, "Invalid input"))
                      else
                        {})
                   ;st
                 else
                   st
         in
         let finalizeFileTpplOp =
           lam p: {errors: (Ref) ([(Info, [Char])]), content: String}.
             lam st.
               let res61 =
                 optionBind
                   st
                   (lam st.
                      match
                        breakableFinalizeParse
                          config
                          st
                      with
                        Some (tops & ([ top ] ++ _ ++ ""))
                      then
                        let errs =
                          breakableDefaultHighlight
                            reportConfig
                            (p.content)
                            tops
                        in
                        let res61: (Info, FileTppl) =
                          unsplit_FileTpplOp
                            top
                        in
                        match
                          null
                            errs
                        with
                          true
                        then
                          Some
                            res61
                        else
                          (modref
                               (p.errors)
                               (concat
                                  (deref
                                     (p.errors))
                                  errs))
                          ;Some
                            (res61.#label"0", BadFileTppl
                              { info =
                                  res61.#label"0" })
                      else
                        (modref
                             (p.errors)
                             (snoc
                                (deref
                                   (p.errors))
                                (NoInfo
                                  {}, "Unfinished FileTppl")))
                        ;None
                          {})
               in
               optionGetOr
                 (NoInfo
                   {}, BadFileTppl
                   { info =
                       NoInfo
                         {} })
                 res61
         in
         let config1 =
           { parenAllowed =
               parenAllowed_DeclTpplOp,
             topAllowed =
               topAllowed_DeclTpplOp,
             leftAllowed =
               leftAllowed_DeclTpplOp,
             rightAllowed =
               rightAllowed_DeclTpplOp,
             groupingsAllowed =
               groupingsAllowed_DeclTpplOp }
         in
         let reportConfig1 =
           { parenAllowed =
               parenAllowed_DeclTpplOp,
             topAllowed =
               topAllowed_DeclTpplOp,
             terminalInfos =
               get_DeclTpplOp_terms,
             getInfo =
               get_DeclTpplOp_info,
             lpar =
               "(",
             rpar =
               ")" }
         in
         let addDeclTpplOpAtom =
           lam #var"".
             lam x19.
               lam st.
                 optionMap
                   (breakableAddAtom
                      config1
                      x19)
                   st
         in
         let addDeclTpplOpInfix =
           lam p: {errors: (Ref) ([(Info, [Char])]), content: String}.
             lam x19.
               lam st.
                 match
                   st
                 with
                   Some st
                 then
                   let st =
                     breakableAddInfix
                       config1
                       x19
                       st
                   in
                   (match
                        st
                      with
                        None _
                      then
                        modref
                          (p.errors)
                          (snoc
                             (deref
                                (p.errors))
                             (get_DeclTpplOp_info
                               x19, "Invalid input"))
                      else
                        {})
                   ;st
                 else
                   st
         in
         let addDeclTpplOpPrefix =
           lam #var"".
             lam x19.
               lam st.
                 optionMap
                   (breakableAddPrefix
                      config1
                      x19)
                   st
         in
         let addDeclTpplOpPostfix =
           lam p: {errors: (Ref) ([(Info, [Char])]), content: String}.
             lam x19.
               lam st.
                 match
                   st
                 with
                   Some st
                 then
                   let st =
                     breakableAddPostfix
                       config1
                       x19
                       st
                   in
                   (match
                        st
                      with
                        None _
                      then
                        modref
                          (p.errors)
                          (snoc
                             (deref
                                (p.errors))
                             (get_DeclTpplOp_info
                               x19, "Invalid input"))
                      else
                        {})
                   ;st
                 else
                   st
         in
         let finalizeDeclTpplOp =
           lam p: {errors: (Ref) ([(Info, [Char])]), content: String}.
             lam st.
               let res61 =
                 optionBind
                   st
                   (lam st.
                      match
                        breakableFinalizeParse
                          config1
                          st
                      with
                        Some (tops & ([ top ] ++ _ ++ ""))
                      then
                        let errs =
                          breakableDefaultHighlight
                            reportConfig1
                            (p.content)
                            tops
                        in
                        let res61: (Info, DeclTppl) =
                          unsplit_DeclTpplOp
                            top
                        in
                        match
                          null
                            errs
                        with
                          true
                        then
                          Some
                            res61
                        else
                          (modref
                               (p.errors)
                               (concat
                                  (deref
                                     (p.errors))
                                  errs))
                          ;Some
                            (res61.#label"0", BadDeclTppl
                              { info =
                                  res61.#label"0" })
                      else
                        (modref
                             (p.errors)
                             (snoc
                                (deref
                                   (p.errors))
                                (NoInfo
                                  {}, "Unfinished DeclTppl")))
                        ;None
                          {})
               in
               optionGetOr
                 (NoInfo
                   {}, BadDeclTppl
                   { info =
                       NoInfo
                         {} })
                 res61
         in
         let config2 =
           { parenAllowed =
               parenAllowed_TypeTpplOp,
             topAllowed =
               topAllowed_TypeTpplOp,
             leftAllowed =
               leftAllowed_TypeTpplOp,
             rightAllowed =
               rightAllowed_TypeTpplOp,
             groupingsAllowed =
               groupingsAllowed_TypeTpplOp }
         in
         let reportConfig2 =
           { parenAllowed =
               parenAllowed_TypeTpplOp,
             topAllowed =
               topAllowed_TypeTpplOp,
             terminalInfos =
               get_TypeTpplOp_terms,
             getInfo =
               get_TypeTpplOp_info,
             lpar =
               "(",
             rpar =
               ")" }
         in
         let addTypeTpplOpAtom =
           lam #var"".
             lam x19.
               lam st.
                 optionMap
                   (breakableAddAtom
                      config2
                      x19)
                   st
         in
         let addTypeTpplOpInfix =
           lam p: {errors: (Ref) ([(Info, [Char])]), content: String}.
             lam x19.
               lam st.
                 match
                   st
                 with
                   Some st
                 then
                   let st =
                     breakableAddInfix
                       config2
                       x19
                       st
                   in
                   (match
                        st
                      with
                        None _
                      then
                        modref
                          (p.errors)
                          (snoc
                             (deref
                                (p.errors))
                             (get_TypeTpplOp_info
                               x19, "Invalid input"))
                      else
                        {})
                   ;st
                 else
                   st
         in
         let addTypeTpplOpPrefix =
           lam #var"".
             lam x19.
               lam st.
                 optionMap
                   (breakableAddPrefix
                      config2
                      x19)
                   st
         in
         let addTypeTpplOpPostfix =
           lam p: {errors: (Ref) ([(Info, [Char])]), content: String}.
             lam x19.
               lam st.
                 match
                   st
                 with
                   Some st
                 then
                   let st =
                     breakableAddPostfix
                       config2
                       x19
                       st
                   in
                   (match
                        st
                      with
                        None _
                      then
                        modref
                          (p.errors)
                          (snoc
                             (deref
                                (p.errors))
                             (get_TypeTpplOp_info
                               x19, "Invalid input"))
                      else
                        {})
                   ;st
                 else
                   st
         in
         let finalizeTypeTpplOp =
           lam p: {errors: (Ref) ([(Info, [Char])]), content: String}.
             lam st.
               let res61 =
                 optionBind
                   st
                   (lam st.
                      match
                        breakableFinalizeParse
                          config2
                          st
                      with
                        Some (tops & ([ top ] ++ _ ++ ""))
                      then
                        let errs =
                          breakableDefaultHighlight
                            reportConfig2
                            (p.content)
                            tops
                        in
                        let res61: (Info, TypeTppl) =
                          unsplit_TypeTpplOp
                            top
                        in
                        match
                          null
                            errs
                        with
                          true
                        then
                          Some
                            res61
                        else
                          (modref
                               (p.errors)
                               (concat
                                  (deref
                                     (p.errors))
                                  errs))
                          ;Some
                            (res61.#label"0", BadTypeTppl
                              { info =
                                  res61.#label"0" })
                      else
                        (modref
                             (p.errors)
                             (snoc
                                (deref
                                   (p.errors))
                                (NoInfo
                                  {}, "Unfinished TypeTppl")))
                        ;None
                          {})
               in
               optionGetOr
                 (NoInfo
                   {}, BadTypeTppl
                   { info =
                       NoInfo
                         {} })
                 res61
         in
         let config3 =
           { parenAllowed =
               parenAllowed_StmtTpplOp,
             topAllowed =
               topAllowed_StmtTpplOp,
             leftAllowed =
               leftAllowed_StmtTpplOp,
             rightAllowed =
               rightAllowed_StmtTpplOp,
             groupingsAllowed =
               groupingsAllowed_StmtTpplOp }
         in
         let reportConfig3 =
           { parenAllowed =
               parenAllowed_StmtTpplOp,
             topAllowed =
               topAllowed_StmtTpplOp,
             terminalInfos =
               get_StmtTpplOp_terms,
             getInfo =
               get_StmtTpplOp_info,
             lpar =
               "(",
             rpar =
               ")" }
         in
         let addStmtTpplOpAtom =
           lam #var"".
             lam x19.
               lam st.
                 optionMap
                   (breakableAddAtom
                      config3
                      x19)
                   st
         in
         let addStmtTpplOpInfix =
           lam p: {errors: (Ref) ([(Info, [Char])]), content: String}.
             lam x19.
               lam st.
                 match
                   st
                 with
                   Some st
                 then
                   let st =
                     breakableAddInfix
                       config3
                       x19
                       st
                   in
                   (match
                        st
                      with
                        None _
                      then
                        modref
                          (p.errors)
                          (snoc
                             (deref
                                (p.errors))
                             (get_StmtTpplOp_info
                               x19, "Invalid input"))
                      else
                        {})
                   ;st
                 else
                   st
         in
         let addStmtTpplOpPrefix =
           lam #var"".
             lam x19.
               lam st.
                 optionMap
                   (breakableAddPrefix
                      config3
                      x19)
                   st
         in
         let addStmtTpplOpPostfix =
           lam p: {errors: (Ref) ([(Info, [Char])]), content: String}.
             lam x19.
               lam st.
                 match
                   st
                 with
                   Some st
                 then
                   let st =
                     breakableAddPostfix
                       config3
                       x19
                       st
                   in
                   (match
                        st
                      with
                        None _
                      then
                        modref
                          (p.errors)
                          (snoc
                             (deref
                                (p.errors))
                             (get_StmtTpplOp_info
                               x19, "Invalid input"))
                      else
                        {})
                   ;st
                 else
                   st
         in
         let finalizeStmtTpplOp =
           lam p: {errors: (Ref) ([(Info, [Char])]), content: String}.
             lam st.
               let res61 =
                 optionBind
                   st
                   (lam st.
                      match
                        breakableFinalizeParse
                          config3
                          st
                      with
                        Some (tops & ([ top ] ++ _ ++ ""))
                      then
                        let errs =
                          breakableDefaultHighlight
                            reportConfig3
                            (p.content)
                            tops
                        in
                        let res61: (Info, StmtTppl) =
                          unsplit_StmtTpplOp
                            top
                        in
                        match
                          null
                            errs
                        with
                          true
                        then
                          Some
                            res61
                        else
                          (modref
                               (p.errors)
                               (concat
                                  (deref
                                     (p.errors))
                                  errs))
                          ;Some
                            (res61.#label"0", BadStmtTppl
                              { info =
                                  res61.#label"0" })
                      else
                        (modref
                             (p.errors)
                             (snoc
                                (deref
                                   (p.errors))
                                (NoInfo
                                  {}, "Unfinished StmtTppl")))
                        ;None
                          {})
               in
               optionGetOr
                 (NoInfo
                   {}, BadStmtTppl
                   { info =
                       NoInfo
                         {} })
                 res61
         in
         let config4 =
           { parenAllowed =
               parenAllowed_ExprTpplOp,
             topAllowed =
               topAllowed_ExprTpplOp,
             leftAllowed =
               leftAllowed_ExprTpplOp,
             rightAllowed =
               rightAllowed_ExprTpplOp,
             groupingsAllowed =
               groupingsAllowed_ExprTpplOp }
         in
         let reportConfig4 =
           { parenAllowed =
               parenAllowed_ExprTpplOp,
             topAllowed =
               topAllowed_ExprTpplOp,
             terminalInfos =
               get_ExprTpplOp_terms,
             getInfo =
               get_ExprTpplOp_info,
             lpar =
               "(",
             rpar =
               ")" }
         in
         let addExprTpplOpAtom =
           lam #var"".
             lam x19.
               lam st.
                 optionMap
                   (breakableAddAtom
                      config4
                      x19)
                   st
         in
         let addExprTpplOpInfix =
           lam p: {errors: (Ref) ([(Info, [Char])]), content: String}.
             lam x19.
               lam st.
                 match
                   st
                 with
                   Some st
                 then
                   let st =
                     breakableAddInfix
                       config4
                       x19
                       st
                   in
                   (match
                        st
                      with
                        None _
                      then
                        modref
                          (p.errors)
                          (snoc
                             (deref
                                (p.errors))
                             (get_ExprTpplOp_info
                               x19, "Invalid input"))
                      else
                        {})
                   ;st
                 else
                   st
         in
         let addExprTpplOpPrefix =
           lam #var"".
             lam x19.
               lam st.
                 optionMap
                   (breakableAddPrefix
                      config4
                      x19)
                   st
         in
         let addExprTpplOpPostfix =
           lam p: {errors: (Ref) ([(Info, [Char])]), content: String}.
             lam x19.
               lam st.
                 match
                   st
                 with
                   Some st
                 then
                   let st =
                     breakableAddPostfix
                       config4
                       x19
                       st
                   in
                   (match
                        st
                      with
                        None _
                      then
                        modref
                          (p.errors)
                          (snoc
                             (deref
                                (p.errors))
                             (get_ExprTpplOp_info
                               x19, "Invalid input"))
                      else
                        {})
                   ;st
                 else
                   st
         in
         let finalizeExprTpplOp =
           lam p: {errors: (Ref) ([(Info, [Char])]), content: String}.
             lam st.
               let res61 =
                 optionBind
                   st
                   (lam st.
                      match
                        breakableFinalizeParse
                          config4
                          st
                      with
                        Some (tops & ([ top ] ++ _ ++ ""))
                      then
                        let errs =
                          breakableDefaultHighlight
                            reportConfig4
                            (p.content)
                            tops
                        in
                        let res61: (Info, ExprTppl) =
                          unsplit_ExprTpplOp
                            top
                        in
                        match
                          null
                            errs
                        with
                          true
                        then
                          Some
                            res61
                        else
                          (modref
                               (p.errors)
                               (concat
                                  (deref
                                     (p.errors))
                                  errs))
                          ;Some
                            (res61.#label"0", BadExprTppl
                              { info =
                                  res61.#label"0" })
                      else
                        (modref
                             (p.errors)
                             (snoc
                                (deref
                                   (p.errors))
                                (NoInfo
                                  {}, "Unfinished ExprTppl")))
                        ;None
                          {})
               in
               optionGetOr
                 (NoInfo
                   {}, BadExprTppl
                   { info =
                       NoInfo
                         {} })
                 res61
         in
         [ { nt =
               kleene,
             label =
               {},
             rhs =
               [ ntSym
                   #var"DeclTppl",
                 ntSym
                   kleene ],
             action =
               lam state: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res.
                   match
                     res
                   with
                     [ UserSym (info, val),
                       UserSym val1 ]
                   then
                     let val1: {__br_terms: [Info], __br_info: Info, decl: [DeclTppl]} =
                       val1
                     in
                     { __br_terms =
                         val1.__br_terms,
                       __br_info =
                         mergeInfo
                           info
                           (val1.__br_info),
                       decl =
                         concat
                           [ val ]
                           (val1.decl) }
                   else
                     never },
           { nt =
               kleene,
             label =
               {},
             rhs =
               "",
             action =
               lam state1: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res1.
                   match
                     res1
                   with
                     ""
                   then
                     { __br_terms =
                         "",
                       __br_info =
                         NoInfo
                           {},
                       decl =
                         "" }
                   else
                     never },
           { nt =
               #var"FileTpplAtom",
             label =
               {},
             rhs =
               [ ntSym
                   #var"DeclTppl",
                 ntSym
                   kleene ],
             action =
               lam state2: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res2.
                   match
                     res2
                   with
                     [ UserSym (info1, val2),
                       UserSym val1 ]
                   then
                     let val1: {__br_terms: [Info], __br_info: Info, decl: [DeclTppl]} =
                       val1
                     in
                     FileTpplOp
                       { __br_terms =
                           val1.__br_terms,
                         __br_info =
                           mergeInfo
                             info1
                             (val1.__br_info),
                         decl =
                           concat
                             [ val2 ]
                             (val1.decl) }
                   else
                     never },
           { nt =
               #var"TypeTpplAtom",
             label =
               {},
             rhs =
               [ tokSym
                   (UIdentRepr
                      {}) ],
             action =
               lam state3: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res3.
                   match
                     res3
                   with
                     [ TokParsed (UIdentTok x) ]
                   then
                     TypeTpplOp
                       { __br_terms =
                           [ x.info ],
                         __br_info =
                           x.info,
                         name =
                           [ { v =
                                 nameNoSym
                                   (x.val),
                               i =
                                 x.info } ] }
                   else
                     never },
           { nt =
               alt,
             label =
               {},
             rhs =
               "",
             action =
               lam state4: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res4.
                   match
                     res4
                   with
                     ""
                   then
                     { __br_terms =
                         "",
                       __br_info =
                         NoInfo
                           {},
                       size =
                         "" }
                   else
                     never },
           { nt =
               alt,
             label =
               {},
             rhs =
               [ tokSym
                   (IntRepr
                      {}) ],
             action =
               lam state5: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res5.
                   match
                     res5
                   with
                     [ TokParsed (IntTok x1) ]
                   then
                     { __br_terms =
                         [ x1.info ],
                       __br_info =
                         x1.info,
                       size =
                         [ { v =
                               x1.val,
                             i =
                               x1.info } ] }
                   else
                     never },
           { nt =
               #var"TypeTpplPostfix",
             label =
               {},
             rhs =
               [ litSym
                   "[",
                 ntSym
                   alt,
                 litSym
                   "]" ],
             action =
               lam state6: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res6.
                   match
                     res6
                   with
                     [ LitParsed l,
                       UserSym val3,
                       LitParsed l1 ]
                   then
                     let val3: {__br_terms: [Info], __br_info: Info, size: [{v: Int, i: Info}]} =
                       val3
                     in
                     SequenceTypeTpplOp
                       { __br_terms =
                           join
                             [ [ l.info ],
                               val3.__br_terms,
                               [ l1.info ] ],
                         __br_info =
                           foldl
                             mergeInfo
                             (l.info)
                             [ val3.__br_info,
                               l1.info ],
                         size =
                           val3.size }
                   else
                     never },
           { nt =
               #var"TypeTpplAtom",
             label =
               {},
             rhs =
               [ litSym
                   "Real" ],
             action =
               lam state7: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res7.
                   match
                     res7
                   with
                     [ LitParsed l2 ]
                   then
                     AtomicRealTypeTpplOp
                       { __br_terms =
                           [ l2.info ],
                         __br_info =
                           l2.info }
                   else
                     never },
           { nt =
               alt1,
             label =
               {},
             rhs =
               "",
             action =
               lam state8: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res8.
                   match
                     res8
                   with
                     ""
                   then
                     { __br_terms =
                         "",
                       __br_info =
                         NoInfo
                           {},
                       model =
                         "" }
                   else
                     never },
           { nt =
               alt1,
             label =
               {},
             rhs =
               [ litSym
                   "model" ],
             action =
               lam state9: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res9.
                   match
                     res9
                   with
                     [ LitParsed l3 ]
                   then
                     { __br_terms =
                         [ l3.info ],
                       __br_info =
                         l3.info,
                       model =
                         [ l3.info ] }
                   else
                     never },
           { nt =
               kleene1,
             label =
               {},
             rhs =
               [ litSym
                   ",",
                 tokSym
                   (LIdentRepr
                      {}),
                 litSym
                   ":",
                 ntSym
                   #var"TypeTppl",
                 ntSym
                   kleene1 ],
             action =
               lam state10: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res10.
                   match
                     res10
                   with
                     [ LitParsed l4,
                       TokParsed (LIdentTok x2),
                       LitParsed l5,
                       UserSym (info2, val4),
                       UserSym val5 ]
                   then
                     let val5: {__br_terms: [Info], __br_info: Info, args: [{name: {v: Name, i: Info}, ty: TypeTppl}]} =
                       val5
                     in
                     { __br_terms =
                         join
                           [ [ l4.info ],
                             [ x2.info ],
                             [ l5.info ],
                             val5.__br_terms ],
                       __br_info =
                         foldl
                           mergeInfo
                           (l4.info)
                           [ x2.info,
                             l5.info,
                             info2,
                             val5.__br_info ],
                       args =
                         concat
                           [ { name =
                                 match
                                   [ { v =
                                         nameNoSym
                                           (x2.val),
                                       i =
                                         x2.info } ]
                                 with
                                   [ x3 ] ++ _ ++ ""
                                 then
                                   x3
                                 else
                                   never,
                               ty =
                                 match
                                   [ val4 ]
                                 with
                                   [ x4 ] ++ _ ++ ""
                                 then
                                   x4
                                 else
                                   never } ]
                           (val5.args) }
                   else
                     never },
           { nt =
               kleene1,
             label =
               {},
             rhs =
               "",
             action =
               lam state11: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res11.
                   match
                     res11
                   with
                     ""
                   then
                     { __br_terms =
                         "",
                       __br_info =
                         NoInfo
                           {},
                       args =
                         "" }
                   else
                     never },
           { nt =
               alt2,
             label =
               {},
             rhs =
               "",
             action =
               lam state12: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res12.
                   match
                     res12
                   with
                     ""
                   then
                     { __br_terms =
                         "",
                       __br_info =
                         NoInfo
                           {},
                       args =
                         "" }
                   else
                     never },
           { nt =
               alt2,
             label =
               {},
             rhs =
               [ tokSym
                   (LIdentRepr
                      {}),
                 litSym
                   ":",
                 ntSym
                   #var"TypeTppl",
                 ntSym
                   kleene1 ],
             action =
               lam state13: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res13.
                   match
                     res13
                   with
                     [ TokParsed (LIdentTok x5),
                       LitParsed l6,
                       UserSym (info3, val6),
                       UserSym val5 ]
                   then
                     let val5: {__br_terms: [Info], __br_info: Info, args: [{name: {v: Name, i: Info}, ty: TypeTppl}]} =
                       val5
                     in
                     { __br_terms =
                         join
                           [ [ x5.info ],
                             [ l6.info ],
                             val5.__br_terms ],
                       __br_info =
                         foldl
                           mergeInfo
                           (x5.info)
                           [ l6.info,
                             info3,
                             val5.__br_info ],
                       args =
                         concat
                           [ { name =
                                 match
                                   [ { v =
                                         nameNoSym
                                           (x5.val),
                                       i =
                                         x5.info } ]
                                 with
                                   [ x6 ] ++ _ ++ ""
                                 then
                                   x6
                                 else
                                   never,
                               ty =
                                 match
                                   [ val6 ]
                                 with
                                   [ x7 ] ++ _ ++ ""
                                 then
                                   x7
                                 else
                                   never } ]
                           (val5.args) }
                   else
                     never },
           { nt =
               alt3,
             label =
               {},
             rhs =
               "",
             action =
               lam state14: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res14.
                   match
                     res14
                   with
                     ""
                   then
                     { __br_terms =
                         "",
                       __br_info =
                         NoInfo
                           {},
                       returnTy =
                         "" }
                   else
                     never },
           { nt =
               alt3,
             label =
               {},
             rhs =
               [ litSym
                   ":",
                 ntSym
                   #var"TypeTppl" ],
             action =
               lam state15: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res15.
                   match
                     res15
                   with
                     [ LitParsed l7,
                       UserSym (info4, val7) ]
                   then
                     { __br_terms =
                         [ l7.info ],
                       __br_info =
                         mergeInfo
                           (l7.info)
                           info4,
                       returnTy =
                         [ val7 ] }
                   else
                     never },
           { nt =
               kleene2,
             label =
               {},
             rhs =
               [ ntSym
                   #var"StmtTppl",
                 ntSym
                   kleene2 ],
             action =
               lam state16: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res16.
                   match
                     res16
                   with
                     [ UserSym (info5, val8),
                       UserSym val9 ]
                   then
                     let val9: {__br_terms: [Info], __br_info: Info, body: [StmtTppl]} =
                       val9
                     in
                     { __br_terms =
                         val9.__br_terms,
                       __br_info =
                         mergeInfo
                           info5
                           (val9.__br_info),
                       body =
                         concat
                           [ val8 ]
                           (val9.body) }
                   else
                     never },
           { nt =
               kleene2,
             label =
               {},
             rhs =
               "",
             action =
               lam state17: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res17.
                   match
                     res17
                   with
                     ""
                   then
                     { __br_terms =
                         "",
                       __br_info =
                         NoInfo
                           {},
                       body =
                         "" }
                   else
                     never },
           { nt =
               #var"DeclTpplAtom",
             label =
               {},
             rhs =
               [ ntSym
                   alt1,
                 litSym
                   "function",
                 tokSym
                   (LIdentRepr
                      {}),
                 litSym
                   "(",
                 ntSym
                   alt2,
                 litSym
                   ")",
                 ntSym
                   alt3,
                 litSym
                   "{",
                 ntSym
                   #var"StmtTppl",
                 ntSym
                   kleene2,
                 litSym
                   "}" ],
             action =
               lam state18: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res18.
                   match
                     res18
                   with
                     [ UserSym val10,
                       LitParsed l8,
                       TokParsed (LIdentTok x8),
                       LitParsed l9,
                       UserSym val11,
                       LitParsed l10,
                       UserSym val12,
                       LitParsed l11,
                       UserSym (info6, val13),
                       UserSym val9,
                       LitParsed l12 ]
                   then
                     let val10: {__br_terms: [Info], __br_info: Info, model: [Info]} =
                       val10
                     in
                     let val11: {__br_terms: [Info], __br_info: Info, args: [{name: {v: Name, i: Info}, ty: TypeTppl}]} =
                       val11
                     in
                     let val12: {__br_terms: [Info], __br_info: Info, returnTy: [TypeTppl]} =
                       val12
                     in
                     let val9: {__br_terms: [Info], __br_info: Info, body: [StmtTppl]} =
                       val9
                     in
                     FunDeclTpplOp
                       { __br_terms =
                           join
                             [ val10.__br_terms,
                               [ l8.info ],
                               [ x8.info ],
                               [ l9.info ],
                               val11.__br_terms,
                               [ l10.info ],
                               val12.__br_terms,
                               [ l11.info ],
                               val9.__br_terms,
                               [ l12.info ] ],
                         __br_info =
                           foldl
                             mergeInfo
                             (val10.__br_info)
                             [ l8.info,
                               x8.info,
                               l9.info,
                               val11.__br_info,
                               l10.info,
                               val12.__br_info,
                               l11.info,
                               info6,
                               val9.__br_info,
                               l12.info ],
                         name =
                           [ { v =
                                 nameNoSym
                                   (x8.val),
                               i =
                                 x8.info } ],
                         model =
                           val10.model,
                         args =
                           val11.args,
                         returnTy =
                           val12.returnTy,
                         body =
                           concat
                             [ val13 ]
                             (val9.body) }
                   else
                     never },
           { nt =
               #var"ExprTpplAtom",
             label =
               {},
             rhs =
               [ tokSym
                   (IntRepr
                      {}) ],
             action =
               lam state19: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res19.
                   match
                     res19
                   with
                     [ TokParsed (IntTok x9) ]
                   then
                     IntegerExprTpplOp
                       { __br_terms =
                           [ x9.info ],
                         __br_info =
                           x9.info,
                         val =
                           [ { v =
                                 x9.val,
                               i =
                                 x9.info } ] }
                   else
                     never },
           { nt =
               #var"ExprTpplAtom",
             label =
               {},
             rhs =
               [ tokSym
                   (StringRepr
                      {}) ],
             action =
               lam state20: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res20.
                   match
                     res20
                   with
                     [ TokParsed (StringTok x10) ]
                   then
                     StringExprTpplOp
                       { __br_terms =
                           [ x10.info ],
                         __br_info =
                           x10.info,
                         val =
                           [ { v =
                                 x10.val,
                               i =
                                 x10.info } ] }
                   else
                     never },
           { nt =
               #var"ExprTpplAtom",
             label =
               {},
             rhs =
               [ tokSym
                   (FloatRepr
                      {}) ],
             action =
               lam state21: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res21.
                   match
                     res21
                   with
                     [ TokParsed (FloatTok x11) ]
                   then
                     RealExprTpplOp
                       { __br_terms =
                           [ x11.info ],
                         __br_info =
                           x11.info,
                         val =
                           [ { v =
                                 x11.val,
                               i =
                                 x11.info } ] }
                   else
                     never },
           { nt =
               #var"ExprTpplAtom",
             label =
               {},
             rhs =
               [ tokSym
                   (LIdentRepr
                      {}) ],
             action =
               lam state22: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res22.
                   match
                     res22
                   with
                     [ TokParsed (LIdentTok x12) ]
                   then
                     VariableExprTpplOp
                       { __br_terms =
                           [ x12.info ],
                         __br_info =
                           x12.info,
                         ident =
                           [ { v =
                                 nameNoSym
                                   (x12.val),
                               i =
                                 x12.info } ] }
                   else
                     never },
           { nt =
               #var"ExprTpplInfix",
             label =
               {},
             rhs =
               [ litSym
                   "+" ],
             action =
               lam state23: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res23.
                   match
                     res23
                   with
                     [ LitParsed l13 ]
                   then
                     AddExprTpplOp
                       { __br_terms =
                           [ l13.info ],
                         __br_info =
                           l13.info }
                   else
                     never },
           { nt =
               #var"ExprTpplInfix",
             label =
               {},
             rhs =
               [ litSym
                   "*" ],
             action =
               lam state24: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res24.
                   match
                     res24
                   with
                     [ LitParsed l14 ]
                   then
                     MulExprTpplOp
                       { __br_terms =
                           [ l14.info ],
                         __br_info =
                           l14.info }
                   else
                     never },
           { nt =
               #var"ExprTpplInfix",
             label =
               {},
             rhs =
               [ litSym
                   "-" ],
             action =
               lam state25: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res25.
                   match
                     res25
                   with
                     [ LitParsed l15 ]
                   then
                     SubExprTpplOp
                       { __br_terms =
                           [ l15.info ],
                         __br_info =
                           l15.info }
                   else
                     never },
           { nt =
               #var"ExprTpplInfix",
             label =
               {},
             rhs =
               [ litSym
                   "/" ],
             action =
               lam state26: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res26.
                   match
                     res26
                   with
                     [ LitParsed l16 ]
                   then
                     DivExprTpplOp
                       { __br_terms =
                           [ l16.info ],
                         __br_info =
                           l16.info }
                   else
                     never },
           { nt =
               #var"ExprTpplPostfix",
             label =
               {},
             rhs =
               [ litSym
                   "is",
                 tokSym
                   (UIdentRepr
                      {}) ],
             action =
               lam state27: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res27.
                   match
                     res27
                   with
                     [ LitParsed l17,
                       TokParsed (UIdentTok x13) ]
                   then
                     IsExprTpplOp
                       { __br_terms =
                           concat
                             [ l17.info ]
                             [ x13.info ],
                         __br_info =
                           mergeInfo
                             (l17.info)
                             (x13.info),
                         constructor =
                           [ { v =
                                 nameNoSym
                                   (x13.val),
                               i =
                                 x13.info } ] }
                   else
                     never },
           { nt =
               #var"ExprTpplInfix",
             label =
               {},
             rhs =
               [ litSym
                   "to" ],
             action =
               lam state28: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res28.
                   match
                     res28
                   with
                     [ LitParsed l18 ]
                   then
                     ToExprTpplOp
                       { __br_terms =
                           [ l18.info ],
                         __br_info =
                           l18.info }
                   else
                     never },
           { nt =
               kleene3,
             label =
               {},
             rhs =
               [ litSym
                   ",",
                 ntSym
                   #var"ExprTppl",
                 ntSym
                   kleene3 ],
             action =
               lam state29: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res29.
                   match
                     res29
                   with
                     [ LitParsed l19,
                       UserSym (info7, val14),
                       UserSym val15 ]
                   then
                     let val15: {__br_terms: [Info], __br_info: Info, args: [ExprTppl]} =
                       val15
                     in
                     { __br_terms =
                         concat
                           [ l19.info ]
                           (val15.__br_terms),
                       __br_info =
                         foldl
                           mergeInfo
                           (l19.info)
                           [ info7,
                             val15.__br_info ],
                       args =
                         concat
                           [ val14 ]
                           (val15.args) }
                   else
                     never },
           { nt =
               kleene3,
             label =
               {},
             rhs =
               "",
             action =
               lam state30: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res30.
                   match
                     res30
                   with
                     ""
                   then
                     { __br_terms =
                         "",
                       __br_info =
                         NoInfo
                           {},
                       args =
                         "" }
                   else
                     never },
           { nt =
               alt4,
             label =
               {},
             rhs =
               "",
             action =
               lam state31: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res31.
                   match
                     res31
                   with
                     ""
                   then
                     { __br_terms =
                         "",
                       __br_info =
                         NoInfo
                           {},
                       args =
                         "" }
                   else
                     never },
           { nt =
               alt4,
             label =
               {},
             rhs =
               [ ntSym
                   #var"ExprTppl",
                 ntSym
                   kleene3 ],
             action =
               lam state32: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res32.
                   match
                     res32
                   with
                     [ UserSym (info8, val16),
                       UserSym val15 ]
                   then
                     let val15: {__br_terms: [Info], __br_info: Info, args: [ExprTppl]} =
                       val15
                     in
                     { __br_terms =
                         val15.__br_terms,
                       __br_info =
                         mergeInfo
                           info8
                           (val15.__br_info),
                       args =
                         concat
                           [ val16 ]
                           (val15.args) }
                   else
                     never },
           { nt =
               #var"ExprTpplPostfix",
             label =
               {},
             rhs =
               [ litSym
                   "(",
                 ntSym
                   alt4,
                 litSym
                   ")" ],
             action =
               lam state33: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res33.
                   match
                     res33
                   with
                     [ LitParsed l20,
                       UserSym val17,
                       LitParsed l21 ]
                   then
                     let val17: {__br_terms: [Info], __br_info: Info, args: [ExprTppl]} =
                       val17
                     in
                     FunCallExprTpplOp
                       { __br_terms =
                           join
                             [ [ l20.info ],
                               val17.__br_terms,
                               [ l21.info ] ],
                         __br_info =
                           foldl
                             mergeInfo
                             (l20.info)
                             [ val17.__br_info,
                               l21.info ],
                         args =
                           val17.args }
                   else
                     never },
           { nt =
               alt5,
             label =
               {},
             rhs =
               "",
             action =
               lam state34: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res34.
                   match
                     res34
                   with
                     ""
                   then
                     { __br_terms =
                         "",
                       __br_info =
                         NoInfo
                           {},
                       value =
                         "" }
                   else
                     never },
           { nt =
               alt5,
             label =
               {},
             rhs =
               [ litSym
                   "=",
                 ntSym
                   #var"ExprTppl" ],
             action =
               lam state35: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res35.
                   match
                     res35
                   with
                     [ LitParsed l22,
                       UserSym (info9, val18) ]
                   then
                     { __br_terms =
                         [ l22.info ],
                       __br_info =
                         mergeInfo
                           (l22.info)
                           info9,
                       value =
                         [ val18 ] }
                   else
                     never },
           { nt =
               alt6,
             label =
               {},
             rhs =
               "",
             action =
               lam state36: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res36.
                   match
                     res36
                   with
                     ""
                   then
                     { __br_terms =
                         "",
                       __br_info =
                         NoInfo
                           {},
                       value =
                         "" }
                   else
                     never },
           { nt =
               alt6,
             label =
               {},
             rhs =
               [ litSym
                   "=",
                 ntSym
                   #var"ExprTppl" ],
             action =
               lam state37: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res37.
                   match
                     res37
                   with
                     [ LitParsed l23,
                       UserSym (info10, val19) ]
                   then
                     { __br_terms =
                         [ l23.info ],
                       __br_info =
                         mergeInfo
                           (l23.info)
                           info10,
                       value =
                         [ val19 ] }
                   else
                     never },
           { nt =
               kleene4,
             label =
               {},
             rhs =
               [ litSym
                   ",",
                 tokSym
                   (LIdentRepr
                      {}),
                 ntSym
                   alt6,
                 ntSym
                   kleene4 ],
             action =
               lam state38: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res38.
                   match
                     res38
                   with
                     [ LitParsed l24,
                       TokParsed (LIdentTok x14),
                       UserSym val20,
                       UserSym val21 ]
                   then
                     let val20: {__br_terms: [Info], __br_info: Info, value: [ExprTppl]} =
                       val20
                     in
                     let val21: {__br_terms: [Info], __br_info: Info, value: [ExprTppl], key: [{v: Name, i: Info}]} =
                       val21
                     in
                     { __br_terms =
                         join
                           [ [ l24.info ],
                             [ x14.info ],
                             val20.__br_terms,
                             val21.__br_terms ],
                       __br_info =
                         foldl
                           mergeInfo
                           (l24.info)
                           [ x14.info,
                             val20.__br_info,
                             val21.__br_info ],
                       value =
                         concat
                           (val20.value)
                           (val21.value),
                       key =
                         concat
                           [ { v =
                                 nameNoSym
                                   (x14.val),
                               i =
                                 x14.info } ]
                           (val21.key) }
                   else
                     never },
           { nt =
               kleene4,
             label =
               {},
             rhs =
               "",
             action =
               lam state39: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res39.
                   match
                     res39
                   with
                     ""
                   then
                     { __br_terms =
                         "",
                       __br_info =
                         NoInfo
                           {},
                       value =
                         "",
                       key =
                         "" }
                   else
                     never },
           { nt =
               alt7,
             label =
               {},
             rhs =
               "",
             action =
               lam state40: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res40.
                   match
                     res40
                   with
                     ""
                   then
                     { __br_terms =
                         "",
                       __br_info =
                         NoInfo
                           {},
                       value =
                         "",
                       key =
                         "" }
                   else
                     never },
           { nt =
               alt7,
             label =
               {},
             rhs =
               [ tokSym
                   (LIdentRepr
                      {}),
                 ntSym
                   alt5,
                 ntSym
                   kleene4 ],
             action =
               lam state41: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res41.
                   match
                     res41
                   with
                     [ TokParsed (LIdentTok x15),
                       UserSym val22,
                       UserSym val21 ]
                   then
                     let val22: {__br_terms: [Info], __br_info: Info, value: [ExprTppl]} =
                       val22
                     in
                     let val21: {__br_terms: [Info], __br_info: Info, value: [ExprTppl], key: [{v: Name, i: Info}]} =
                       val21
                     in
                     { __br_terms =
                         join
                           [ [ x15.info ],
                             val22.__br_terms,
                             val21.__br_terms ],
                       __br_info =
                         foldl
                           mergeInfo
                           (x15.info)
                           [ val22.__br_info,
                             val21.__br_info ],
                       value =
                         concat
                           (val22.value)
                           (val21.value),
                       key =
                         concat
                           [ { v =
                                 nameNoSym
                                   (x15.val),
                               i =
                                 x15.info } ]
                           (val21.key) }
                   else
                     never },
           { nt =
               #var"ExprTpplAtom",
             label =
               {},
             rhs =
               [ litSym
                   "{",
                 ntSym
                   alt7,
                 litSym
                   "}" ],
             action =
               lam state42: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res42.
                   match
                     res42
                   with
                     [ LitParsed l25,
                       UserSym val23,
                       LitParsed l26 ]
                   then
                     let val23: {__br_terms: [Info], __br_info: Info, value: [ExprTppl], key: [{v: Name, i: Info}]} =
                       val23
                     in
                     RecordExprTpplOp
                       { __br_terms =
                           join
                             [ [ l25.info ],
                               val23.__br_terms,
                               [ l26.info ] ],
                         __br_info =
                           foldl
                             mergeInfo
                             (l25.info)
                             [ val23.__br_info,
                               l26.info ],
                         value =
                           val23.value,
                         key =
                           val23.key }
                   else
                     never },
           { nt =
               #var"StmtTpplAtom",
             label =
               {},
             rhs =
               [ tokSym
                   (LIdentRepr
                      {}),
                 litSym
                   "~",
                 ntSym
                   #var"ExprTppl" ],
             action =
               lam state43: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res43.
                   match
                     res43
                   with
                     [ TokParsed (LIdentTok x16),
                       LitParsed l27,
                       UserSym (info11, val24) ]
                   then
                     AssumeStmtTpplOp
                       { __br_terms =
                           concat
                             [ x16.info ]
                             [ l27.info ],
                         __br_info =
                           foldl
                             mergeInfo
                             (x16.info)
                             [ l27.info,
                               info11 ],
                         dist =
                           [ val24 ],
                         randomVar =
                           [ { v =
                                 nameNoSym
                                   (x16.val),
                               i =
                                 x16.info } ] }
                   else
                     never },
           { nt =
               #var"ExprTpplAtom",
             label =
               {},
             rhs =
               [ litSym
                   "Bernoulli",
                 litSym
                   "(",
                 ntSym
                   #var"ExprTppl",
                 litSym
                   ")" ],
             action =
               lam state44: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res44.
                   match
                     res44
                   with
                     [ LitParsed l28,
                       LitParsed l29,
                       UserSym (info12, val25),
                       LitParsed l30 ]
                   then
                     BernoulliExprTpplOp
                       { __br_terms =
                           join
                             [ [ l28.info ],
                               [ l29.info ],
                               [ l30.info ] ],
                         __br_info =
                           foldl
                             mergeInfo
                             (l28.info)
                             [ l29.info,
                               info12,
                               l30.info ],
                         prob =
                           [ val25 ] }
                   else
                     never },
           { nt =
               kleene5,
             label =
               {},
             rhs =
               [ litSym
                   ",",
                 ntSym
                   #var"ExprTppl",
                 ntSym
                   kleene5 ],
             action =
               lam state45: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res45.
                   match
                     res45
                   with
                     [ LitParsed l31,
                       UserSym (info13, val26),
                       UserSym val27 ]
                   then
                     let val27: {__br_terms: [Info], __br_info: Info, args: [ExprTppl]} =
                       val27
                     in
                     { __br_terms =
                         concat
                           [ l31.info ]
                           (val27.__br_terms),
                       __br_info =
                         foldl
                           mergeInfo
                           (l31.info)
                           [ info13,
                             val27.__br_info ],
                       args =
                         concat
                           [ val26 ]
                           (val27.args) }
                   else
                     never },
           { nt =
               kleene5,
             label =
               {},
             rhs =
               "",
             action =
               lam state46: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res46.
                   match
                     res46
                   with
                     ""
                   then
                     { __br_terms =
                         "",
                       __br_info =
                         NoInfo
                           {},
                       args =
                         "" }
                   else
                     never },
           { nt =
               alt8,
             label =
               {},
             rhs =
               "",
             action =
               lam state47: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res47.
                   match
                     res47
                   with
                     ""
                   then
                     { __br_terms =
                         "",
                       __br_info =
                         NoInfo
                           {},
                       args =
                         "" }
                   else
                     never },
           { nt =
               alt8,
             label =
               {},
             rhs =
               [ ntSym
                   #var"ExprTppl",
                 ntSym
                   kleene5 ],
             action =
               lam state48: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res48.
                   match
                     res48
                   with
                     [ UserSym (info14, val28),
                       UserSym val27 ]
                   then
                     let val27: {__br_terms: [Info], __br_info: Info, args: [ExprTppl]} =
                       val27
                     in
                     { __br_terms =
                         val27.__br_terms,
                       __br_info =
                         mergeInfo
                           info14
                           (val27.__br_info),
                       args =
                         concat
                           [ val28 ]
                           (val27.args) }
                   else
                     never },
           { nt =
               #var"StmtTpplAtom",
             label =
               {},
             rhs =
               [ litSym
                   "observe",
                 ntSym
                   #var"ExprTppl",
                 litSym
                   "~",
                 tokSym
                   (UIdentRepr
                      {}),
                 litSym
                   "(",
                 ntSym
                   alt8,
                 litSym
                   ")" ],
             action =
               lam state49: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res49.
                   match
                     res49
                   with
                     [ LitParsed l32,
                       UserSym (info15, val29),
                       LitParsed l33,
                       TokParsed (UIdentTok x17),
                       LitParsed l34,
                       UserSym val30,
                       LitParsed l35 ]
                   then
                     let val30: {__br_terms: [Info], __br_info: Info, args: [ExprTppl]} =
                       val30
                     in
                     ObserveStmtTpplOp
                       { __br_terms =
                           join
                             [ [ l32.info ],
                               [ l33.info ],
                               [ x17.info ],
                               [ l34.info ],
                               val30.__br_terms,
                               [ l35.info ] ],
                         __br_info =
                           foldl
                             mergeInfo
                             (l32.info)
                             [ info15,
                               l33.info,
                               x17.info,
                               l34.info,
                               val30.__br_info,
                               l35.info ],
                         args =
                           val30.args,
                         value =
                           [ val29 ],
                         distribution =
                           [ { v =
                                 nameNoSym
                                   (x17.val),
                               i =
                                 x17.info } ] }
                   else
                     never },
           { nt =
               kleene6,
             label =
               {},
             rhs =
               [ ntSym
                   #var"StmtTppl",
                 ntSym
                   kleene6 ],
             action =
               lam state50: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res50.
                   match
                     res50
                   with
                     [ UserSym (info16, val31),
                       UserSym val32 ]
                   then
                     let val32: {__br_terms: [Info], __br_info: Info, ifTrueStmts: [StmtTppl]} =
                       val32
                     in
                     { __br_terms =
                         val32.__br_terms,
                       __br_info =
                         mergeInfo
                           info16
                           (val32.__br_info),
                       ifTrueStmts =
                         concat
                           [ val31 ]
                           (val32.ifTrueStmts) }
                   else
                     never },
           { nt =
               kleene6,
             label =
               {},
             rhs =
               "",
             action =
               lam state51: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res51.
                   match
                     res51
                   with
                     ""
                   then
                     { __br_terms =
                         "",
                       __br_info =
                         NoInfo
                           {},
                       ifTrueStmts =
                         "" }
                   else
                     never },
           { nt =
               kleene7,
             label =
               {},
             rhs =
               [ ntSym
                   #var"StmtTppl",
                 ntSym
                   kleene7 ],
             action =
               lam state52: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res52.
                   match
                     res52
                   with
                     [ UserSym (info17, val33),
                       UserSym val34 ]
                   then
                     let val34: {__br_terms: [Info], __br_info: Info, ifFalseStmts: [StmtTppl]} =
                       val34
                     in
                     { __br_terms =
                         val34.__br_terms,
                       __br_info =
                         mergeInfo
                           info17
                           (val34.__br_info),
                       ifFalseStmts =
                         concat
                           [ val33 ]
                           (val34.ifFalseStmts) }
                   else
                     never },
           { nt =
               kleene7,
             label =
               {},
             rhs =
               "",
             action =
               lam state53: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res53.
                   match
                     res53
                   with
                     ""
                   then
                     { __br_terms =
                         "",
                       __br_info =
                         NoInfo
                           {},
                       ifFalseStmts =
                         "" }
                   else
                     never },
           { nt =
               alt9,
             label =
               {},
             rhs =
               "",
             action =
               lam state54: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res54.
                   match
                     res54
                   with
                     ""
                   then
                     { __br_terms =
                         "",
                       __br_info =
                         NoInfo
                           {},
                       ifFalseStmts =
                         "" }
                   else
                     never },
           { nt =
               alt9,
             label =
               {},
             rhs =
               [ litSym
                   "else",
                 litSym
                   "{",
                 ntSym
                   #var"StmtTppl",
                 ntSym
                   kleene7,
                 litSym
                   "}" ],
             action =
               lam state55: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res55.
                   match
                     res55
                   with
                     [ LitParsed l36,
                       LitParsed l37,
                       UserSym (info18, val35),
                       UserSym val34,
                       LitParsed l38 ]
                   then
                     let val34: {__br_terms: [Info], __br_info: Info, ifFalseStmts: [StmtTppl]} =
                       val34
                     in
                     { __br_terms =
                         join
                           [ [ l36.info ],
                             [ l37.info ],
                             val34.__br_terms,
                             [ l38.info ] ],
                       __br_info =
                         foldl
                           mergeInfo
                           (l36.info)
                           [ l37.info,
                             info18,
                             val34.__br_info,
                             l38.info ],
                       ifFalseStmts =
                         concat
                           [ val35 ]
                           (val34.ifFalseStmts) }
                   else
                     never },
           { nt =
               #var"StmtTpplAtom",
             label =
               {},
             rhs =
               [ litSym
                   "if",
                 ntSym
                   #var"ExprTppl",
                 litSym
                   "{",
                 ntSym
                   #var"StmtTppl",
                 ntSym
                   kleene6,
                 litSym
                   "}",
                 ntSym
                   alt9,
                 litSym
                   "}" ],
             action =
               lam state56: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res56.
                   match
                     res56
                   with
                     [ LitParsed l39,
                       UserSym (info19, val36),
                       LitParsed l40,
                       UserSym (info20, val37),
                       UserSym val32,
                       LitParsed l41,
                       UserSym val38,
                       LitParsed l42 ]
                   then
                     let val32: {__br_terms: [Info], __br_info: Info, ifTrueStmts: [StmtTppl]} =
                       val32
                     in
                     let val38: {__br_terms: [Info], __br_info: Info, ifFalseStmts: [StmtTppl]} =
                       val38
                     in
                     IfStmtTpplOp
                       { __br_terms =
                           join
                             [ [ l39.info ],
                               [ l40.info ],
                               val32.__br_terms,
                               [ l41.info ],
                               val38.__br_terms,
                               [ l42.info ] ],
                         __br_info =
                           foldl
                             mergeInfo
                             (l39.info)
                             [ info19,
                               l40.info,
                               info20,
                               val32.__br_info,
                               l41.info,
                               val38.__br_info,
                               l42.info ],
                         ifTrueStmts =
                           concat
                             [ val37 ]
                             (val32.ifTrueStmts),
                         ifFalseStmts =
                           val38.ifFalseStmts,
                         condition =
                           [ val36 ] }
                   else
                     never },
           { nt =
               kleene8,
             label =
               {},
             rhs =
               [ ntSym
                   #var"StmtTppl",
                 ntSym
                   kleene8 ],
             action =
               lam state57: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res57.
                   match
                     res57
                   with
                     [ UserSym (info21, val39),
                       UserSym val40 ]
                   then
                     let val40: {__br_terms: [Info], __br_info: Info, forStmts: [StmtTppl]} =
                       val40
                     in
                     { __br_terms =
                         val40.__br_terms,
                       __br_info =
                         mergeInfo
                           info21
                           (val40.__br_info),
                       forStmts =
                         concat
                           [ val39 ]
                           (val40.forStmts) }
                   else
                     never },
           { nt =
               kleene8,
             label =
               {},
             rhs =
               "",
             action =
               lam state58: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res58.
                   match
                     res58
                   with
                     ""
                   then
                     { __br_terms =
                         "",
                       __br_info =
                         NoInfo
                           {},
                       forStmts =
                         "" }
                   else
                     never },
           { nt =
               #var"StmtTpplAtom",
             label =
               {},
             rhs =
               [ litSym
                   "for",
                 tokSym
                   (LIdentRepr
                      {}),
                 litSym
                   "in",
                 ntSym
                   #var"ExprTppl",
                 litSym
                   "{",
                 ntSym
                   #var"StmtTppl",
                 ntSym
                   kleene8,
                 litSym
                   "}" ],
             action =
               lam state59: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res59.
                   match
                     res59
                   with
                     [ LitParsed l43,
                       TokParsed (LIdentTok x18),
                       LitParsed l44,
                       UserSym (info22, val41),
                       LitParsed l45,
                       UserSym (info23, val42),
                       UserSym val40,
                       LitParsed l46 ]
                   then
                     let val40: {__br_terms: [Info], __br_info: Info, forStmts: [StmtTppl]} =
                       val40
                     in
                     ForLoopStmtTpplOp
                       { __br_terms =
                           join
                             [ [ l43.info ],
                               [ x18.info ],
                               [ l44.info ],
                               [ l45.info ],
                               val40.__br_terms,
                               [ l46.info ] ],
                         __br_info =
                           foldl
                             mergeInfo
                             (l43.info)
                             [ x18.info,
                               l44.info,
                               info22,
                               l45.info,
                               info23,
                               val40.__br_info,
                               l46.info ],
                         forStmts =
                           concat
                             [ val42 ]
                             (val40.forStmts),
                         range =
                           [ val41 ],
                         iterator =
                           [ { v =
                                 nameNoSym
                                   (x18.val),
                               i =
                                 x18.info } ] }
                   else
                     never },
           { nt =
               #var"StmtTpplAtom",
             label =
               {},
             rhs =
               [ litSym
                   "return",
                 ntSym
                   #var"ExprTppl" ],
             action =
               lam state60: {errors: (Ref) ([(Info, [Char])]), content: String}.
                 lam res60.
                   match
                     res60
                   with
                     [ LitParsed l47,
                       UserSym (info24, val43) ]
                   then
                     ReturnStmtTpplOp
                       { __br_terms =
                           [ l47.info ],
                         __br_info =
                           mergeInfo
                             (l47.info)
                             info24,
                         return =
                           [ val43 ] }
                   else
                     never },
           { nt =
               #var"FileTppl",
             label =
               {},
             rhs =
               [ ntSym
                   #var"FileTppl_lclosed" ],
             action =
               lam #var"".
                 lam seq.
                   match
                     seq
                   with
                     [ UserSym cont ]
                   then
                     cont
                       (Some
                          (breakableInitState
                             {}))
                   else
                     never },
           { nt =
               #var"FileTppl_lclosed",
             label =
               {},
             rhs =
               [ ntSym
                   #var"FileTpplAtom",
                 ntSym
                   #var"FileTppl_lopen" ],
             action =
               lam p.
                 lam seq.
                   match
                     seq
                   with
                     [ UserSym x19,
                       UserSym cont ]
                   then
                     lam st.
                       cont
                         (addFileTpplOpAtom
                            p
                            x19
                            st)
                   else
                     never },
           { nt =
               #var"FileTppl_lopen",
             label =
               {},
             rhs =
               [ ntSym
                   #var"FileTpplInfix",
                 ntSym
                   #var"FileTppl_lclosed" ],
             action =
               lam p.
                 lam seq.
                   match
                     seq
                   with
                     [ UserSym x19,
                       UserSym cont ]
                   then
                     lam st.
                       cont
                         (addFileTpplOpInfix
                            p
                            x19
                            st)
                   else
                     never },
           { nt =
               #var"FileTppl_lclosed",
             label =
               {},
             rhs =
               [ ntSym
                   #var"FileTpplPrefix",
                 ntSym
                   #var"FileTppl_lclosed" ],
             action =
               lam p.
                 lam seq.
                   match
                     seq
                   with
                     [ UserSym x19,
                       UserSym cont ]
                   then
                     lam st.
                       cont
                         (addFileTpplOpPrefix
                            p
                            x19
                            st)
                   else
                     never },
           { nt =
               #var"FileTppl_lopen",
             label =
               {},
             rhs =
               [ ntSym
                   #var"FileTpplPostfix",
                 ntSym
                   #var"FileTppl_lopen" ],
             action =
               lam p.
                 lam seq.
                   match
                     seq
                   with
                     [ UserSym x19,
                       UserSym cont ]
                   then
                     lam st.
                       cont
                         (addFileTpplOpPostfix
                            p
                            x19
                            st)
                   else
                     never },
           { nt =
               #var"FileTppl_lopen",
             label =
               {},
             rhs =
               "",
             action =
               lam p.
                 lam #var"".
                   lam st.
                     finalizeFileTpplOp
                       p
                       st },
           { nt =
               #var"DeclTppl",
             label =
               {},
             rhs =
               [ ntSym
                   #var"DeclTppl_lclosed" ],
             action =
               lam #var"".
                 lam seq.
                   match
                     seq
                   with
                     [ UserSym cont ]
                   then
                     cont
                       (Some
                          (breakableInitState
                             {}))
                   else
                     never },
           { nt =
               #var"DeclTppl_lclosed",
             label =
               {},
             rhs =
               [ ntSym
                   #var"DeclTpplAtom",
                 ntSym
                   #var"DeclTppl_lopen" ],
             action =
               lam p.
                 lam seq.
                   match
                     seq
                   with
                     [ UserSym x19,
                       UserSym cont ]
                   then
                     lam st.
                       cont
                         (addDeclTpplOpAtom
                            p
                            x19
                            st)
                   else
                     never },
           { nt =
               #var"DeclTppl_lopen",
             label =
               {},
             rhs =
               [ ntSym
                   #var"DeclTpplInfix",
                 ntSym
                   #var"DeclTppl_lclosed" ],
             action =
               lam p.
                 lam seq.
                   match
                     seq
                   with
                     [ UserSym x19,
                       UserSym cont ]
                   then
                     lam st.
                       cont
                         (addDeclTpplOpInfix
                            p
                            x19
                            st)
                   else
                     never },
           { nt =
               #var"DeclTppl_lclosed",
             label =
               {},
             rhs =
               [ ntSym
                   #var"DeclTpplPrefix",
                 ntSym
                   #var"DeclTppl_lclosed" ],
             action =
               lam p.
                 lam seq.
                   match
                     seq
                   with
                     [ UserSym x19,
                       UserSym cont ]
                   then
                     lam st.
                       cont
                         (addDeclTpplOpPrefix
                            p
                            x19
                            st)
                   else
                     never },
           { nt =
               #var"DeclTppl_lopen",
             label =
               {},
             rhs =
               [ ntSym
                   #var"DeclTpplPostfix",
                 ntSym
                   #var"DeclTppl_lopen" ],
             action =
               lam p.
                 lam seq.
                   match
                     seq
                   with
                     [ UserSym x19,
                       UserSym cont ]
                   then
                     lam st.
                       cont
                         (addDeclTpplOpPostfix
                            p
                            x19
                            st)
                   else
                     never },
           { nt =
               #var"DeclTppl_lopen",
             label =
               {},
             rhs =
               "",
             action =
               lam p.
                 lam #var"".
                   lam st.
                     finalizeDeclTpplOp
                       p
                       st },
           { nt =
               #var"TypeTppl",
             label =
               {},
             rhs =
               [ ntSym
                   #var"TypeTppl_lclosed" ],
             action =
               lam #var"".
                 lam seq.
                   match
                     seq
                   with
                     [ UserSym cont ]
                   then
                     cont
                       (Some
                          (breakableInitState
                             {}))
                   else
                     never },
           { nt =
               #var"TypeTppl_lclosed",
             label =
               {},
             rhs =
               [ ntSym
                   #var"TypeTpplAtom",
                 ntSym
                   #var"TypeTppl_lopen" ],
             action =
               lam p.
                 lam seq.
                   match
                     seq
                   with
                     [ UserSym x19,
                       UserSym cont ]
                   then
                     lam st.
                       cont
                         (addTypeTpplOpAtom
                            p
                            x19
                            st)
                   else
                     never },
           { nt =
               #var"TypeTppl_lopen",
             label =
               {},
             rhs =
               [ ntSym
                   #var"TypeTpplInfix",
                 ntSym
                   #var"TypeTppl_lclosed" ],
             action =
               lam p.
                 lam seq.
                   match
                     seq
                   with
                     [ UserSym x19,
                       UserSym cont ]
                   then
                     lam st.
                       cont
                         (addTypeTpplOpInfix
                            p
                            x19
                            st)
                   else
                     never },
           { nt =
               #var"TypeTppl_lclosed",
             label =
               {},
             rhs =
               [ ntSym
                   #var"TypeTpplPrefix",
                 ntSym
                   #var"TypeTppl_lclosed" ],
             action =
               lam p.
                 lam seq.
                   match
                     seq
                   with
                     [ UserSym x19,
                       UserSym cont ]
                   then
                     lam st.
                       cont
                         (addTypeTpplOpPrefix
                            p
                            x19
                            st)
                   else
                     never },
           { nt =
               #var"TypeTppl_lopen",
             label =
               {},
             rhs =
               [ ntSym
                   #var"TypeTpplPostfix",
                 ntSym
                   #var"TypeTppl_lopen" ],
             action =
               lam p.
                 lam seq.
                   match
                     seq
                   with
                     [ UserSym x19,
                       UserSym cont ]
                   then
                     lam st.
                       cont
                         (addTypeTpplOpPostfix
                            p
                            x19
                            st)
                   else
                     never },
           { nt =
               #var"TypeTppl_lopen",
             label =
               {},
             rhs =
               "",
             action =
               lam p.
                 lam #var"".
                   lam st.
                     finalizeTypeTpplOp
                       p
                       st },
           { nt =
               #var"StmtTppl",
             label =
               {},
             rhs =
               [ ntSym
                   #var"StmtTppl_lclosed" ],
             action =
               lam #var"".
                 lam seq.
                   match
                     seq
                   with
                     [ UserSym cont ]
                   then
                     cont
                       (Some
                          (breakableInitState
                             {}))
                   else
                     never },
           { nt =
               #var"StmtTppl_lclosed",
             label =
               {},
             rhs =
               [ ntSym
                   #var"StmtTpplAtom",
                 ntSym
                   #var"StmtTppl_lopen" ],
             action =
               lam p.
                 lam seq.
                   match
                     seq
                   with
                     [ UserSym x19,
                       UserSym cont ]
                   then
                     lam st.
                       cont
                         (addStmtTpplOpAtom
                            p
                            x19
                            st)
                   else
                     never },
           { nt =
               #var"StmtTppl_lopen",
             label =
               {},
             rhs =
               [ ntSym
                   #var"StmtTpplInfix",
                 ntSym
                   #var"StmtTppl_lclosed" ],
             action =
               lam p.
                 lam seq.
                   match
                     seq
                   with
                     [ UserSym x19,
                       UserSym cont ]
                   then
                     lam st.
                       cont
                         (addStmtTpplOpInfix
                            p
                            x19
                            st)
                   else
                     never },
           { nt =
               #var"StmtTppl_lclosed",
             label =
               {},
             rhs =
               [ ntSym
                   #var"StmtTpplPrefix",
                 ntSym
                   #var"StmtTppl_lclosed" ],
             action =
               lam p.
                 lam seq.
                   match
                     seq
                   with
                     [ UserSym x19,
                       UserSym cont ]
                   then
                     lam st.
                       cont
                         (addStmtTpplOpPrefix
                            p
                            x19
                            st)
                   else
                     never },
           { nt =
               #var"StmtTppl_lopen",
             label =
               {},
             rhs =
               [ ntSym
                   #var"StmtTpplPostfix",
                 ntSym
                   #var"StmtTppl_lopen" ],
             action =
               lam p.
                 lam seq.
                   match
                     seq
                   with
                     [ UserSym x19,
                       UserSym cont ]
                   then
                     lam st.
                       cont
                         (addStmtTpplOpPostfix
                            p
                            x19
                            st)
                   else
                     never },
           { nt =
               #var"StmtTppl_lopen",
             label =
               {},
             rhs =
               "",
             action =
               lam p.
                 lam #var"".
                   lam st.
                     finalizeStmtTpplOp
                       p
                       st },
           { nt =
               #var"ExprTppl",
             label =
               {},
             rhs =
               [ ntSym
                   #var"ExprTppl_lclosed" ],
             action =
               lam #var"".
                 lam seq.
                   match
                     seq
                   with
                     [ UserSym cont ]
                   then
                     cont
                       (Some
                          (breakableInitState
                             {}))
                   else
                     never },
           { nt =
               #var"ExprTppl_lclosed",
             label =
               {},
             rhs =
               [ ntSym
                   #var"ExprTpplAtom",
                 ntSym
                   #var"ExprTppl_lopen" ],
             action =
               lam p.
                 lam seq.
                   match
                     seq
                   with
                     [ UserSym x19,
                       UserSym cont ]
                   then
                     lam st.
                       cont
                         (addExprTpplOpAtom
                            p
                            x19
                            st)
                   else
                     never },
           { nt =
               #var"ExprTppl_lopen",
             label =
               {},
             rhs =
               [ ntSym
                   #var"ExprTpplInfix",
                 ntSym
                   #var"ExprTppl_lclosed" ],
             action =
               lam p.
                 lam seq.
                   match
                     seq
                   with
                     [ UserSym x19,
                       UserSym cont ]
                   then
                     lam st.
                       cont
                         (addExprTpplOpInfix
                            p
                            x19
                            st)
                   else
                     never },
           { nt =
               #var"ExprTppl_lclosed",
             label =
               {},
             rhs =
               [ ntSym
                   #var"ExprTpplPrefix",
                 ntSym
                   #var"ExprTppl_lclosed" ],
             action =
               lam p.
                 lam seq.
                   match
                     seq
                   with
                     [ UserSym x19,
                       UserSym cont ]
                   then
                     lam st.
                       cont
                         (addExprTpplOpPrefix
                            p
                            x19
                            st)
                   else
                     never },
           { nt =
               #var"ExprTppl_lopen",
             label =
               {},
             rhs =
               [ ntSym
                   #var"ExprTpplPostfix",
                 ntSym
                   #var"ExprTppl_lopen" ],
             action =
               lam p.
                 lam seq.
                   match
                     seq
                   with
                     [ UserSym x19,
                       UserSym cont ]
                   then
                     lam st.
                       cont
                         (addExprTpplOpPostfix
                            p
                            x19
                            st)
                   else
                     never },
           { nt =
               #var"ExprTppl_lopen",
             label =
               {},
             rhs =
               "",
             action =
               lam p.
                 lam #var"".
                   lam st.
                     finalizeExprTpplOp
                       p
                       st } ] })
in
match
  target
with
  Right table
then
  table
else
  never

let parseTreePPL
: String -> String -> Either [(Info, String)] FileTppl
= lam filename. lam content.
  use ParseTreePPL in
  let config = {errors = ref [], content = content} in
  let res = parseWithTable _table filename config content in
  let errors = deref config.errors in
  let errors =
    match res with Left err then
      let err = ll1DefaultHighlight content (ll1ToErrorHighlightSpec err) in
      snoc errors err
    else errors in
  if null errors then eitherMapRight (lam x. match x with (_, x) in x) res else Left errors

let parseTreePPLExn
: String -> String -> FileTppl
= lam filename. lam content.
  switch parseTreePPL filename content
  case Left errors then
    for_ errors (lam x. match x with (info, msg) in printLn (infoErrorString info msg));
    exit 1
  case Right file then file
  end