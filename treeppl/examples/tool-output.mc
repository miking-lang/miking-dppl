include "seq.mc"

lang SelfhostBaseAst

  syn File =
  syn Decl =
  syn Type =
  syn Cons =
  syn Stmnt =
  syn Expr =

  sem smapAccumL_File_File : ((a) -> ((File) -> ((a, File)))) -> ((a) -> ((File) -> ((a, File))))
  sem smapAccumL_File_File f acc =
  | x ->
    (acc, x)

  sem smap_File_File : ((File) -> (File)) -> ((File) -> (File))
  sem smap_File_File f =
  | x ->
    (smapAccumL_File_File
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_File_File : ((a) -> ((File) -> (a))) -> ((a) -> ((File) -> (a)))
  sem sfold_File_File f acc =
  | x ->
    (smapAccumL_File_File
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_File_Decl : ((a) -> ((Decl) -> ((a, Decl)))) -> ((a) -> ((File) -> ((a, File))))
  sem smapAccumL_File_Decl f acc =
  | x ->
    (acc, x)

  sem smap_File_Decl : ((Decl) -> (Decl)) -> ((File) -> (File))
  sem smap_File_Decl f =
  | x ->
    (smapAccumL_File_Decl
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_File_Decl : ((a) -> ((Decl) -> (a))) -> ((a) -> ((File) -> (a)))
  sem sfold_File_Decl f acc =
  | x ->
    (smapAccumL_File_Decl
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_File_Type : ((a) -> ((Type) -> ((a, Type)))) -> ((a) -> ((File) -> ((a, File))))
  sem smapAccumL_File_Type f acc =
  | x ->
    (acc, x)

  sem smap_File_Type : ((Type) -> (Type)) -> ((File) -> (File))
  sem smap_File_Type f =
  | x ->
    (smapAccumL_File_Type
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_File_Type : ((a) -> ((Type) -> (a))) -> ((a) -> ((File) -> (a)))
  sem sfold_File_Type f acc =
  | x ->
    (smapAccumL_File_Type
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_File_Cons : ((a) -> ((Cons) -> ((a, Cons)))) -> ((a) -> ((File) -> ((a, File))))
  sem smapAccumL_File_Cons f acc =
  | x ->
    (acc, x)

  sem smap_File_Cons : ((Cons) -> (Cons)) -> ((File) -> (File))
  sem smap_File_Cons f =
  | x ->
    (smapAccumL_File_Cons
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_File_Cons : ((a) -> ((Cons) -> (a))) -> ((a) -> ((File) -> (a)))
  sem sfold_File_Cons f acc =
  | x ->
    (smapAccumL_File_Cons
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_File_Stmnt : ((a) -> ((Stmnt) -> ((a, Stmnt)))) -> ((a) -> ((File) -> ((a, File))))
  sem smapAccumL_File_Stmnt f acc =
  | x ->
    (acc, x)

  sem smap_File_Stmnt : ((Stmnt) -> (Stmnt)) -> ((File) -> (File))
  sem smap_File_Stmnt f =
  | x ->
    (smapAccumL_File_Stmnt
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_File_Stmnt : ((a) -> ((Stmnt) -> (a))) -> ((a) -> ((File) -> (a)))
  sem sfold_File_Stmnt f acc =
  | x ->
    (smapAccumL_File_Stmnt
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_File_Expr : ((a) -> ((Expr) -> ((a, Expr)))) -> ((a) -> ((File) -> ((a, File))))
  sem smapAccumL_File_Expr f acc =
  | x ->
    (acc, x)

  sem smap_File_Expr : ((Expr) -> (Expr)) -> ((File) -> (File))
  sem smap_File_Expr f =
  | x ->
    (smapAccumL_File_Expr
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_File_Expr : ((a) -> ((Expr) -> (a))) -> ((a) -> ((File) -> (a)))
  sem sfold_File_Expr f acc =
  | x ->
    (smapAccumL_File_Expr
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_Decl_File : ((a) -> ((File) -> ((a, File)))) -> ((a) -> ((Decl) -> ((a, Decl))))
  sem smapAccumL_Decl_File f acc =
  | x ->
    (acc, x)

  sem smap_Decl_File : ((File) -> (File)) -> ((Decl) -> (Decl))
  sem smap_Decl_File f =
  | x ->
    (smapAccumL_Decl_File
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_Decl_File : ((a) -> ((File) -> (a))) -> ((a) -> ((Decl) -> (a)))
  sem sfold_Decl_File f acc =
  | x ->
    (smapAccumL_Decl_File
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_Decl_Decl : ((a) -> ((Decl) -> ((a, Decl)))) -> ((a) -> ((Decl) -> ((a, Decl))))
  sem smapAccumL_Decl_Decl f acc =
  | x ->
    (acc, x)

  sem smap_Decl_Decl : ((Decl) -> (Decl)) -> ((Decl) -> (Decl))
  sem smap_Decl_Decl f =
  | x ->
    (smapAccumL_Decl_Decl
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_Decl_Decl : ((a) -> ((Decl) -> (a))) -> ((a) -> ((Decl) -> (a)))
  sem sfold_Decl_Decl f acc =
  | x ->
    (smapAccumL_Decl_Decl
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_Decl_Type : ((a) -> ((Type) -> ((a, Type)))) -> ((a) -> ((Decl) -> ((a, Decl))))
  sem smapAccumL_Decl_Type f acc =
  | x ->
    (acc, x)

  sem smap_Decl_Type : ((Type) -> (Type)) -> ((Decl) -> (Decl))
  sem smap_Decl_Type f =
  | x ->
    (smapAccumL_Decl_Type
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_Decl_Type : ((a) -> ((Type) -> (a))) -> ((a) -> ((Decl) -> (a)))
  sem sfold_Decl_Type f acc =
  | x ->
    (smapAccumL_Decl_Type
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_Decl_Cons : ((a) -> ((Cons) -> ((a, Cons)))) -> ((a) -> ((Decl) -> ((a, Decl))))
  sem smapAccumL_Decl_Cons f acc =
  | x ->
    (acc, x)

  sem smap_Decl_Cons : ((Cons) -> (Cons)) -> ((Decl) -> (Decl))
  sem smap_Decl_Cons f =
  | x ->
    (smapAccumL_Decl_Cons
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_Decl_Cons : ((a) -> ((Cons) -> (a))) -> ((a) -> ((Decl) -> (a)))
  sem sfold_Decl_Cons f acc =
  | x ->
    (smapAccumL_Decl_Cons
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_Decl_Stmnt : ((a) -> ((Stmnt) -> ((a, Stmnt)))) -> ((a) -> ((Decl) -> ((a, Decl))))
  sem smapAccumL_Decl_Stmnt f acc =
  | x ->
    (acc, x)

  sem smap_Decl_Stmnt : ((Stmnt) -> (Stmnt)) -> ((Decl) -> (Decl))
  sem smap_Decl_Stmnt f =
  | x ->
    (smapAccumL_Decl_Stmnt
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_Decl_Stmnt : ((a) -> ((Stmnt) -> (a))) -> ((a) -> ((Decl) -> (a)))
  sem sfold_Decl_Stmnt f acc =
  | x ->
    (smapAccumL_Decl_Stmnt
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_Decl_Expr : ((a) -> ((Expr) -> ((a, Expr)))) -> ((a) -> ((Decl) -> ((a, Decl))))
  sem smapAccumL_Decl_Expr f acc =
  | x ->
    (acc, x)

  sem smap_Decl_Expr : ((Expr) -> (Expr)) -> ((Decl) -> (Decl))
  sem smap_Decl_Expr f =
  | x ->
    (smapAccumL_Decl_Expr
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_Decl_Expr : ((a) -> ((Expr) -> (a))) -> ((a) -> ((Decl) -> (a)))
  sem sfold_Decl_Expr f acc =
  | x ->
    (smapAccumL_Decl_Expr
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_Type_File : ((a) -> ((File) -> ((a, File)))) -> ((a) -> ((Type) -> ((a, Type))))
  sem smapAccumL_Type_File f acc =
  | x ->
    (acc, x)

  sem smap_Type_File : ((File) -> (File)) -> ((Type) -> (Type))
  sem smap_Type_File f =
  | x ->
    (smapAccumL_Type_File
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_Type_File : ((a) -> ((File) -> (a))) -> ((a) -> ((Type) -> (a)))
  sem sfold_Type_File f acc =
  | x ->
    (smapAccumL_Type_File
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_Type_Decl : ((a) -> ((Decl) -> ((a, Decl)))) -> ((a) -> ((Type) -> ((a, Type))))
  sem smapAccumL_Type_Decl f acc =
  | x ->
    (acc, x)

  sem smap_Type_Decl : ((Decl) -> (Decl)) -> ((Type) -> (Type))
  sem smap_Type_Decl f =
  | x ->
    (smapAccumL_Type_Decl
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_Type_Decl : ((a) -> ((Decl) -> (a))) -> ((a) -> ((Type) -> (a)))
  sem sfold_Type_Decl f acc =
  | x ->
    (smapAccumL_Type_Decl
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_Type_Type : ((a) -> ((Type) -> ((a, Type)))) -> ((a) -> ((Type) -> ((a, Type))))
  sem smapAccumL_Type_Type f acc =
  | x ->
    (acc, x)

  sem smap_Type_Type : ((Type) -> (Type)) -> ((Type) -> (Type))
  sem smap_Type_Type f =
  | x ->
    (smapAccumL_Type_Type
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_Type_Type : ((a) -> ((Type) -> (a))) -> ((a) -> ((Type) -> (a)))
  sem sfold_Type_Type f acc =
  | x ->
    (smapAccumL_Type_Type
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_Type_Cons : ((a) -> ((Cons) -> ((a, Cons)))) -> ((a) -> ((Type) -> ((a, Type))))
  sem smapAccumL_Type_Cons f acc =
  | x ->
    (acc, x)

  sem smap_Type_Cons : ((Cons) -> (Cons)) -> ((Type) -> (Type))
  sem smap_Type_Cons f =
  | x ->
    (smapAccumL_Type_Cons
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_Type_Cons : ((a) -> ((Cons) -> (a))) -> ((a) -> ((Type) -> (a)))
  sem sfold_Type_Cons f acc =
  | x ->
    (smapAccumL_Type_Cons
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_Type_Stmnt : ((a) -> ((Stmnt) -> ((a, Stmnt)))) -> ((a) -> ((Type) -> ((a, Type))))
  sem smapAccumL_Type_Stmnt f acc =
  | x ->
    (acc, x)

  sem smap_Type_Stmnt : ((Stmnt) -> (Stmnt)) -> ((Type) -> (Type))
  sem smap_Type_Stmnt f =
  | x ->
    (smapAccumL_Type_Stmnt
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_Type_Stmnt : ((a) -> ((Stmnt) -> (a))) -> ((a) -> ((Type) -> (a)))
  sem sfold_Type_Stmnt f acc =
  | x ->
    (smapAccumL_Type_Stmnt
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_Type_Expr : ((a) -> ((Expr) -> ((a, Expr)))) -> ((a) -> ((Type) -> ((a, Type))))
  sem smapAccumL_Type_Expr f acc =
  | x ->
    (acc, x)

  sem smap_Type_Expr : ((Expr) -> (Expr)) -> ((Type) -> (Type))
  sem smap_Type_Expr f =
  | x ->
    (smapAccumL_Type_Expr
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_Type_Expr : ((a) -> ((Expr) -> (a))) -> ((a) -> ((Type) -> (a)))
  sem sfold_Type_Expr f acc =
  | x ->
    (smapAccumL_Type_Expr
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_Cons_File : ((a) -> ((File) -> ((a, File)))) -> ((a) -> ((Cons) -> ((a, Cons))))
  sem smapAccumL_Cons_File f acc =
  | x ->
    (acc, x)

  sem smap_Cons_File : ((File) -> (File)) -> ((Cons) -> (Cons))
  sem smap_Cons_File f =
  | x ->
    (smapAccumL_Cons_File
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_Cons_File : ((a) -> ((File) -> (a))) -> ((a) -> ((Cons) -> (a)))
  sem sfold_Cons_File f acc =
  | x ->
    (smapAccumL_Cons_File
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_Cons_Decl : ((a) -> ((Decl) -> ((a, Decl)))) -> ((a) -> ((Cons) -> ((a, Cons))))
  sem smapAccumL_Cons_Decl f acc =
  | x ->
    (acc, x)

  sem smap_Cons_Decl : ((Decl) -> (Decl)) -> ((Cons) -> (Cons))
  sem smap_Cons_Decl f =
  | x ->
    (smapAccumL_Cons_Decl
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_Cons_Decl : ((a) -> ((Decl) -> (a))) -> ((a) -> ((Cons) -> (a)))
  sem sfold_Cons_Decl f acc =
  | x ->
    (smapAccumL_Cons_Decl
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_Cons_Type : ((a) -> ((Type) -> ((a, Type)))) -> ((a) -> ((Cons) -> ((a, Cons))))
  sem smapAccumL_Cons_Type f acc =
  | x ->
    (acc, x)

  sem smap_Cons_Type : ((Type) -> (Type)) -> ((Cons) -> (Cons))
  sem smap_Cons_Type f =
  | x ->
    (smapAccumL_Cons_Type
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_Cons_Type : ((a) -> ((Type) -> (a))) -> ((a) -> ((Cons) -> (a)))
  sem sfold_Cons_Type f acc =
  | x ->
    (smapAccumL_Cons_Type
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_Cons_Cons : ((a) -> ((Cons) -> ((a, Cons)))) -> ((a) -> ((Cons) -> ((a, Cons))))
  sem smapAccumL_Cons_Cons f acc =
  | x ->
    (acc, x)

  sem smap_Cons_Cons : ((Cons) -> (Cons)) -> ((Cons) -> (Cons))
  sem smap_Cons_Cons f =
  | x ->
    (smapAccumL_Cons_Cons
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_Cons_Cons : ((a) -> ((Cons) -> (a))) -> ((a) -> ((Cons) -> (a)))
  sem sfold_Cons_Cons f acc =
  | x ->
    (smapAccumL_Cons_Cons
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_Cons_Stmnt : ((a) -> ((Stmnt) -> ((a, Stmnt)))) -> ((a) -> ((Cons) -> ((a, Cons))))
  sem smapAccumL_Cons_Stmnt f acc =
  | x ->
    (acc, x)

  sem smap_Cons_Stmnt : ((Stmnt) -> (Stmnt)) -> ((Cons) -> (Cons))
  sem smap_Cons_Stmnt f =
  | x ->
    (smapAccumL_Cons_Stmnt
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_Cons_Stmnt : ((a) -> ((Stmnt) -> (a))) -> ((a) -> ((Cons) -> (a)))
  sem sfold_Cons_Stmnt f acc =
  | x ->
    (smapAccumL_Cons_Stmnt
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_Cons_Expr : ((a) -> ((Expr) -> ((a, Expr)))) -> ((a) -> ((Cons) -> ((a, Cons))))
  sem smapAccumL_Cons_Expr f acc =
  | x ->
    (acc, x)

  sem smap_Cons_Expr : ((Expr) -> (Expr)) -> ((Cons) -> (Cons))
  sem smap_Cons_Expr f =
  | x ->
    (smapAccumL_Cons_Expr
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_Cons_Expr : ((a) -> ((Expr) -> (a))) -> ((a) -> ((Cons) -> (a)))
  sem sfold_Cons_Expr f acc =
  | x ->
    (smapAccumL_Cons_Expr
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_Stmnt_File : ((a) -> ((File) -> ((a, File)))) -> ((a) -> ((Stmnt) -> ((a, Stmnt))))
  sem smapAccumL_Stmnt_File f acc =
  | x ->
    (acc, x)

  sem smap_Stmnt_File : ((File) -> (File)) -> ((Stmnt) -> (Stmnt))
  sem smap_Stmnt_File f =
  | x ->
    (smapAccumL_Stmnt_File
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_Stmnt_File : ((a) -> ((File) -> (a))) -> ((a) -> ((Stmnt) -> (a)))
  sem sfold_Stmnt_File f acc =
  | x ->
    (smapAccumL_Stmnt_File
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_Stmnt_Decl : ((a) -> ((Decl) -> ((a, Decl)))) -> ((a) -> ((Stmnt) -> ((a, Stmnt))))
  sem smapAccumL_Stmnt_Decl f acc =
  | x ->
    (acc, x)

  sem smap_Stmnt_Decl : ((Decl) -> (Decl)) -> ((Stmnt) -> (Stmnt))
  sem smap_Stmnt_Decl f =
  | x ->
    (smapAccumL_Stmnt_Decl
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_Stmnt_Decl : ((a) -> ((Decl) -> (a))) -> ((a) -> ((Stmnt) -> (a)))
  sem sfold_Stmnt_Decl f acc =
  | x ->
    (smapAccumL_Stmnt_Decl
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_Stmnt_Type : ((a) -> ((Type) -> ((a, Type)))) -> ((a) -> ((Stmnt) -> ((a, Stmnt))))
  sem smapAccumL_Stmnt_Type f acc =
  | x ->
    (acc, x)

  sem smap_Stmnt_Type : ((Type) -> (Type)) -> ((Stmnt) -> (Stmnt))
  sem smap_Stmnt_Type f =
  | x ->
    (smapAccumL_Stmnt_Type
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_Stmnt_Type : ((a) -> ((Type) -> (a))) -> ((a) -> ((Stmnt) -> (a)))
  sem sfold_Stmnt_Type f acc =
  | x ->
    (smapAccumL_Stmnt_Type
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_Stmnt_Cons : ((a) -> ((Cons) -> ((a, Cons)))) -> ((a) -> ((Stmnt) -> ((a, Stmnt))))
  sem smapAccumL_Stmnt_Cons f acc =
  | x ->
    (acc, x)

  sem smap_Stmnt_Cons : ((Cons) -> (Cons)) -> ((Stmnt) -> (Stmnt))
  sem smap_Stmnt_Cons f =
  | x ->
    (smapAccumL_Stmnt_Cons
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_Stmnt_Cons : ((a) -> ((Cons) -> (a))) -> ((a) -> ((Stmnt) -> (a)))
  sem sfold_Stmnt_Cons f acc =
  | x ->
    (smapAccumL_Stmnt_Cons
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_Stmnt_Stmnt : ((a) -> ((Stmnt) -> ((a, Stmnt)))) -> ((a) -> ((Stmnt) -> ((a, Stmnt))))
  sem smapAccumL_Stmnt_Stmnt f acc =
  | x ->
    (acc, x)

  sem smap_Stmnt_Stmnt : ((Stmnt) -> (Stmnt)) -> ((Stmnt) -> (Stmnt))
  sem smap_Stmnt_Stmnt f =
  | x ->
    (smapAccumL_Stmnt_Stmnt
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_Stmnt_Stmnt : ((a) -> ((Stmnt) -> (a))) -> ((a) -> ((Stmnt) -> (a)))
  sem sfold_Stmnt_Stmnt f acc =
  | x ->
    (smapAccumL_Stmnt_Stmnt
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_Stmnt_Expr : ((a) -> ((Expr) -> ((a, Expr)))) -> ((a) -> ((Stmnt) -> ((a, Stmnt))))
  sem smapAccumL_Stmnt_Expr f acc =
  | x ->
    (acc, x)

  sem smap_Stmnt_Expr : ((Expr) -> (Expr)) -> ((Stmnt) -> (Stmnt))
  sem smap_Stmnt_Expr f =
  | x ->
    (smapAccumL_Stmnt_Expr
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_Stmnt_Expr : ((a) -> ((Expr) -> (a))) -> ((a) -> ((Stmnt) -> (a)))
  sem sfold_Stmnt_Expr f acc =
  | x ->
    (smapAccumL_Stmnt_Expr
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_Expr_File : ((a) -> ((File) -> ((a, File)))) -> ((a) -> ((Expr) -> ((a, Expr))))
  sem smapAccumL_Expr_File f acc =
  | x ->
    (acc, x)

  sem smap_Expr_File : ((File) -> (File)) -> ((Expr) -> (Expr))
  sem smap_Expr_File f =
  | x ->
    (smapAccumL_Expr_File
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_Expr_File : ((a) -> ((File) -> (a))) -> ((a) -> ((Expr) -> (a)))
  sem sfold_Expr_File f acc =
  | x ->
    (smapAccumL_Expr_File
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_Expr_Decl : ((a) -> ((Decl) -> ((a, Decl)))) -> ((a) -> ((Expr) -> ((a, Expr))))
  sem smapAccumL_Expr_Decl f acc =
  | x ->
    (acc, x)

  sem smap_Expr_Decl : ((Decl) -> (Decl)) -> ((Expr) -> (Expr))
  sem smap_Expr_Decl f =
  | x ->
    (smapAccumL_Expr_Decl
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_Expr_Decl : ((a) -> ((Decl) -> (a))) -> ((a) -> ((Expr) -> (a)))
  sem sfold_Expr_Decl f acc =
  | x ->
    (smapAccumL_Expr_Decl
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_Expr_Type : ((a) -> ((Type) -> ((a, Type)))) -> ((a) -> ((Expr) -> ((a, Expr))))
  sem smapAccumL_Expr_Type f acc =
  | x ->
    (acc, x)

  sem smap_Expr_Type : ((Type) -> (Type)) -> ((Expr) -> (Expr))
  sem smap_Expr_Type f =
  | x ->
    (smapAccumL_Expr_Type
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_Expr_Type : ((a) -> ((Type) -> (a))) -> ((a) -> ((Expr) -> (a)))
  sem sfold_Expr_Type f acc =
  | x ->
    (smapAccumL_Expr_Type
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_Expr_Cons : ((a) -> ((Cons) -> ((a, Cons)))) -> ((a) -> ((Expr) -> ((a, Expr))))
  sem smapAccumL_Expr_Cons f acc =
  | x ->
    (acc, x)

  sem smap_Expr_Cons : ((Cons) -> (Cons)) -> ((Expr) -> (Expr))
  sem smap_Expr_Cons f =
  | x ->
    (smapAccumL_Expr_Cons
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_Expr_Cons : ((a) -> ((Cons) -> (a))) -> ((a) -> ((Expr) -> (a)))
  sem sfold_Expr_Cons f acc =
  | x ->
    (smapAccumL_Expr_Cons
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_Expr_Stmnt : ((a) -> ((Stmnt) -> ((a, Stmnt)))) -> ((a) -> ((Expr) -> ((a, Expr))))
  sem smapAccumL_Expr_Stmnt f acc =
  | x ->
    (acc, x)

  sem smap_Expr_Stmnt : ((Stmnt) -> (Stmnt)) -> ((Expr) -> (Expr))
  sem smap_Expr_Stmnt f =
  | x ->
    (smapAccumL_Expr_Stmnt
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_Expr_Stmnt : ((a) -> ((Stmnt) -> (a))) -> ((a) -> ((Expr) -> (a)))
  sem sfold_Expr_Stmnt f acc =
  | x ->
    (smapAccumL_Expr_Stmnt
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem smapAccumL_Expr_Expr : ((a) -> ((Expr) -> ((a, Expr)))) -> ((a) -> ((Expr) -> ((a, Expr))))
  sem smapAccumL_Expr_Expr f acc =
  | x ->
    (acc, x)

  sem smap_Expr_Expr : ((Expr) -> (Expr)) -> ((Expr) -> (Expr))
  sem smap_Expr_Expr f =
  | x ->
    (smapAccumL_Expr_Expr
       (lam #var"".
          lam x.
            ({}, f
              x))
       {}
       x).#label"1"

  sem sfold_Expr_Expr : ((a) -> ((Expr) -> (a))) -> ((a) -> ((Expr) -> (a)))
  sem sfold_Expr_Expr f acc =
  | x ->
    (smapAccumL_Expr_Expr
       (lam acc.
          lam x.
            (f
              acc
              x, x))
       acc
       x).#label"0"

  sem get_File_info : (File) -> (Info)

  sem set_File_info : (Info) -> ((File) -> (File))

  sem mapAccum_File_info : ((a) -> ((Info) -> ((a, Info)))) -> ((a) -> ((File) -> ((a, File))))
  sem mapAccum_File_info f acc =
  | target ->
    match
      f
        acc
        (get_File_info
           target)
    with
      (acc, val)
    then
      (acc, set_File_info
        val
        target)
    else
      never

  sem map_File_info : ((Info) -> (Info)) -> ((File) -> (File))
  sem map_File_info f =
  | target ->
    set_File_info
      (f
         (get_File_info
            target))
      target

  sem get_Decl_info : (Decl) -> (Info)

  sem set_Decl_info : (Info) -> ((Decl) -> (Decl))

  sem mapAccum_Decl_info : ((a) -> ((Info) -> ((a, Info)))) -> ((a) -> ((Decl) -> ((a, Decl))))
  sem mapAccum_Decl_info f acc =
  | target ->
    match
      f
        acc
        (get_Decl_info
           target)
    with
      (acc, val)
    then
      (acc, set_Decl_info
        val
        target)
    else
      never

  sem map_Decl_info : ((Info) -> (Info)) -> ((Decl) -> (Decl))
  sem map_Decl_info f =
  | target ->
    set_Decl_info
      (f
         (get_Decl_info
            target))
      target

  sem get_Type_info : (Type) -> (Info)

  sem set_Type_info : (Info) -> ((Type) -> (Type))

  sem mapAccum_Type_info : ((a) -> ((Info) -> ((a, Info)))) -> ((a) -> ((Type) -> ((a, Type))))
  sem mapAccum_Type_info f acc =
  | target ->
    match
      f
        acc
        (get_Type_info
           target)
    with
      (acc, val)
    then
      (acc, set_Type_info
        val
        target)
    else
      never

  sem map_Type_info : ((Info) -> (Info)) -> ((Type) -> (Type))
  sem map_Type_info f =
  | target ->
    set_Type_info
      (f
         (get_Type_info
            target))
      target

  sem get_Cons_info : (Cons) -> (Info)

  sem set_Cons_info : (Info) -> ((Cons) -> (Cons))

  sem mapAccum_Cons_info : ((a) -> ((Info) -> ((a, Info)))) -> ((a) -> ((Cons) -> ((a, Cons))))
  sem mapAccum_Cons_info f acc =
  | target ->
    match
      f
        acc
        (get_Cons_info
           target)
    with
      (acc, val)
    then
      (acc, set_Cons_info
        val
        target)
    else
      never

  sem map_Cons_info : ((Info) -> (Info)) -> ((Cons) -> (Cons))
  sem map_Cons_info f =
  | target ->
    set_Cons_info
      (f
         (get_Cons_info
            target))
      target

  sem get_Stmnt_info : (Stmnt) -> (Info)

  sem set_Stmnt_info : (Info) -> ((Stmnt) -> (Stmnt))

  sem mapAccum_Stmnt_info : ((a) -> ((Info) -> ((a, Info)))) -> ((a) -> ((Stmnt) -> ((a, Stmnt))))
  sem mapAccum_Stmnt_info f acc =
  | target ->
    match
      f
        acc
        (get_Stmnt_info
           target)
    with
      (acc, val)
    then
      (acc, set_Stmnt_info
        val
        target)
    else
      never

  sem map_Stmnt_info : ((Info) -> (Info)) -> ((Stmnt) -> (Stmnt))
  sem map_Stmnt_info f =
  | target ->
    set_Stmnt_info
      (f
         (get_Stmnt_info
            target))
      target

  sem get_Expr_info : (Expr) -> (Info)

  sem set_Expr_info : (Info) -> ((Expr) -> (Expr))

  sem mapAccum_Expr_info : ((a) -> ((Info) -> ((a, Info)))) -> ((a) -> ((Expr) -> ((a, Expr))))
  sem mapAccum_Expr_info f acc =
  | target ->
    match
      f
        acc
        (get_Expr_info
           target)
    with
      (acc, val)
    then
      (acc, set_Expr_info
        val
        target)
    else
      never

  sem map_Expr_info : ((Info) -> (Info)) -> ((Expr) -> (Expr))
  sem map_Expr_info f =
  | target ->
    set_Expr_info
      (f
         (get_Expr_info
            target))
      target

end

lang FileAst = SelfhostBaseAst
  type FileRecord = {decl: [Decl], info: Info}

  syn File =
  | File FileRecord

  sem smapAccumL_File_Decl f acc =
  | File x ->
    match
      match
        let decl =
          x.decl
        in
        mapAccumL
          (lam acc1.
             lam x1.
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
      (acc, File
        x)
    else
      never

  sem get_File_info =
  | File target ->
    target.info

  sem set_File_info val =
  | File target ->
    { target
      with
      info =
        val }

end

lang FunctionDeclAst = SelfhostBaseAst
  type FunctionDeclRecord = {name: {v: LName, i: Info}, info: Info, model: (Option) (Info), args: [{ty: Type, name: {v: LName, i: Info}}], returnTy: (Option) (Type), body: [Stmnt]}

  syn Decl =
  | FunctionDecl FunctionDeclRecord

  sem smapAccumL_Decl_Type f acc =
  | FunctionDecl x ->
    match
      match
        let args =
          x.args
        in
        mapAccumL
          (lam acc1.
             lam x1.
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
      (acc, FunctionDecl
        x)
    else
      never

  sem smapAccumL_Decl_Stmnt f acc =
  | FunctionDecl x ->
    match
      match
        let body =
          x.body
        in
        mapAccumL
          (lam acc1.
             lam x1.
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
      (acc, FunctionDecl
        x)
    else
      never

  sem get_Decl_info =
  | FunctionDecl target ->
    target.info

  sem set_Decl_info val =
  | FunctionDecl target ->
    { target
      with
      info =
        val }

end

lang ConstructorConsAst = SelfhostBaseAst
  type ConstructorConsRecord = {name: {v: UName, i: Info}, info: Info}

  syn Cons =
  | ConstructorCons ConstructorConsRecord

  sem get_Cons_info =
  | ConstructorCons target ->
    target.info

  sem set_Cons_info val =
  | ConstructorCons target ->
    { target
      with
      info =
        val }

end

lang TypeDeclarationDeclAst = SelfhostBaseAst
  type TypeDeclarationDeclRecord = {name: {v: UName, i: Info}, info: Info, cons: [Cons]}

  syn Decl =
  | TypeDeclarationDecl TypeDeclarationDeclRecord

  sem smapAccumL_Decl_Cons f acc =
  | TypeDeclarationDecl x ->
    match
      match
        let cons =
          x.cons
        in
        mapAccumL
          (lam acc1.
             lam x1.
               f
                 acc1
                 x1)
          acc
          cons
      with
        (acc, cons)
      then
        (acc, { x
          with
          cons =
            cons })
      else
        never
    with
      (acc, x)
    then
      (acc, TypeDeclarationDecl
        x)
    else
      never

  sem get_Decl_info =
  | TypeDeclarationDecl target ->
    target.info

  sem set_Decl_info val =
  | TypeDeclarationDecl target ->
    { target
      with
      info =
        val }

end

lang TypeConstructorTypeAst = SelfhostBaseAst
  type TypeConstructorTypeRecord = {name: {v: UName, i: Info}, info: Info}

  syn Type =
  | TypeConstructorType TypeConstructorTypeRecord

  sem get_Type_info =
  | TypeConstructorType target ->
    target.info

  sem set_Type_info val =
  | TypeConstructorType target ->
    { target
      with
      info =
        val }

end

lang IntegerExprAst = SelfhostBaseAst
  type IntegerExprRecord = {info: Info, val: {v: Integer, i: Info}}

  syn Expr =
  | IntegerExpr IntegerExprRecord

  sem get_Expr_info =
  | IntegerExpr target ->
    target.info

  sem set_Expr_info val =
  | IntegerExpr target ->
    { target
      with
      info =
        val }

end

lang StringExprAst = SelfhostBaseAst
  type StringExprRecord = {info: Info, val: {v: String, i: Info}}

  syn Expr =
  | StringExpr StringExprRecord

  sem get_Expr_info =
  | StringExpr target ->
    target.info

  sem set_Expr_info val =
  | StringExpr target ->
    { target
      with
      info =
        val }

end

lang RealExprAst = SelfhostBaseAst
  type RealExprRecord = {info: Info, val: {v: Real, i: Info}}

  syn Expr =
  | RealExpr RealExprRecord

  sem get_Expr_info =
  | RealExpr target ->
    target.info

  sem set_Expr_info val =
  | RealExpr target ->
    { target
      with
      info =
        val }

end

lang VariableExprAst = SelfhostBaseAst
  type VariableExprRecord = {info: Info, ident: {v: LName, i: Info}}

  syn Expr =
  | VariableExpr VariableExprRecord

  sem get_Expr_info =
  | VariableExpr target ->
    target.info

  sem set_Expr_info val =
  | VariableExpr target ->
    { target
      with
      info =
        val }

end

lang AdditionExprAst = SelfhostBaseAst
  type AdditionExprRecord = {info: Info, left: Expr, right: Expr}

  syn Expr =
  | AdditionExpr AdditionExprRecord

  sem smapAccumL_Expr_Expr f acc =
  | AdditionExpr x ->
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
      (acc, AdditionExpr
        x)
    else
      never

  sem get_Expr_info =
  | AdditionExpr target ->
    target.info

  sem set_Expr_info val =
  | AdditionExpr target ->
    { target
      with
      info =
        val }

end

lang MultiplicationExprAst = SelfhostBaseAst
  type MultiplicationExprRecord = {info: Info, left: Expr, right: Expr}

  syn Expr =
  | MultiplicationExpr MultiplicationExprRecord

  sem smapAccumL_Expr_Expr f acc =
  | MultiplicationExpr x ->
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
      (acc, MultiplicationExpr
        x)
    else
      never

  sem get_Expr_info =
  | MultiplicationExpr target ->
    target.info

  sem set_Expr_info val =
  | MultiplicationExpr target ->
    { target
      with
      info =
        val }

end

lang SubtractionExprAst = SelfhostBaseAst
  type SubtractionExprRecord = {info: Info, left: Expr, right: Expr}

  syn Expr =
  | SubtractionExpr SubtractionExprRecord

  sem smapAccumL_Expr_Expr f acc =
  | SubtractionExpr x ->
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
      (acc, SubtractionExpr
        x)
    else
      never

  sem get_Expr_info =
  | SubtractionExpr target ->
    target.info

  sem set_Expr_info val =
  | SubtractionExpr target ->
    { target
      with
      info =
        val }

end

lang DivisionExprAst = SelfhostBaseAst
  type DivisionExprRecord = {info: Info, left: Expr, right: Expr}

  syn Expr =
  | DivisionExpr DivisionExprRecord

  sem smapAccumL_Expr_Expr f acc =
  | DivisionExpr x ->
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
      (acc, DivisionExpr
        x)
    else
      never

  sem get_Expr_info =
  | DivisionExpr target ->
    target.info

  sem set_Expr_info val =
  | DivisionExpr target ->
    { target
      with
      info =
        val }

end

lang FunctionCallExprAst = SelfhostBaseAst
  type FunctionCallExprRecord = {info: Info, args: [Expr], f: Expr}

  syn Expr =
  | FunctionCallExpr FunctionCallExprRecord

  sem smapAccumL_Expr_Expr f acc =
  | FunctionCallExpr x ->
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
               lam x1.
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
      (acc, FunctionCallExpr
        x)
    else
      never

  sem get_Expr_info =
  | FunctionCallExpr target ->
    target.info

  sem set_Expr_info val =
  | FunctionCallExpr target ->
    { target
      with
      info =
        val }

end

lang ListExprAst = SelfhostBaseAst
  type ListExprRecord = {info: Info, value: [{v: LName, i: Info}], key: [{v: LName, i: Info}]}

  syn Expr =
  | ListExpr ListExprRecord

  sem get_Expr_info =
  | ListExpr target ->
    target.info

  sem set_Expr_info val =
  | ListExpr target ->
    { target
      with
      info =
        val }

end

lang AssumeStmntAst = SelfhostBaseAst
  type AssumeStmntRecord = {info: Info, args: [Expr], randomVar: {v: LName, i: Info}, distribution: {v: UName, i: Info}}

  syn Stmnt =
  | AssumeStmnt AssumeStmntRecord

  sem smapAccumL_Stmnt_Expr f acc =
  | AssumeStmnt x ->
    match
      match
        let args =
          x.args
        in
        mapAccumL
          (lam acc1.
             lam x1.
               f
                 acc1
                 x1)
          acc
          args
      with
        (acc, args)
      then
        (acc, { x
          with
          args =
            args })
      else
        never
    with
      (acc, x)
    then
      (acc, AssumeStmnt
        x)
    else
      never

  sem get_Stmnt_info =
  | AssumeStmnt target ->
    target.info

  sem set_Stmnt_info val =
  | AssumeStmnt target ->
    { target
      with
      info =
        val }

end

lang ObserveStmntAst = SelfhostBaseAst
  type ObserveStmntRecord = {info: Info, args: [Expr], value: Expr, distribution: {v: UName, i: Info}}

  syn Stmnt =
  | ObserveStmnt ObserveStmntRecord

  sem smapAccumL_Stmnt_Expr f acc =
  | ObserveStmnt x ->
    match
      match
        let args =
          x.args
        in
        mapAccumL
          (lam acc1.
             lam x1.
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
      (acc, ObserveStmnt
        x)
    else
      never

  sem get_Stmnt_info =
  | ObserveStmnt target ->
    target.info

  sem set_Stmnt_info val =
  | ObserveStmnt target ->
    { target
      with
      info =
        val }

end

lang ForLoopStmntAst = SelfhostBaseAst
  type ForLoopStmntRecord = {info: Info, some: [Stmnt], end: Expr, begin: Expr}

  syn Stmnt =
  | ForLoopStmnt ForLoopStmntRecord

  sem smapAccumL_Stmnt_Stmnt f acc =
  | ForLoopStmnt x ->
    match
      match
        let some =
          x.some
        in
        mapAccumL
          (lam acc1.
             lam x1.
               f
                 acc1
                 x1)
          acc
          some
      with
        (acc, some)
      then
        (acc, { x
          with
          some =
            some })
      else
        never
    with
      (acc, x)
    then
      (acc, ForLoopStmnt
        x)
    else
      never

  sem smapAccumL_Stmnt_Expr f acc =
  | ForLoopStmnt x ->
    match
      match
        let end =
          x.end
        in
        f
          acc
          end
      with
        (acc, end)
      then
        match
          let begin =
            x.begin
          in
          f
            acc
            begin
        with
          (acc, begin)
        then
          (acc, { { x
              with
              end =
                end }
            with
            begin =
              begin })
        else
          never
      else
        never
    with
      (acc, x)
    then
      (acc, ForLoopStmnt
        x)
    else
      never

  sem get_Stmnt_info =
  | ForLoopStmnt target ->
    target.info

  sem set_Stmnt_info val =
  | ForLoopStmnt target ->
    { target
      with
      info =
        val }

end

lang IfStmntAst = SelfhostBaseAst
  type IfStmntRecord = {info: Info, some: [Stmnt], condition: Expr}

  syn Stmnt =
  | IfStmnt IfStmntRecord

  sem smapAccumL_Stmnt_Stmnt f acc =
  | IfStmnt x ->
    match
      match
        let some =
          x.some
        in
        mapAccumL
          (lam acc1.
             lam x1.
               f
                 acc1
                 x1)
          acc
          some
      with
        (acc, some)
      then
        (acc, { x
          with
          some =
            some })
      else
        never
    with
      (acc, x)
    then
      (acc, IfStmnt
        x)
    else
      never

  sem smapAccumL_Stmnt_Expr f acc =
  | IfStmnt x ->
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
      (acc, IfStmnt
        x)
    else
      never

  sem get_Stmnt_info =
  | IfStmnt target ->
    target.info

  sem set_Stmnt_info val =
  | IfStmnt target ->
    { target
      with
      info =
        val }

end

lang ReturnStatementStmntAst = SelfhostBaseAst
  type ReturnStatementStmntRecord = {info: Info, some: Expr}

  syn Stmnt =
  | ReturnStatementStmnt ReturnStatementStmntRecord

  sem smapAccumL_Stmnt_Expr f acc =
  | ReturnStatementStmnt x ->
    match
      match
        let some =
          x.some
        in
        f
          acc
          some
      with
        (acc, some)
      then
        (acc, { x
          with
          some =
            some })
      else
        never
    with
      (acc, x)
    then
      (acc, ReturnStatementStmnt
        x)
    else
      never

  sem get_Stmnt_info =
  | ReturnStatementStmnt target ->
    target.info

  sem set_Stmnt_info val =
  | ReturnStatementStmnt target ->
    { target
      with
      info =
        val }

end

lang SelfhostAst = FileAst + FunctionDeclAst + ConstructorConsAst + TypeDeclarationDeclAst + TypeConstructorTypeAst + IntegerExprAst + StringExprAst + RealExprAst + VariableExprAst + AdditionExprAst + MultiplicationExprAst + SubtractionExprAst + DivisionExprAst + FunctionCallExprAst + ListExprAst + AssumeStmntAst + ObserveStmntAst + ForLoopStmntAst + IfStmntAst + ReturnStatementStmntAst



end

lang FileOpBase = SelfhostBaseAst

  syn FileOp =

  sem topAllowed_FileOp : (FileOp) -> (Bool)
  sem topAllowed_FileOp =
  | _ ->
    true

  sem leftAllowed_FileOp : ({parent: FileOp, child: FileOp}) -> (Bool)
  sem leftAllowed_FileOp =
  | _ ->
    true

  sem rightAllowed_FileOp : ({parent: FileOp, child: FileOp}) -> (Bool)
  sem rightAllowed_FileOp =
  | _ ->
    true

  sem groupingsAllowed_FileOp : (FileOp) -> (AllowedDirection)
  sem groupingsAllowed_FileOp =
  | _ ->
    GEither
      {}

  sem parenAllowed_FileOp : (FileOp) -> (AllowedDirection)
  sem parenAllowed_FileOp =
  | _ ->
    GEither
      {}

  sem get_FileOp_info : (FileOp) -> (Info)

  sem get_FileOp_terms : (FileOp) -> ([Info])

  sem unsplit_FileOp : ((PermanentNode) (FileOp)) -> ((Info, File))

end

lang DeclOpBase = SelfhostBaseAst

  syn DeclOp =

  sem topAllowed_DeclOp : (DeclOp) -> (Bool)
  sem topAllowed_DeclOp =
  | _ ->
    true

  sem leftAllowed_DeclOp : ({parent: DeclOp, child: DeclOp}) -> (Bool)
  sem leftAllowed_DeclOp =
  | _ ->
    true

  sem rightAllowed_DeclOp : ({parent: DeclOp, child: DeclOp}) -> (Bool)
  sem rightAllowed_DeclOp =
  | _ ->
    true

  sem groupingsAllowed_DeclOp : (DeclOp) -> (AllowedDirection)
  sem groupingsAllowed_DeclOp =
  | _ ->
    GEither
      {}

  sem parenAllowed_DeclOp : (DeclOp) -> (AllowedDirection)
  sem parenAllowed_DeclOp =
  | _ ->
    GEither
      {}

  sem get_DeclOp_info : (DeclOp) -> (Info)

  sem get_DeclOp_terms : (DeclOp) -> ([Info])

  sem unsplit_DeclOp : ((PermanentNode) (DeclOp)) -> ((Info, Decl))

end

lang TypeOpBase = SelfhostBaseAst

  syn TypeOp =

  sem topAllowed_TypeOp : (TypeOp) -> (Bool)
  sem topAllowed_TypeOp =
  | _ ->
    true

  sem leftAllowed_TypeOp : ({parent: TypeOp, child: TypeOp}) -> (Bool)
  sem leftAllowed_TypeOp =
  | _ ->
    true

  sem rightAllowed_TypeOp : ({parent: TypeOp, child: TypeOp}) -> (Bool)
  sem rightAllowed_TypeOp =
  | _ ->
    true

  sem groupingsAllowed_TypeOp : (TypeOp) -> (AllowedDirection)
  sem groupingsAllowed_TypeOp =
  | _ ->
    GEither
      {}

  sem parenAllowed_TypeOp : (TypeOp) -> (AllowedDirection)
  sem parenAllowed_TypeOp =
  | _ ->
    GEither
      {}

  sem get_TypeOp_info : (TypeOp) -> (Info)

  sem get_TypeOp_terms : (TypeOp) -> ([Info])

  sem unsplit_TypeOp : ((PermanentNode) (TypeOp)) -> ((Info, Type))

end

lang ConsOpBase = SelfhostBaseAst

  syn ConsOp =

  sem topAllowed_ConsOp : (ConsOp) -> (Bool)
  sem topAllowed_ConsOp =
  | _ ->
    true

  sem leftAllowed_ConsOp : ({parent: ConsOp, child: ConsOp}) -> (Bool)
  sem leftAllowed_ConsOp =
  | _ ->
    true

  sem rightAllowed_ConsOp : ({parent: ConsOp, child: ConsOp}) -> (Bool)
  sem rightAllowed_ConsOp =
  | _ ->
    true

  sem groupingsAllowed_ConsOp : (ConsOp) -> (AllowedDirection)
  sem groupingsAllowed_ConsOp =
  | _ ->
    GEither
      {}

  sem parenAllowed_ConsOp : (ConsOp) -> (AllowedDirection)
  sem parenAllowed_ConsOp =
  | _ ->
    GEither
      {}

  sem get_ConsOp_info : (ConsOp) -> (Info)

  sem get_ConsOp_terms : (ConsOp) -> ([Info])

  sem unsplit_ConsOp : ((PermanentNode) (ConsOp)) -> ((Info, Cons))

end

lang StmntOpBase = SelfhostBaseAst

  syn StmntOp =

  sem topAllowed_StmntOp : (StmntOp) -> (Bool)
  sem topAllowed_StmntOp =
  | _ ->
    true

  sem leftAllowed_StmntOp : ({parent: StmntOp, child: StmntOp}) -> (Bool)
  sem leftAllowed_StmntOp =
  | _ ->
    true

  sem rightAllowed_StmntOp : ({parent: StmntOp, child: StmntOp}) -> (Bool)
  sem rightAllowed_StmntOp =
  | _ ->
    true

  sem groupingsAllowed_StmntOp : (StmntOp) -> (AllowedDirection)
  sem groupingsAllowed_StmntOp =
  | _ ->
    GEither
      {}

  sem parenAllowed_StmntOp : (StmntOp) -> (AllowedDirection)
  sem parenAllowed_StmntOp =
  | _ ->
    GEither
      {}

  sem get_StmntOp_info : (StmntOp) -> (Info)

  sem get_StmntOp_terms : (StmntOp) -> ([Info])

  sem unsplit_StmntOp : ((PermanentNode) (StmntOp)) -> ((Info, Stmnt))

end

lang ExprOpBase = SelfhostBaseAst

  syn ExprOp =

  sem topAllowed_ExprOp : (ExprOp) -> (Bool)
  sem topAllowed_ExprOp =
  | _ ->
    true

  sem leftAllowed_ExprOp : ({parent: ExprOp, child: ExprOp}) -> (Bool)
  sem leftAllowed_ExprOp =
  | _ ->
    true

  sem rightAllowed_ExprOp : ({parent: ExprOp, child: ExprOp}) -> (Bool)
  sem rightAllowed_ExprOp =
  | _ ->
    true

  sem groupingsAllowed_ExprOp : (ExprOp) -> (AllowedDirection)
  sem groupingsAllowed_ExprOp =
  | _ ->
    GEither
      {}

  sem parenAllowed_ExprOp : (ExprOp) -> (AllowedDirection)
  sem parenAllowed_ExprOp =
  | _ ->
    GEither
      {}

  sem get_ExprOp_info : (ExprOp) -> (Info)

  sem get_ExprOp_terms : (ExprOp) -> ([Info])

  sem unsplit_ExprOp : ((PermanentNode) (ExprOp)) -> ((Info, Expr))

end

lang FileOp = FileOpBase + FileAst

  syn FileOp =
  | FileOp {__br_terms: [Info], decl: [Decl], __br_info: Info}

  sem get_FileOp_info =
  | FileOp x ->
    x.__br_info

  sem get_FileOp_terms =
  | FileOp x ->
    x.__br_terms

  sem unsplit_FileOp =
  | AtomP {self = FileOp x} ->
    (x.__br_info, File
      { decl =
          x.decl,
        info =
          x.__br_info })

end

lang FunctionDeclOp = DeclOpBase + FunctionDeclAst

  syn DeclOp =
  | FunctionDeclOp {__br_terms: [Info], __br_info: Info, name: [{v: LName, i: Info}], model: [Info], args: [{ty: Type, name: {v: LName, i: Info}}], returnTy: [Type], body: [Stmnt]}

  sem get_DeclOp_info =
  | FunctionDeclOp x ->
    x.__br_info

  sem get_DeclOp_terms =
  | FunctionDeclOp x ->
    x.__br_terms

  sem unsplit_DeclOp =
  | AtomP {self = FunctionDeclOp x} ->
    (x.__br_info, FunctionDecl
      { name =
          match
            x.name
          with
            [ x1 ] ++ _ ++ ""
          then
            x1
          else
            never,
        info =
          x.__br_info,
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

lang ConstructorConsOp = ConsOpBase + ConstructorConsAst

  syn ConsOp =
  | ConstructorConsOp {__br_terms: [Info], __br_info: Info, name: [{v: UName, i: Info}]}

  sem get_ConsOp_info =
  | ConstructorConsOp x ->
    x.__br_info

  sem get_ConsOp_terms =
  | ConstructorConsOp x ->
    x.__br_terms

  sem unsplit_ConsOp =
  | AtomP {self = ConstructorConsOp x} ->
    (x.__br_info, ConstructorCons
      { name =
          match
            x.name
          with
            [ x1 ] ++ _ ++ ""
          then
            x1
          else
            never,
        info =
          x.__br_info })

end

lang TypeDeclarationDeclOp = DeclOpBase + TypeDeclarationDeclAst

  syn DeclOp =
  | TypeDeclarationDeclOp {__br_terms: [Info], __br_info: Info, name: [{v: UName, i: Info}], cons: [Cons]}

  sem get_DeclOp_info =
  | TypeDeclarationDeclOp x ->
    x.__br_info

  sem get_DeclOp_terms =
  | TypeDeclarationDeclOp x ->
    x.__br_terms

  sem unsplit_DeclOp =
  | AtomP {self = TypeDeclarationDeclOp x} ->
    (x.__br_info, TypeDeclarationDecl
      { name =
          match
            x.name
          with
            [ x1 ] ++ _ ++ ""
          then
            x1
          else
            never,
        info =
          x.__br_info,
        cons =
          x.cons })

end

lang TypeConstructorTypeOp = TypeOpBase + TypeConstructorTypeAst

  syn TypeOp =
  | TypeConstructorTypeOp {__br_terms: [Info], __br_info: Info, name: [{v: UName, i: Info}]}

  sem get_TypeOp_info =
  | TypeConstructorTypeOp x ->
    x.__br_info

  sem get_TypeOp_terms =
  | TypeConstructorTypeOp x ->
    x.__br_terms

  sem unsplit_TypeOp =
  | AtomP {self = TypeConstructorTypeOp x} ->
    (x.__br_info, TypeConstructorType
      { name =
          match
            x.name
          with
            [ x1 ] ++ _ ++ ""
          then
            x1
          else
            never,
        info =
          x.__br_info })

end

lang IntegerExprOp = ExprOpBase + IntegerExprAst

  syn ExprOp =
  | IntegerExprOp {__br_terms: [Info], __br_info: Info, val: [{v: Integer, i: Info}]}

  sem get_ExprOp_info =
  | IntegerExprOp x ->
    x.__br_info

  sem get_ExprOp_terms =
  | IntegerExprOp x ->
    x.__br_terms

  sem unsplit_ExprOp =
  | AtomP {self = IntegerExprOp x} ->
    (x.__br_info, IntegerExpr
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

lang StringExprOp = ExprOpBase + StringExprAst

  syn ExprOp =
  | StringExprOp {__br_terms: [Info], __br_info: Info, val: [{v: String, i: Info}]}

  sem get_ExprOp_info =
  | StringExprOp x ->
    x.__br_info

  sem get_ExprOp_terms =
  | StringExprOp x ->
    x.__br_terms

  sem unsplit_ExprOp =
  | AtomP {self = StringExprOp x} ->
    (x.__br_info, StringExpr
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

lang RealExprOp = ExprOpBase + RealExprAst

  syn ExprOp =
  | RealExprOp {__br_terms: [Info], __br_info: Info, val: [{v: Real, i: Info}]}

  sem get_ExprOp_info =
  | RealExprOp x ->
    x.__br_info

  sem get_ExprOp_terms =
  | RealExprOp x ->
    x.__br_terms

  sem unsplit_ExprOp =
  | AtomP {self = RealExprOp x} ->
    (x.__br_info, RealExpr
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

lang VariableExprOp = ExprOpBase + VariableExprAst

  syn ExprOp =
  | VariableExprOp {__br_terms: [Info], __br_info: Info, ident: [{v: LName, i: Info}]}

  sem get_ExprOp_info =
  | VariableExprOp x ->
    x.__br_info

  sem get_ExprOp_terms =
  | VariableExprOp x ->
    x.__br_terms

  sem unsplit_ExprOp =
  | AtomP {self = VariableExprOp x} ->
    (x.__br_info, VariableExpr
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

lang AdditionExprOp = ExprOpBase + AdditionExprAst

  syn ExprOp =
  | AdditionExprOp {__br_terms: [Info], __br_info: Info}

  sem get_ExprOp_info =
  | AdditionExprOp x ->
    x.__br_info

  sem get_ExprOp_terms =
  | AdditionExprOp x ->
    x.__br_terms

  sem unsplit_ExprOp =
  | InfixP {self = AdditionExprOp x, leftChildAlts = [ l ] ++ _ ++ "", rightChildAlts = [ r ] ++ _ ++ ""} ->
    match
      (unsplit_ExprOp
        l, unsplit_ExprOp
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
      (info, AdditionExpr
        { info =
            info })
    else
      never

end

lang MultiplicationExprOp = ExprOpBase + MultiplicationExprAst

  syn ExprOp =
  | MultiplicationExprOp {__br_terms: [Info], __br_info: Info}

  sem get_ExprOp_info =
  | MultiplicationExprOp x ->
    x.__br_info

  sem get_ExprOp_terms =
  | MultiplicationExprOp x ->
    x.__br_terms

  sem unsplit_ExprOp =
  | InfixP {self = MultiplicationExprOp x, leftChildAlts = [ l ] ++ _ ++ "", rightChildAlts = [ r ] ++ _ ++ ""} ->
    match
      (unsplit_ExprOp
        l, unsplit_ExprOp
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
      (info, MultiplicationExpr
        { info =
            info })
    else
      never

end

lang SubtractionExprOp = ExprOpBase + SubtractionExprAst

  syn ExprOp =
  | SubtractionExprOp {__br_terms: [Info], __br_info: Info}

  sem get_ExprOp_info =
  | SubtractionExprOp x ->
    x.__br_info

  sem get_ExprOp_terms =
  | SubtractionExprOp x ->
    x.__br_terms

  sem unsplit_ExprOp =
  | InfixP {self = SubtractionExprOp x, leftChildAlts = [ l ] ++ _ ++ "", rightChildAlts = [ r ] ++ _ ++ ""} ->
    match
      (unsplit_ExprOp
        l, unsplit_ExprOp
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
      (info, SubtractionExpr
        { info =
            info })
    else
      never

end

lang DivisionExprOp = ExprOpBase + DivisionExprAst

  syn ExprOp =
  | DivisionExprOp {__br_terms: [Info], __br_info: Info}

  sem get_ExprOp_info =
  | DivisionExprOp x ->
    x.__br_info

  sem get_ExprOp_terms =
  | DivisionExprOp x ->
    x.__br_terms

  sem unsplit_ExprOp =
  | InfixP {self = DivisionExprOp x, leftChildAlts = [ l ] ++ _ ++ "", rightChildAlts = [ r ] ++ _ ++ ""} ->
    match
      (unsplit_ExprOp
        l, unsplit_ExprOp
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
      (info, DivisionExpr
        { info =
            info })
    else
      never

end

lang FunctionCallExprOp = ExprOpBase + FunctionCallExprAst

  syn ExprOp =
  | FunctionCallExprOp {__br_terms: [Info], __br_info: Info, args: [Expr]}

  sem get_ExprOp_info =
  | FunctionCallExprOp x ->
    x.__br_info

  sem get_ExprOp_terms =
  | FunctionCallExprOp x ->
    x.__br_terms

  sem unsplit_ExprOp =
  | PostfixP {self = FunctionCallExprOp x, leftChildAlts = [ l ] ++ _ ++ ""} ->
    match
      unsplit_ExprOp
        l
    with
      (linfo, l)
    then
      let info =
        mergeInfo
          linfo
          (x.__br_info)
      in
      (info, FunctionCallExpr
        { info =
            info,
          args =
            x.args })
    else
      never

end

lang ListExprOp = ExprOpBase + ListExprAst

  syn ExprOp =
  | ListExprOp {__br_terms: [Info], __br_info: Info, value: [{v: LName, i: Info}], key: [{v: LName, i: Info}]}

  sem get_ExprOp_info =
  | ListExprOp x ->
    x.__br_info

  sem get_ExprOp_terms =
  | ListExprOp x ->
    x.__br_terms

  sem unsplit_ExprOp =
  | AtomP {self = ListExprOp x} ->
    (x.__br_info, ListExpr
      { info =
          x.__br_info,
        value =
          x.value,
        key =
          x.key })

end

lang AssumeStmntOp = StmntOpBase + AssumeStmntAst

  syn StmntOp =
  | AssumeStmntOp {__br_terms: [Info], __br_info: Info, args: [Expr], randomVar: [{v: LName, i: Info}], distribution: [{v: UName, i: Info}]}

  sem get_StmntOp_info =
  | AssumeStmntOp x ->
    x.__br_info

  sem get_StmntOp_terms =
  | AssumeStmntOp x ->
    x.__br_terms

  sem unsplit_StmntOp =
  | AtomP {self = AssumeStmntOp x} ->
    (x.__br_info, AssumeStmnt
      { info =
          x.__br_info,
        args =
          x.args,
        randomVar =
          match
            x.randomVar
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

lang ObserveStmntOp = StmntOpBase + ObserveStmntAst

  syn StmntOp =
  | ObserveStmntOp {__br_terms: [Info], __br_info: Info, args: [Expr], value: [Expr], distribution: [{v: UName, i: Info}]}

  sem get_StmntOp_info =
  | ObserveStmntOp x ->
    x.__br_info

  sem get_StmntOp_terms =
  | ObserveStmntOp x ->
    x.__br_terms

  sem unsplit_StmntOp =
  | AtomP {self = ObserveStmntOp x} ->
    (x.__br_info, ObserveStmnt
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

lang ForLoopStmntOp = StmntOpBase + ForLoopStmntAst

  syn StmntOp =
  | ForLoopStmntOp {__br_terms: [Info], __br_info: Info, some: [Stmnt], end: [Expr], begin: [Expr]}

  sem get_StmntOp_info =
  | ForLoopStmntOp x ->
    x.__br_info

  sem get_StmntOp_terms =
  | ForLoopStmntOp x ->
    x.__br_terms

  sem unsplit_StmntOp =
  | AtomP {self = ForLoopStmntOp x} ->
    (x.__br_info, ForLoopStmnt
      { info =
          x.__br_info,
        some =
          x.some,
        end =
          match
            x.end
          with
            [ x1 ] ++ _ ++ ""
          then
            x1
          else
            never,
        begin =
          match
            x.begin
          with
            [ x2 ] ++ _ ++ ""
          then
            x2
          else
            never })

end

lang IfStmntOp = StmntOpBase + IfStmntAst

  syn StmntOp =
  | IfStmntOp {__br_terms: [Info], __br_info: Info, some: [Stmnt], condition: [Expr]}

  sem get_StmntOp_info =
  | IfStmntOp x ->
    x.__br_info

  sem get_StmntOp_terms =
  | IfStmntOp x ->
    x.__br_terms

  sem unsplit_StmntOp =
  | AtomP {self = IfStmntOp x} ->
    (x.__br_info, IfStmnt
      { info =
          x.__br_info,
        some =
          x.some,
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

lang ReturnStatementStmntOp = StmntOpBase + ReturnStatementStmntAst

  syn StmntOp =
  | ReturnStatementStmntOp {__br_terms: [Info], __br_info: Info, some: [Expr]}

  sem get_StmntOp_info =
  | ReturnStatementStmntOp x ->
    x.__br_info

  sem get_StmntOp_terms =
  | ReturnStatementStmntOp x ->
    x.__br_terms

  sem unsplit_StmntOp =
  | AtomP {self = ReturnStatementStmntOp x} ->
    (x.__br_info, ReturnStatementStmnt
      { info =
          x.__br_info,
        some =
          match
            x.some
          with
            [ x1 ] ++ _ ++ ""
          then
            x1
          else
            never })

end