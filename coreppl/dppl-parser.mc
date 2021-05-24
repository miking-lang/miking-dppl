

include "mexpr/boot-parser.mc"
include "mexpr/keyword-maker.mc"
include "coreppl.mc"



lang DPPLParser = BootParser + MExprPrettyPrint + CorePPL + KeywordMaker

  -- Keyword maker
  sem isKeyword =
  | TmAssume _ -> true
  | TmObserve _ -> true
  | TmWeight _ -> true
  | TmDist _ -> true

  sem matchKeywordString (info: Info) =
  | "assume" -> Some (1, lam lst. TmAssume {dist = get lst 0,
                                            ty = TyUnknown {info = info},
                                            info = info})
  | "observe" -> Some (2, lam lst. TmObserve {value = get lst 0,
                                              dist = get lst 1,
                                              ty = TyUnknown {info = info},
                                              info = info})
  | "weight" -> Some (1, lam lst. TmWeight {weight = get lst 0,
                                            ty = TyUnknown {info = info},
                                            info = info})
  | "Bern" -> Some (1, lam lst. TmDist {dist = DBern {p = get lst 0},
                                        ty = TyUnknown {info = info},
                                        info = info})

  | "Beta" -> Some (2, lam lst. TmDist {dist = DBeta {a = get lst 0, b = get lst 1},
                                        ty = TyUnknown {info = info},
                                        info = info})


end

let keywords =
["assume", "observe", "weight",
 "Bern", "Beta"]


let getAst = lam filename. lam printModel.
  use DPPLParser in
  -- Read and parse the mcore file
  let ast = makeKeywords [] (parseMCoreFile keywords filename) in
  -- Pretty print the model?
  if printModel then
    print (expr2str ast);
    print "\n"

  else ();
  ast
