


lang Dist
  syn Expr = 
  | TmDist { dist: Dist }

  syn Dist =

end



lang ExpDist = Dist
  syn Dist = 
  | DExp { a: Expr }

end


lang BernDist = Dist
  syn Dist = 
  | DBern { p: Expr }

end


lang BetaDist = Dist

  syn Dist = 
  | DBeta { a: Expr, b: Expr }

end


lang EmpiricalDist = Dist

  syn Dist =
  | DEmpirical { sample: [(Float, Expr)] }

  sem getConstStringCode (indent : Int) =
  | DEmpirical d ->
    let pprintEnvEmpty = { nameMap = mapEmpty nameCmp,
                          count = mapEmpty cmpString,
                          strings = mapEmpty cmpString } in
    join ["empirical", pprintNewline (pprintIncr indent), pprintCode indent pprintEnvEmpty d.sample]

end



lang DistAll = DExpr + DBern + DBeta + DEmpirical


