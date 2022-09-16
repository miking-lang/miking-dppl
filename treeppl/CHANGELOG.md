# CHANGELOG

## PR-candidate 2022-07-07 (externals-for-treeppl)

This PR attempts to add externals support to TreePPL.

We introduce two new functions, which may need to be moved to `externals.mc`

	sem constructExternalMap : Expr -> Map String Name
	sem filterExternalMap: Set String -> Expr -> Expr

Then we parse a runtime, which only includes `dist-ext.mc` and `math-ext.mc`, we filter only the externals that we need, we run `symbolize` on the filtered externals, then we construct a map from the identifier strings to the Names for the externals, and finally we retrieve the Names of the externals that we need by searching this map. Finally, to use an external we utilize `(app_ (nvar_ NAME))`, whereas NAME is the name we just found and is passed around with a `TpplCompileContext` record.

Currently, there is an issue in `mexprCompile`. To reproduce the issue, attempt to compile any of the examples:

	miking-dppl/treeppl$ mi run src/tppl.mc -- models/externals/externals.tppl models/externals/data.mc 
	checkpoint 1

	FILE "/home/viktor/Dropbox/Work/miking/stdlib/ext/math-ext.mc" 25:0-25:37 ERROR: Error in exprName for CFA
	external externalLog : Float -> Float

	miking-dppl/treeppl$ 

