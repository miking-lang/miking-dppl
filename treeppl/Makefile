src/treeppl-ast.mc: src/treeppl.syn
	tool src/treeppl.syn src/treeppl-ast.mc

flip: src/treeppl-ast.mc models/flip/flip.tppl
	boot eval --debug-stack-trace src/tpplc.mc -- models/flip/flip.tppl models/flip/data.mc

flip-ast: src/treeppl.syn src/testparser.mc
	boot eval src/testparser.mc -- models/flip/flip.tppl

.PHONY: flip flip-ast
