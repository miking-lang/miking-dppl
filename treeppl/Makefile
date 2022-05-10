compiler/treeppl-ast.mc: compiler/treeppl.syn
	tool compiler/treeppl.syn compiler/treeppl-ast.mc

flip: compiler/treeppl-ast.mc examples/flip/flip.tppl
	boot eval --debug-stack-trace compiler/tpplc.mc -- examples/flip/flip.tppl examples/flip/data.mc

flip-ast: compiler/treeppl.syn compiler/testparser.mc
	boot eval compiler/testparser.mc -- examples/flip/flip.tppl

.PHONY: flip flip-ast
