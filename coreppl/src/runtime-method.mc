include "dppl-arg.mc"
lang RuntimeMethodBase
  syn RuntimeMethod =
  | Default { m:String}
  sem cmpRuntimeRMethod : RuntimeMethod -> RuntimeMethod -> Int
  sem cmpRuntimeRMethod lhs =
  | rhs -> subi (constructorTag lhs) (constructorTag rhs)
  
  sem runtimeMethodToString : RuntimeMethod -> String
  sem runtimeMethodToString =
  | Default d -> d.s

  sem runtimeMethodFromOptions : Options -> String -> RuntimeMethod
  sem runtimeMethodFromOptions options =
  | s -> error (concat "Unknown runtime method string: " s)

end
