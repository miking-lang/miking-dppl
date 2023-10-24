include "../coreppl.mc"
include "../dppl-arg.mc"
lang CounterMethod = MExprPPL
  syn RuntimeMethod =
  | Counter {m:String}

  sem runtimeMethodFromOptions options =
  | ("counter")&s -> Counter {m=s}

  sem runtimeMethodToString =
  | Counter c -> c.m

end
