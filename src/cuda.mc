include "pplcore.mc"

lang C

  syn Expr =
    | CTest {}

end

lang CUDA

  syn Expr =
    | CuTest {}

  sem output =
    | CuTest c -> "Test"

end
