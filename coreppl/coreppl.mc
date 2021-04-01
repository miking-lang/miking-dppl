-- CorePPL
-- Note that we should NOT implement eval or compile functions
-- CorePPL. Instead, we implement function 'toMExpr' which translates
-- the core terms into an MExpr term. By doing so, we can use the
-- standard eval and compile functions when running the inference.

include "mexpr/ast.mc"
include "mexpr/pprint.mc"
include "mexpr/ast-builder.mc"
include "dist.mc"
include "smc.mc"

-- We will add this to the Miking later on
lang Ast 
  syn Expr =
end



lang Infer = 
  -- Evaluation of TmInfer returns a TmDist
  syn Expr =
  | TmInfer { method: InferMethod
              model: Expr}

  -- Interface type for infer methods
  syn InferMethod =

end


-- Assume defines a new random variable
lang Assume = Dist
  syn Expr =
  | TmAssume { dist: Expr,
               info: Info} 

end


-- Observe gives a random variable conditioned on a specific value
lang Observe = Dist
  syn Expr =
  | TmObserve { dist: Expr,
                value: Expr,
                info: Info }                
end

-- Defines a weight term
lang Weight =
  syn Expr =
  | TmWeight { weight: Float }
end


-- Translations in between weight and observe terms
lang ObserveWeightTranslation = Observe + Weight
  -- Translates ALL observe terms into weight terms. 
  sem observeToWeight =
  | TmObserve -> unit_ -- TODO

  -- Translates SOME weight terms into observe terms.
  sem weightToObserve =
  | TmWeight -> unit_ -- TODO
end




   


lang CorePPL = Ast + Assume + Observe + Weight + ObserveWeightTranslation


lang CorePPLinference = CorePPL + SMC 
