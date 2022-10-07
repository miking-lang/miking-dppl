include "mexpr/ast.mc"
include "../../coreppl.mc"
include "../../dppl-arg.mc"

lang ImportanceSamplingMethod = InferMethodBase + MExprAst
  syn InferMethod =
  | Importance {particles : Int}

  sem inferMethodToString =
  | Importance _ -> "mexpr-importance"

  sem inferMethodFromOptions options =
  | "mexpr-importance" ->
    Importance {particles = options.particles}

  sem parseInferMethodH env =
  | "mexpr-importance" ->
    Importance {particles = parseRequiredFieldInt env "particles"}

  sem typeCheckInferMethod env tyRes info =
  | Importance _ ->
    let weightsTy = TySeq {ty = TyFloat {info = info}, info = info} in
    let samplesTy = TySeq {ty = tyRes, info = info} in
    TyRecord {
      fields = mapFromSeq cmpSID [
        (stringToSid "0", weightsTy), (stringToSid "1", samplesTy)],
      info = info }
end
