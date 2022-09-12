include "mexpr/ast.mc"
include "../../coreppl.mc"

lang ImportanceSamplingMethod = InferMethodBase + MExprAst
  syn InferMethod =
  | ImportanceSampling ()

  sem inferMethodName =
  | ImportanceSampling _ -> "inferImportance"

  sem typeCheckInferMethod env tyRes info =
  | ImportanceSampling _ ->
    let weightsTy = TySeq {ty = TyFloat {info = info}, info = info} in
    let samplesTy = TySeq {ty = tyRes, info = info} in
    TyRecord {
      fields = mapFromSeq cmpSID [
        (stringToSid "0", weightsTy), (stringToSid "1", samplesTy)],
      info = info }
end
