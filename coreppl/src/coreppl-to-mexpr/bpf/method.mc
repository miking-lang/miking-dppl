include "mexpr/ast.mc"
include "../../coreppl.mc"

lang BPFMethod = InferMethodBase + MExprAst
  syn InferMethod =
  | BPF ()

  sem inferMethodToString =
  | BPF _ -> "mexpr-bpf"

  sem parseInferMethod =
  | "mexpr-bpf" -> BPF ()

  sem typeCheckInferMethod env tyRes info =
  | BPF _ ->
    let weightsTy = TySeq {ty = TyFloat {info = info}, info = info} in
    let samplesTy = TySeq {ty = tyRes, info = info} in
    TyRecord {
			fields = mapFromSeq cmpSID [
				(stringToSid "0", weightsTy), (stringToSid "1", samplesTy)],
			info = info }
end
