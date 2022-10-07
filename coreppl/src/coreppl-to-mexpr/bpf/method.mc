include "mexpr/ast.mc"
include "../../coreppl.mc"
include "../../dppl-arg.mc"

lang BPFMethod = InferMethodBase + MExprAst
  syn InferMethod =
  | BPF {particles : Int}

  sem inferMethodToString =
  | BPF _ -> "mexpr-bpf"

  sem inferMethodFromOptions options =
  | "mexpr-bpf" ->
    BPF {particles = options.particles}

  sem parseInferMethodH env =
  | "mexpr-bpf" ->
    BPF {particles = parseRequiredFieldInt env "particles"}

  sem typeCheckInferMethod env tyRes info =
  | BPF _ ->
    let weightsTy = TySeq {ty = TyFloat {info = info}, info = info} in
    let samplesTy = TySeq {ty = tyRes, info = info} in
    TyRecord {
			fields = mapFromSeq cmpSID [
				(stringToSid "0", weightsTy), (stringToSid "1", samplesTy)],
			info = info }
end
