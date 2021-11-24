include "arg.mc"

-- Options type
type Options = {
  method: String,
  particles : Int,
  resample: String,
  printModel: Bool,
  printMCore: Bool,
  exitBefore: Bool,
  transform: Bool
}

-- Default values for options
let default = {
  method = "",
  particles = 5000,
  resample = "manual",
  printModel = false,
  printMCore = false,
  exitBefore = false,
  transform = false
}

-- Options configuration
let config = [
  ([("-m", " ", "<method>")],
    "The selected inference method. The supported methods are: importance, rootppl-smc.",
    lam p: ArgPart.
      let o: Options = p.options in {o with method = argToString p}),
  ([("-p", " ", "<particles>")],
    join ["The number of particles. The default is 5000. This option is used if one ",
          "of the following methods are used: importance, rootppl-smc."],
    lam p: ArgPart.
      let o: Options = p.options in {o with particles = argToIntMin p 1}),
  ([("--resample", " ", "<method>")],
    "The selected resampling method, for inference algorithms where applicable. The supported methods are: weight, align, manual.",
    lam p: ArgPart.
      let o: Options = p.options in {o with resample = argToString p}),
  ([("--print-model", "", "")],
    "The parsed model is pretty printed before inference.",
    lam p: ArgPart.
      let o: Options = p.options in {o with printModel = true}),
  ([("--print-mcore", "", "")],
    "Print the generated MCore program before execution.",
    lam p: ArgPart.
      let o: Options = p.options in {o with printMCore = true}),
  ([("--exit-before", "", "")],
    "Exit before inference takes place. ",
    lam p: ArgPart.
      let o: Options = p.options in {o with exitBefore = true}),
  ([("--transform", "", "")],
    "The model is transformed to an efficient representation if possible.",
    lam p: ArgPart.
      let o: Options = p.options in {o with transform = true})
]

-- Menu
let menu = lam. join [
  "Usage: midppl file.mc [<options>]\n\n",
  "Options:\n",
  argHelpOptions config,
  "\n"
]

