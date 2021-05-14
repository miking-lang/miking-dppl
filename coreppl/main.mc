


include "arg.mc"


type Options = {
  debugParse : Bool,
  particles : Int
}

let optionDefaults = {

}

let optionConfig = [
("--particles", "=", "Number of particles for SMC an importance sampling",
  lam o. lam v. {o with particles = argInt v})
]

-- Menu
let menu = join [
  "Usage: mi <command> [<options>] file [<options>]\n\n",
  argHelpArgString optionConfig
]


mexpr


utest xvar with 1 in

()
