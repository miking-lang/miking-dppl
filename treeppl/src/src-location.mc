-- Functions that determine where on the system the TreePPL source files can be found

include "sys.mc"

let tpplSrcCwd = sysGetCwd ()

let tpplSrcLocUnix =
  match sysGetEnv "HOME" with Some path then
    join [path, "/.local/src/treeppl/"]
  else error "Environment variable HOME not set"

let tpplSrcLoc =
  match sysGetEnv "TPPL_SRC" with Some path then path
  else
    if sysFileExists tpplSrcLocUnix then tpplSrcLocUnix else tpplSrcCwd
