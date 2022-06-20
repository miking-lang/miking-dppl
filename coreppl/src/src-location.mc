-- Functions that determine where on the system the CorePPL source files can be found

include "sys.mc"

let corepplSrcCwd = sysGetCwd ()

let corepplSrcLocUnix =
  match sysGetEnv "HOME" with Some path then
    join [path, "/.local/src/coreppl/"]
  else error "Environment variable HOME not set"

let corepplSrcLoc =
  match sysGetEnv "MIDPPL_SRC" with Some path then path
  else
    if sysFileExists corepplSrcLocUnix then corepplSrcLocUnix else corepplSrcCwd

