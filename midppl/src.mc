-- Functions that determine where on the system the Miking DPPL source files can be found

include "sys.mc"

let srcCwd = sysGetCwd ()

let srcLocUnix =
  match sysGetEnv "HOME" with Some path then
    join [path, "/.local/src/midppl/"]
  else srcCwd

let srcLoc =
  match sysGetEnv "MIDPPL_SRC" with Some path then path
  else
    if sysFileExists srcLocUnix then srcLocUnix else srcCwd

