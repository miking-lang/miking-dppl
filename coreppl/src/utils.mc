include "common.mc"
include "option.mc"
include "map.mc"

let _wts = ref (mapEmpty cmpString)

let utilStartTimer = lam str.
  modref _wts (mapInsert str (wallTimeMs ()) (deref _wts))

let utilPrintTime = lam str.
  optionMapOrElse (lam. printLn (join ["No active timer ", str]))
    (lam wt1.
      let wt2 = wallTimeMs () in
      printLn (join [
        "============\n",
        str, " timed at ", float2string (subf wt2 wt1), " ms since start",
        "\n============"
      ]))
    (mapLookup str (deref _wts))
