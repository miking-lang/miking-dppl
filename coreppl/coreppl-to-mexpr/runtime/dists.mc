-- Runtime support for first-class distributions in coreppl-to-mexpr compiler.

include "ext/dist-ext.mc"

type Dist = all a. { sample: () -> a, logObserve: a -> Float }
