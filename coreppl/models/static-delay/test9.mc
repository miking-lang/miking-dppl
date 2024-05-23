--cppl coreppl/models/static-delay/test9.mc -m is-lw --no-print-samples --extract-simplification inline --print-model --static-delay

mexpr
let lst = create 2 (lam. assume (Beta 4. 4.)) in
let i = assume (Categorical [0.3,0.7]) in
let a = get lst i in
observe true (Bernoulli a );
let i = assume (Categorical [0.3,0.7]) in
let b = get lst i in
observe false (Bernoulli b )