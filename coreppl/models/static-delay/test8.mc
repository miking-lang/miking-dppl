mexpr
let lst = [assume (Gaussian 0. 1.), assume (Gaussian 0. 1.), assume (Gaussian 0. 1.), assume (Gaussian 0. 1.)] in
let i = assume (Categorical [0.3,0.2,0.4,0.1]) in
let a = get lst i in
observe 0.5 (Gaussian a 1.)


/-

mexpr
let paramlst = [(0.,1.),(0.,1.), (0.,1.),(0.,1.)] in
let i = assume (Categorical [0.3,0.2,0.4,0.1]) in
let a = get lst i in
let param = get paramlst i in
let margParam = f param in
observe 0.5 (Gaussian margParam 1.);
let reorderedParam = blabla in
let lst = mapi (lam ind. lam e. if eqi ind i then reorderedParam else e) [assume (Gaussian 0. 1.), assume (Gaussian 0. 1.), assume (Gaussian 0. 1.), assume (Gaussian 0. 1.)] in
-/