-- Applies a function of two arguments elementwise over two sequences.
let map2 = lam f. lam xs. lam ys.
  create (length xs) (lam i. f (get xs i) (get ys i))

-- Zips two seqences together to form a sequence of pairs.
let zip = lam xs. lam ys. map2 (lam x. lam y. (x, y)) xs ys

-- Flattens a sequence of sequences.
let join = lam seq. foldl concat [] seq

-- Computes an ODE solution trace for the ODE `f`, inital values `xs0` and
-- time-steps `dts`.
let odeTrace : (Float -> [Float] -> [Float]) -> [Float] -> [Float] -> [[Float]] =
  lam f. lam xs0. lam dts.
    reverse
      (foldl
         (lam trace. lam dt.
           cons (solveode (RK4 { stepSize = 1e-3 }) f (head trace) dt) trace)
         [xs0] dts)

-- `printSeq p xs` prints the sequence `xs` to standard out, printing each
-- element of the seqence with `p`.
recursive let printSeq : all a. (a -> ()) -> [a] -> () =
  lam p. lam xs.
    print "[";
    switch xs
    case [] then print "]"
    case [x] then p x; print "]"
    case [x] ++ xs then p x; print ","
    end
end

-- prints a float to standard out.
let printFloat = lam x. print (float2string x)

-- prints an ode trace, consisting of time-value pairs, to standard out.
let odePrintTrace : [(Float, [Float])] = lam tr.
  let pf = printFloat in
  printSeqx
    (lam p.
      match p with (t, xs) in
      print "("; printFloat t; print ","; printSeq printFloat xs; print ")")
    tr

-- Prints a distribution to standard out.
let printDist = lam printFun. lam dist.
  match distEmpiricalSamples dist with (samples, weights) in
  recursive let work = lam samples. lam weights.
    match (samples, weights) with ([s] ++ samples, [w] ++ weights) then
      print (printFun s);
      print " ";
      print (float2string w);
      print "\n";
      work samples weights
    else ()
  in work samples weights
