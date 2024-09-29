include "int.mc"

-- Applies a function of two arguments elementwise over two sequences.
let map2 = lam f. lam xs. lam ys.
  create (length xs) (lam i. f (get xs i) (get ys i))

-- Zips two seqences together to form a sequence of pairs.
let zip = lam xs. lam ys. map2 (lam x. lam y. (x, y)) xs ys

-- Unzips a sequence of pairs.
let unzip =
  lam xs. foldl (lam acc. lam x. (snoc acc.0 x.0, snoc acc.1 x.1)) ([], []) xs

let onehot = lam n. lam i. create n (lam j. if eqi i j then 1. else 0.)

-- Flattens a sequence of sequences.
let join = lam seq. foldl concat [] seq

-- Divides a seqence into a seqence of sub-sequence of length at most n. E.g.
-- divide [1, 2, 3, 4, 5] 2 = [[1, 2], [3, 4], [5]].
let divide = lam seq. lam n.
  recursive let recur = lam acc. lam seq.
    switch splitAt seq (mini n (length seq))
    case (seq, []) then snoc acc seq
    case (seq1, seq2) then recur (snoc acc seq1) seq2
    end
  in recur [] seq

-- Computes the mean squared error between two sequences
let mse = lam xs : [Float]. lam ys : [Float].
  divf
    (foldl addf 0. (map (lam err. pow err 2.) (map2 subf xs ys)))
    (int2float (length xs))

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
recursive
  let printSeq : all a. (a -> ()) -> [a] -> () = lam p. lam xs.
    print "["; printSeqH p xs

  let printSeqH  =
    lam p. lam xs.
      switch xs
      case [] then print "]"
      case [x] then p x; print "]"
      case [x] ++ xs then p x; print ","; printSeqH p xs
      end
end

-- prints a float to standard out.
let printFloat = lam x. print (float2string x)

-- prints an ode trace, consisting of time-value pairs, to standard out.
let odePrintTrace = lam tr : [(Float, [Float])].
  let pf = printFloat in
  printSeq
    (lam p.
      match p with (t, xs) in
      print "("; printFloat t; print ","; printSeq printFloat xs; print ")")
    tr

let odePrintTraces = lam trs : [[(Float, [Float])]]. printSeq odePrintTrace trs

-- Prints a distribution to standard out.
let printDist = lam printFun. lam dist.
  match distEmpiricalSamples dist with (samples, weights) in
  recursive let work = lam samples. lam weights.
    match (samples, weights) with ([s] ++ samples, [w] ++ weights) then
      printFun s;
      print " ";
      print (float2string w);
      print "\n";
      work samples weights
    else ()
  in work samples weights
