
include "ext/file-ext.mc"
include "ext/math-ext.mc"
include "seq.mc"
include "string.mc"

type Res a = ([Float],[a])
type ResOption a = ([Float],[Option a])

-- Returns the number of particles/points from the program argument
let numarg = lam.
  if neqi (length argv) 2 then
    writeString stderr
      "The number of particles/points need to be given as a program argument.\n";
    exit 1
  else string2int (get argv 1)

-- Returns the number of particles/samples/executions and sweeps from the program argument
let monteCarloArgs = lam.
  let particles = if leqi (length argv) 1 then 1 else string2int (get argv 1) in
  let sweeps = if leqi (length argv) 2 then 1 else string2int (get argv 2) in
  (particles, sweeps)

-- Save the data to a CSV file
let saveCSV = lam res. lam names. lam filename. lam expOnLogWeights.
  match writeOpen filename with Some ch then
    writeString ch (strJoin "," names);
    writeString ch "\n";
    iter (lam lst. writeString ch (strJoin "," (map float2string lst));
                   writeString ch  "\n") (expOnLogWeights res);
    writeClose ch
  else
    writeString stderr (join ["Cannot write to file ", filename, "\n"])


-- Saves the CSV file and pretty prints expected values, variance, etc.
let printStatistics = lam res. lam names. lam normConst. lam expVals. lam varianceVals.
  let pad = 18 in
  let padPrint = lam s. lam n.
    if geqi n (length s) then
      print s; print (create (subi n (length s)) (lam. ' '))
    else print s in
  padPrint "Variable" 14;
  padPrint "Expected Value" pad;
  padPrint "Variance" pad;
  padPrint "Standard Deviation" pad;
  print "\n";
  recursive let work = lam names. lam ev. lam vv.
    match (names, ev, vv) with ([n]++ns, [e]++es, [v]++vs) then
      if isPrefix eqChar "#" n then work ns ev vv
      else
        padPrint n 14;
        padPrint (float2string e) pad;
        padPrint (float2string v) pad;
        padPrint (float2string (sqrt v)) pad;
        print "\n";
        work ns es vs
    else ()
  in
    work names expVals varianceVals;
    print "\n";
    print (join ["Normalization constant: ", float2string normConst, "\n"])


-- Computing the normalization constant using the log-sum-exp trick
let normConstant : [Float] -> Float = lam res.
  let negInf = (divf (negf 1.) 0.) in
  let max = foldl (lam acc. lam x. if geqf x acc then x else acc) negInf res in
  let sum = foldl (lam acc. lam x. addf (exp (subf x max)) acc) 0. res in
  subf (addf max (log sum)) (log (int2float (length res)))

-- Computes the expected value for all variables. Returns
-- a list that excludes the weight component and only contains
-- the expected values for the given variables
-- The function assumes that the first element in the
-- result list is the weight.
let expectedValues = lam res. lam normConst.
  foldl (lam acc. lam t.
     let w = exp (subf (head t) normConst) in
     let ys = tail t in
     recursive let work = lam acc. lam xs.
       match (acc,xs) with ([a]++as, [x]++xs) then
         cons (addf (mulf x w) a) (work as xs)
       else []
     in
       work acc ys) (create (subi (length (head res)) 1) (lam. 0.)) res

-- Computes the variances for the list of variables
-- Assumes that the first element in the result list is the weight.
let variance = lam res. lam expVals.
  let sum = foldl (lam acc. lam t.
    recursive let work = lam acc. lam xs. lam expv.
      match (acc,xs,expv) with ([a]++as, [x]++xs, [e]++es) then
        let v = subf x e in
        cons (addf a (mulf v v)) (work as xs es)
      else []
    in
      work acc (tail t) expVals) (create (subi (length (head res)) 1) (lam. 0.)) res
  in
    let dval = int2float (length res) in
    map (lam x. divf x dval) sum

-- The log-weight is always in the first column (list of lists)
let expOnLogWeights = lam res.
  mapReverse (lam t. match t with [x]++xs in cons (exp x) xs) res

-- The output function. Prints normalizing constants, expected values, and variance
-- to the standard output. Saves the plot data in a CSV file.
-- NOTE(dlunde,2022-06-07): Does not currently type check
-- let output = lam res. lam names.
--   let names = cons "#" names in
--   let nc = normConstant res in
--   let expVals = expectedValues res nc in
--   let varianceVals = variance res expVals in
--   printStatistics res names nc expVals varianceVals;
--   saveCSV res names "data.csv" expOnLogWeights

let printSamples : all a. (a -> String) -> [Float] -> [a] -> () =
  lam printFun. lam weights. lam samples.
    recursive let rec : [Float] -> [a] -> () = lam weights. lam samples.
      if null weights then () else
        let w = head weights in
        let weights = tail weights in
        let s = head samples in
        let samples = tail samples in
        print (printFun s);
        print " ";
        printLn (float2string w);
        rec weights samples
    in rec weights samples

let printSamplesOption : all a. (a -> String) -> [Float] -> [Option a] -> () =
  lam printFun. lam weights. lam samples.
    recursive let rec : [Float] -> [Option a] -> () = lam weights. lam samples.
      if null weights then () else
        let w = head weights in
        let weights = tail weights in
        let s = head samples in
        let samples = tail samples in
        (match s with Some s then print (printFun s)
         else print ".");
        print " ";
        printLn (float2string w);
        rec weights samples
    in rec weights samples
