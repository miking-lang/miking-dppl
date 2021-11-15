
include "ext/file-ext.mc"


-- Returns the number of particles/points from the program argument
let numarg = lam.
  if neqi (length argv) 2 then
    writeString stderr
      "The number of particles/points need to be given as a program argument.\n";
    exit 1
  else string2int (get argv 1)


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
