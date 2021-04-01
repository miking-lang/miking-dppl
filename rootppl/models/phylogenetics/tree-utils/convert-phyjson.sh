#!/bin/bash
echo "convert-phyjson.sh JSONDIR OUTPUT"
echo "Will process JSON dir into OUTPUT.cuh header file"
echo "Adjust tree_utils.cuh to include it"

JSONDIR=$1
OUTPUT=$2

trees=$(ls $JSONDIR)

echo "#ifndef $OUTPUT
#define $OUTPUT

/*
 * File birds.cuh contains pre-processed phylogenetic trees stored in SoA-format (Structure of Arrays). Taken from the concept paper.
 */

//const int ROOT_IDX = 0;
" > $OUTPUT.cuh


for t in $trees
do
    node tree-parser.js $JSONDIR/$t $t >> $OUTPUT.cuh
done

	 

echo "#endif" >> $OUTPUT.cuh
