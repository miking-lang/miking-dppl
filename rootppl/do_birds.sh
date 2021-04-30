#!/bin/bash
BIRDDIR=/home/viktor/ownCloud/miking-dppl/rootppl/analysis/birds
NUMPART=$1
NUMRUN=$2
CORES=32
RESULTDIR=/home/viktor/ownCloud/miking-dppl/rootppl/results/birds-e2-$NUMPART
if [ ! -f $RESULTDIR ]
then
    mkdir $RESULTDIR
fi
# test var needs to be set 
for model in $(ls $BIRDDIR)
do
    make clean
    make model=$BIRDDIR/$model -j$CORES
    ./program $NUMPART $NUMRUN
    cp log_norm_const.txt $RESULTDIR/$model.logz
    mv results/EXP-* $RESULTDIR/
done
