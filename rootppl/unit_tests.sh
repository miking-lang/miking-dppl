#!/bin/bash
TESTDIR=tests
DATADIR=test-data
NUMPART=10000
NUMRUN=10
CORES=32
# test var needs to be set 
for test in  testChiSquared testStudentClassic testStudent testWaitingTime testWaitingTime  testObserveWaitingTime testObserveWaitingTimeDelayed testObserveXEvents testObserveXEventsDelayed testNormalInverseGammaNormal testNormalInverseGammaNormalMultipass testLogAlphaSigmaSquared testLogAlphaSigmaSquaredDelayed
do
    make model=$TESTDIR/$test.cu -j$CORES
    ./program $NUMPART $NUMRUN
    mv $TESTDIR/$test.csv $TESTDIR/$DATADIR
    cp log_norm_const.txt $TESTDIR/$DATADIR/$test.logz
done
echo
echo
cd $TESTDIR
Rscript ./do_tests.R
