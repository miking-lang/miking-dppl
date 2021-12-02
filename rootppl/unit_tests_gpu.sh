#!/bin/bash
TESTDIR=tests
DATADIR=test-data
NUMPART=10000
NUMRUN=10
CORES=32
GPUARCH=75



# GPU
echo "=============GPU TESTS============="
for test in testStudentClassic testWaitingTime testWaitingTimeDelayed testObserveWaitingTime testObserveWaitingTimeDelayed testObserveXEvents testObserveXEventsDelayed
do
    rootppl clean
    rootppl $TESTDIR/$test.cu --arch $GPUARCH -j $CORES
    ./program $NUMPART $NUMRUN
    mv $TESTDIR/$test.csv $TESTDIR/$DATADIR
    cp log_norm_const.txt $TESTDIR/$DATADIR/$test.logz
done
echo
echo
cd $TESTDIR

for test in testStudentClassic testWaitingTime testObserveWaitingTime testObserveXEvents
do
    Rscript ./do_$test.R $NUMPART
done



echo "Removing data..."
for test in testStudentClassic testWaitingTime testWaitingTimeDelayed testObserveWaitingTime testObserveWaitingTimeDelayed testObserveXEvents testObserveXEventsDelayed
##for test in testChiSquared testStudentClassic testStudent testWaitingTime testWaitingTimeDelayed  testObserveWaitingTime testObserveWaitingTimeDelayed testObserveXEvents testObserveXEventsDelayed testNormalInverseGammaNormal testNormalInverseGammaNormalMultipass testLogAlphaSigmaSquared testLogAlphaSigmaSquaredDelayed testWaitingTimeMultipass testWaitingTimeDelayedMultipass testBernoulli testBetaBernoulli
#for test in  testNormalInverseGammaNormal testNormalInverseGammaNormalMultipass 
do
   rm $DATADIR/$test.csv 
   rm $DATADIR/$test.logz
done

