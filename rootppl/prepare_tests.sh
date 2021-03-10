#!/bin/bash
TESTDIR=tests
DATADIR=test-data
NUMPART=10000
NUMRUN=10
# test var needs to be set testWaitingTime testWaitingTime  testObserveWaitingTime testObserveWaitingTimeDelayed
for test in testObserveXEvents testObserveXEventsDelayed
do
    make model=$TESTDIR/$test.cu
    ./program $NUMPART $NUMRUN
    mv $TESTDIR/$test.csv $TESTDIR/$DATADIR
    cp log_norm_const.txt $TESTDIR/$DATADIR/$test.logz
done
