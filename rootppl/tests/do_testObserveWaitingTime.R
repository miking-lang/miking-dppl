source("test_functions.R")

options(warn=-1)

args <- commandArgs(trailingOnly = TRUE)

renormalize_log_weights_stable(test_file = "testObserveWaitingTime", subsample_size =  args[[1]])
renormalize_log_weights_stable(test_file = "testObserveWaitingTimeDelayed", subsample_size = args[[1]])
do_test(left= "testObserveWaitingTime", right = "testObserveWaitingTimeDelayed", size = 1000)
do_test_logz(left = "testObserveWaitingTime", right = "testObserveWaitingTimeDelayed")
