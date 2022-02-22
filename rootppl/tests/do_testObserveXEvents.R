source("test_functions.R")

options(warn=-1)

args <- commandArgs(trailingOnly = TRUE)

renormalize_log_weights_stable(test_file = "testObserveXEvents", subsample_size =  args[[1]])
renormalize_log_weights_stable(test_file = "testObserveXEventsDelayed", subsample_size = args[[1]])
do_test(left= "testObserveXEvents", right = "testObserveXEventsDelayed", size = 1000)
do_test_logz(left = "testObserveXEvents", right = "testObserveXEvents")
