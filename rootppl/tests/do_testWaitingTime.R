source("test_functions.R")

options(warn=-1)

args <- commandArgs(trailingOnly = TRUE)

renormalize_log_weights_stable(test_file = "testWaitingTime", subsample_size = args[[1]])
renormalize_log_weights_stable(test_file = "testWaitingTimeDelayed", subsample_size = args[[1]])
do_test(left= "testWaitingTime", right = "testWaitingTimeDelayed", size = 1000)
