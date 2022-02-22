source("test_functions.R")

options(warn=-1)

args <- commandArgs(trailingOnly = TRUE)

renormalize_log_weights_stable(test_file = "testNormalInverseGammaNormalMultipass", subsample_size = args[[1]])
renormalize_log_weights_stable(test_file = "testLinearNormalInverseGammaNormalMultipass", subsample_size = args[[1]])
do_test(left= "testNormalInverseGammaNormalMultipass", right = "testNormalInverseGammaNormalBirchMultipass", size = 1000)
do_test(left= "testNormalInverseGammaNormalMultipass", right = "testLinearNormalInverseGammaNormalMultipass", size = 1000)
