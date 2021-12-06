source("test_functions.R")

options(warn=-1)

args <- commandArgs(trailingOnly = TRUE)

renormalize_log_weights_stable(test_file = "testNormalInverseGammaNormal", subsample_size = args[[1]])
renormalize_log_weights_stable(test_file = "testLinearNormalInverseGammaNormal", subsample_size = args[[1]])
do_test(left= "testNormalInverseGammaNormal", right = "testNormalInverseGammaNormalBirch", size = 1000)
do_test(left= "testNormalInverseGammaNormal", right = "testLinearNormalInverseGammaNormal", size = 1000)
