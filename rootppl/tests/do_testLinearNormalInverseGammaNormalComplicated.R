source("test_functions.R")

options(warn=-1)

args <- commandArgs(trailingOnly = TRUE)

renormalize_log_weights_stable(test_file = "testLinearNormalInverseGammaNormalComplicated", subsample_size = args[[1]])
do_test(left= "testLinearNormalInverseGammaNormalComplicated", right = "testLinearNormalInverseGammaNormalComplicatedBirch", size = 1000)


