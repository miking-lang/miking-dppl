source("test_functions.R")

options(warn=-1)

args <- commandArgs(trailingOnly = TRUE)

renormalize_log_weights_stable(test_file = "testStudentClassic", subsample_size = 10000)

#renormalize_weights(test_file = "testStudent", numparts = args[[1]])
do_test(left= "testStudentClassic", right = "testStudentClassicBirch", size = 1000)
do_test(left= "testStudentClassic", right = "testStudentClassicR", size = 1000)
#do_test(left= "testStudent", right = "testStudentBirch", size = 1000)
