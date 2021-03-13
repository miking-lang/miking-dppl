options(warn=-1)

do_test = function(left, right, size = 35, dir = "test-data//") {
  leftf = read.csv(paste0(dir, left, ".csv"))
  rightf = read.csv(paste0(dir, right, ".csv"))

  sample_left = sample(leftf[[1]], size = size, prob = leftf[[2]])
  sample_right = sample(rightf[[1]], size = size, prob = rightf[[2]])

  test_result = ks.test(sample_left, sample_right)

  if (test_result$p.value > 0.05) {
    cat(paste("PASS:", left, "<->", right,  "p:", test_result$p.value, "\n"))
  } else {
    cat(paste("FAIL:", left, "<->", right,  "p:", test_result$p.value, "\n"))
  }
}

do_test_logz = function(left, right, dir = "test-data//") {
  leftz = as.numeric(strsplit(sub("\\]", "", sub("\\[", "", readLines(paste0(dir, left, ".logz")))), ",")[[1]])
  rightz = as.numeric(strsplit(sub("\\]", "", sub("\\[", "", readLines(paste0(dir, right, ".logz")))), ",")[[1]])
  test_result = t.test(leftz, rightz)
  if (test_result$p.value > 0.05) {
    cat(paste("PASS:", "log Z", left, "<->", right,  "p:", test_result$p.value, "\n"))
  } else {
    cat(paste("FAIL:", "log Z", left, "<->", right,  "p:", test_result$p.value, "\n"))
  }
}

do_test(left= "testWaitingTime", right = "testWaitingTimeDelayed", size = 1000)
do_test(left= "testObserveWaitingTime", right = "testObserveWaitingTimeDelayed", size = 1000)
do_test_logz(left = "testObserveWaitingTime", right = "testObserveWaitingTimeDelayed")

do_test(left= "testObserveXEvents", right = "testObserveXEventsDelayed", size = 1000)
do_test_logz(left = "testObserveXEvents", right = "testObserveXEvents")

do_test(left= "testStudentClassic", right = "testStudentClassicBirch", size = 1000)
do_test(left= "testStudent", right = "testStudentBirch", size = 1000)

do_test(left= "testNormalInverseGammaNormal", right = "testNormalInverseGammaNormal", size = 1000)
