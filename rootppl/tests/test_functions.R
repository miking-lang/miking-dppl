renormalize_weights = function(test_file, dir = "test-data//", numparts) {
  logz = as.numeric(strsplit(sub("\\]", "", sub("\\[", "", readLines(paste0(dir, test_file, ".logz")))), ",")[[1]])
  
  test = read.csv(paste0(dir, test_file, ".csv"), header = FALSE)
  
  test$logz = unlist(lapply(logz, function(l) {rep(l, numparts)}))
  
  test[[2]] = test[[2]] * exp(test$logz)
  test[[2]] = test[[2]] / sum(test[[2]])
  
  write.table(test, paste0(dir, test_file, ".csv"), 
                           sep = ",", quote = FALSE, row.names = FALSE,
              col.names = FALSE)
} 


renormalize_log_weights_stable = function(test_file, dir = "test-data//", subsample_size = 5) {
  logz = as.numeric(strsplit(sub("\\]", "", sub("\\[", "", readLines(paste0(dir, test_file, ".logz")))), ",")[[1]])
  
  test = read.csv(paste0(dir, test_file, ".csv"), header = FALSE)
  
  test$logz = unlist(lapply(logz, function(l) {rep(l, subsample_size)}))

                                        #  test[[2]] = test[[2]] * exp(test$logz)
                                        #  test[[2]] = test[[2]] / sum(test[[2]])
  
  logweightlogz = test[[2]] + test$logz
  unnormalized_weight = exp(logweightlogz - max(logweightlogz))
  test[[2]] = unnormalized_weight/sum(unnormalized_weight)
  
  write.table(test, paste0(dir, test_file, ".csv"), 
                           sep = ",", quote = FALSE, row.names = FALSE,
              col.names = FALSE)
}

do_test = function(left, right, size = 35, dir = "test-data//") {
  leftf = read.csv(paste0(dir, left, ".csv"))
  rightf = read.csv(paste0(dir, right, ".csv"))

  sample_left = sample(leftf[[1]], size = size, prob = leftf[[2]])
  sample_right = sample(rightf[[1]], size = size, prob = rightf[[2]])

  test_result = ks.test(sample_left, sample_right)

  if (test_result$p.value > 0.01) {
    cat(paste("PASS:", left, "<->", right,  "p:", test_result$p.value, "\n"))
  } else {
    cat(paste("FAIL:", left, "<->", right,  "p:", test_result$p.value, "\n"))
  }
}

do_test_logz = function(left, right, dir = "test-data//") {
  leftz = as.numeric(strsplit(sub("\\]", "", sub("\\[", "", readLines(paste0(dir, left, ".logz")))), ",")[[1]])
  rightz = as.numeric(strsplit(sub("\\]", "", sub("\\[", "", readLines(paste0(dir, right, ".logz")))), ",")[[1]])
  test_result = t.test(leftz, rightz)
  if (test_result$p.value > 0.01) {
    cat(paste("PASS:", "log Z", left, "<->", right,  "p:", test_result$p.value, "\n"))
  } else {
    cat(paste("FAIL:", "log Z", left, "<->", right,  "p:", test_result$p.value, "\n"))
  }
}