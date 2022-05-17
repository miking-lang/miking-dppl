# ./runppl.sh CRBD-Demo Anatinae 0.8709677419354839 1.0 1.0 1.0 0.5 1.0 1.0 0 1.0 3.0 0.1 0 1.0 3.0 0.1 false 1 0 9999 500 50000 10
treeppl = function(program = "CRBD-Demo", tree, rho,  k_lambda, theta_lambda, k_mu, theta_mu, particles, sweeps, run) {
  oldwd = getwd()
  setwd("/home/viktor/Work/phyrppl/")
  script = "./runppl.sh"
  command = paste(script, program, tree, rho, k_lambda, theta_lambda, k_mu, theta_mu, "1.0 1.0 0 1.0 3.0 0.1 0 1.0 3.0 0.1 false 1 0 9999 500", particles, sweeps)
  if (run) {
      system(paste("./CRBD-Demo_Anatinae_0.8709677419354839_1.0_1.0_1.0_0.5_1.0_1.0_0_1.0_3.0_0.1_0_1.0_3.0_0.1_false_1_0_9999_500_50000_10_.out", particles, sweeps))  
  }
  dir = paste0(paste("CRBD-Demo_Anatinae_0.8709677419354839_1.0_1.0_1.0_0.5_1.0_1.0_0_1.0_3.0_0.1_0_1.0_3.0_0.1_false_1_0_9999_500", particles, sweeps, sep = "_"), "_")
  list.files(dir)
  data = read.csv(file.path(dir, "crbd_demo.csv"), header = FALSE)
  N = nrow(data)
  transformed_data = data.frame(value = c(data[,1], data[,2]), var = c(rep("lambda", N), rep("mu", N)))
  setwd(oldwd)
  return(transformed_data)
}
  