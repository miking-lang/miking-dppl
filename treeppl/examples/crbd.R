#library(treeppl)
source("libtreeppl.R")

#tree = treeppl::readphyjson(file = "some phyjson file")
program = "crbd-demo"
tree = "Anatinae"
rho = 0.8709677419354839
k_lambda = 1.0
theta_lambda = 1.0
k_mu = 1.0
theta_mu = 0.5

result = treeppl(program = program,
                 tree = tree,
                 rho = rho,
                 k_lambda = k_lambda,
                 theta_lambda = theta_lambda,
                 k_mu = k_mu,
                 theta_mu = theta_mu,
                 particles = 50000,
                 sweeps = 100,
                 run = FALSE)

library(ggplot2)
ggplot(data = result, aes(x = value, col = var)) + geom_density()
