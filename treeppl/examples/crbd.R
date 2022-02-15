# Imaginary R syntax to read to invoke TreePPL form R

library(treeppl)

# INPUT
tree = treeppl.readphyjson(filename="some phyjson file")
k_lambda = 1
theta_lambda = 0.2

ppl = readLines(file = "crbd.tppl")
result = treeppl(ppl, input = list(k_lambda, k_theta, k_mu, theta_mu, tree))

# OUTPUT
# result is a list
# lambda and mu are data.frames
# normconst is a numeric
result$lambda$values 
result$lambda$weights
result$mu$values
result$mu$weights
result$normconst

# delayed sampling
result$lambda_k$values
result$lambda_theta$weights
result$mu_k$values
result$mu_theta$weights
result$normconst

# importance sampling
# plotting
