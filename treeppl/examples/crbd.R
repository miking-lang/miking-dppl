# Imaginary R syntax to read to invoke TreePPL form R

tree = phyjson.read(filename="some PhyJSON file")
k_lambda = 1
theta_lambda = 0.2

k_mu = 1
theta_mu = 0.1

ppl = readLines(file = "crbd.tppl")

result = treeppl(ppl, input = list(k_lambda, theta_lambda, k_mu, theta_mu, tree), method = "SMC", particles = "10000" )

result$lambda
result$mu
result$normconst

plot(result$lambda)


