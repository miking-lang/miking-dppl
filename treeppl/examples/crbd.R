# Imaginary R syntax to read to invoke TreePPL form R

tree = phyjson.read(filename="some PhyJSON file")
k_lambda = 1
theta_lambda = 0.2

ppl = readLines(file = "crbd.tppl")

result = treeppl(ppl, input = list(k_lambda, k_theta, k_mu, theta_mu, tree), method = "SMC", particles = "10000" )

result$lambda
result$mu
result$normconst

plot(result$lambda)


