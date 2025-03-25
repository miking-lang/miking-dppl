using DifferentialEquations
using Plots
function f(u, p, t, W)
    sin(W) - u
end
u0 = 0.00
tspan = (0.0, 5.0)
prob = RODEProblem(f, u0, tspan)
N = 20
p = plot(legend=false)
for i in 1:N
    sol = solve(prob, RandomEM(), dt = 1 / 100)
    plot!(p, sol)
end
display(p)
