import treeppl

crbd_code = """\
import tree.utils

//! inference=smc repeat=20 particles=10000 samples=5
function model(observation: Tree): Real, Real {
    // ... code ...
    //! resample
    return lambda, mu
}
"""

crbd = treeppl.loads(crbd_code)
# crbd = treeppl.load("crbd.treeppl")

observation = crbd.load_tree("somefile.nex")

inference_result = treeppl.infer(crbd.model, observation)
# or maybe just: inference_result = crbd.model(observation)

# Overriding inference hints:
# inference_result = treeppl.infer(crbd.model, observation, samples=5000, inference='mcmc')

# inference_result.store("...")

log_z = inference_result.marginal_loglikelihood()

def some_test_function(l, m):
    return l

mean_lambda = inference_result.expected_value(some_test_function)
weighted_samples = inference_result.weighted_samples()
posterior_distribution_lambda = inference_result.distribution(lambda l, m: l)

