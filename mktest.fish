#!/usr/bin/env fish

echo running transformation
build/temp > build/model.mc
and echo transformation complete, fixing stuff
and sed \
  -e '/^ *external /{s/^.*$//;N;/^\n *in$/d;D}' \
  -e "s/\(Bernoulli\|Exponential\|Gaussian\|Gamma\|Poisson\|Uniform\|Dirichlet\|Categorical\)/mk\1/g" \
  build/model.mc > build/transformed-model.mc
and sed -e "/{{HERE}}/{r build/transformed-model.mc" -e "d}" \
  test-template.mc > build/full-temp.mc
and echo fixing complete, compiling model
and mi compile build/full-temp.mc --output build/full-temp
and echo compilation complete, running
and time build/full-temp
and echo running complete, done
