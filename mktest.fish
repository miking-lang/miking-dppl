#!/usr/bin/env fish

build/temp > build/model.mc
and sed \
  -e "/^ *external /d" \
  -e "s/\(Bernoulli\|Exponential\|Gaussian\|Gamma\|Poisson\|Uniform\)/mk\1/g" \
  build/model.mc > build/transformed-model.mc
and sed -e "/{{HERE}}/{r build/transformed-model.mc" -e "d}" \
  test-template.mc > build/full-temp.mc
and mi compile build/full-temp.mc --output build/full-temp
and time build/full-temp
