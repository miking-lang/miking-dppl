mexpr
let rain = assume (Bernoulli 0.2) in
let sprinkler =
  if rain then assume (Bernoulli 0.01)
          else assume (Bernoulli 0.4) in
let grassWet = true in
observe grassWet (
  switch (sprinkler, rain)
  case (false, false) then Bernoulli 0.0
  case (false, true) then Bernoulli 0.8
  case (true, false) then Bernoulli 0.9
  case (true, true) then Bernoulli 0.99
  end
);
rain
