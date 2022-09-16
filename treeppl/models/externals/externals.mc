mexpr

external externalExp : Float -> Float
in
external externalLog : Float -> Float
in
let p =
  0.5
in
recursive
  let foobar =
    lam p: Float.
      let e =
        assume
          (Bernoulli
             p)
      in
      let ifCont =
        lam #var"": Int.
          e
      in
      match
        e
      with
        true
      then
        let foo =
          weight
            (externalLog
               2.)
        in
        ifCont
          0
      else
        ifCont
          0
in
foobar
  p
