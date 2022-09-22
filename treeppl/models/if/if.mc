mexpr
external externalExp : Float -> Float
in
external externalLog : Float -> Float
in
let p =
  0.5
in
recursive
  let iftest =
    lam p: Float.
      let test1 =
        assume
          (Bernoulli
             p)
      in
      let test2 =
        assume
          (Bernoulli
             p)
      in
      let ifCont =
        lam #var"": Int.
          let case4 =
            assume
              (Bernoulli
                 p)
          in
          case4
      in
      match
        test1
      with
        true
      then
        let ifCont1 =
          lam #var"": Int.
            ifCont
              0
        in
        match
          test2
        with
          true
        then
          let case1 =
            assume
              (Bernoulli
                 p)
          in
          case1
        else
          let case2 =
            assume
              (Bernoulli
                 p)
          in
          ifCont1
            0
      else
        let case3 =
          assume
            (Bernoulli
               p)
        in
        ifCont
          0
in
iftest
  p
