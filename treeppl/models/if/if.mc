include "math.mc"

mexpr

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
      let if-cont =
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
        let if-cont1 =
          lam #var"": Int.
            if-cont
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
          if-cont1
            0
      else
        let case3 =
          assume
            (Bernoulli
               p)
        in
        if-cont
          0
  let flip =
    lam p: Float.
      let e =
        assume
          (Bernoulli
             p)
      in
      e
in
let p =
  0.5
in
iftest
  p


