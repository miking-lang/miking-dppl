-- -*- compile-command : "mi compile --test runtime-ad.mc --output out && ./out && rm ./out" -*-

include "ext/reflection-ext.mc"

let assertFloat : Float -> Float
  = lam a.
    if isfloat a then a
    else error "Float assertion failed. The assertion likely failed beacuse the derivative of a non-differentiable functions was requested."

let _uc = unsafeCoerce

let _e = ref 0.
let geneps : () -> Float =
  lam. modref _e (addf (deref _e) 1.); deref _e

recursive let discardTangent : Float -> Float
  = lam a.
    if isfloat a then a
    else
      match _uc a with (_, ap, _) in
      discardTangent (_uc ap)
end

let tangent : Float -> Float -> Float
  = lam e. lam a.
    if isfloat a then 0.
    else
      match _uc a with (ea, _, at) in
      if ltf ea e then 0. else _uc at

let dual : Float -> Float -> Float -> Float
  = lam e. lam xp. lam xt.
    _uc (e, xp, xt)

recursive let _addf : Float -> Float -> Float
  = lam a. lam b.
    switch (isfloat a, isfloat b)
    case (true, true) then addf a b
    case (true, _) then
      match _uc b with (e, bp, bt) in
      _uc (e, _addf a (_uc bp), bt)
    case (_, true) then
      match _uc a with (e, ap, at) in
      _uc (e, _addf (_uc ap) b, at)
    case _ then
      match _uc (a, b) with ((ea, ap, at), (eb, bp, bt)) in
      if ltf ea eb then
        _uc (eb, _addf a (_uc bp), bt)
      else
        if ltf eb ea then
          _uc (ea, _addf (_uc ap) b, at)
        else
          _uc (ea, _addf (_uc ap) (_uc bp), _addf (_uc at) (_uc bt))
    end
end

recursive let _mulf : Float -> Float -> Float
  = lam a. lam b.
    switch (isfloat a, isfloat b)
    case (true, true) then mulf a b
    case (true, _) then
      match _uc b with (e, bp, bt) in
      _uc (e, _mulf a (_uc bp), _mulf a (_uc bt))
    case (_, true) then
      match _uc a with (e, ap, at) in
      _uc (e, _mulf (_uc ap) b, _mulf b (_uc at))
    case _ then
      match _uc (a, b) with ((ea, ap, at), (eb, bp, bt)) in
      if ltf ea eb then
        _uc (eb, _mulf a (_uc bp), _mulf a (_uc bt))
      else
        if ltf eb ea then
          _uc (ea, _mulf (_uc ap) b, _mulf (_uc at) b)
        else
          _uc
            (ea,
             _mulf (_uc ap) (_uc bp),
             _addf (_mulf (_uc bp) (_uc at)) (_mulf (_uc ap) (_uc bt)))
    end
end

recursive let _negf : Float -> Float
  = lam a.
    if isfloat a then negf a
    else
      match _uc a with (e, ap, at) in
      _uc (e, _negf (_uc ap), _negf (_uc at))
end

recursive let _subf : Float -> Float -> Float
  = lam a. lam b.
    switch (isfloat a, isfloat b)
    case (true, true) then subf a b
    case (true, _) then
      match _uc b with (e, bp, bt) in
      _uc (e, _subf a (_uc bp), _negf (_uc bt))
    case(_, true) then
      match _uc a with (e, ap, at) in
      _uc (e, _subf (_uc ap) b, at)
    case _ then
      match _uc (a, b) with ((ea, ap, at), (eb, bp, bt)) in
      if ltf ea eb then
        _uc (eb, _subf a (_uc bp), _negf (_uc bt))
      else
        if ltf eb ea then
          _uc (ea, _subf (_uc ap) b, at)
        else
          _uc (ea, _subf (_uc ap) (_uc bp), _subf (_uc at) (_uc bt))
    end
end

recursive let _divf : Float -> Float -> Float
  = lam a. lam b.
    let dfda = lam b. _divf 1. b in
    let dfdb = lam a. lam b. _divf (_negf a) (_mulf b b) in
    switch (isfloat a, isfloat b)
    case (true, true) then divf a b
    case (true, _) then
      match _uc b with (e, bp, bt) in
      _uc (e, _divf a (_uc bp), _mulf (dfdb a (_uc bp)) (_uc bt))
    case (_, true) then
      match _uc a with (e, ap, at) in
      _uc (e, _divf (_uc ap) b, _mulf (dfda b) (_uc at))
    case _ then
      match _uc (a, b) with ((ea, ap, at), (eb, bp, bt)) in
      if ltf ea eb then
        _uc (eb, _divf a (_uc bp), _mulf (dfdb a (_uc bp)) (_uc bt))
      else
        if ltf eb ea then
          _uc (ea, _divf (_uc ap) b, _mulf (dfda b) (_uc at))
        else
          _uc
            (ea,
             _divf (_uc ap) (_uc bp),
             _addf
               (_mulf (dfda (_uc bp)) (_uc at))
               (_mulf (dfdb (_uc ap) (_uc bp)) (_uc bt)))
    end
end

let _eqf : Float -> Float -> Bool
  = lam a. lam b. eqf (discardTangent a) (discardTangent b)

let _neqf : Float -> Float -> Bool
  = lam a. lam b. neqf (discardTangent a) (discardTangent b)

let _ltf : Float -> Float -> Bool
  = lam a. lam b. ltf (discardTangent a) (discardTangent b)

let _leqf : Float -> Float -> Bool
  = lam a. lam b. leqf (discardTangent a) (discardTangent b)

let _gtf : Float -> Float -> Bool
  = lam a. lam b. gtf (discardTangent a) (discardTangent b)

let _geqf : Float -> Float -> Bool
  = lam a. lam b. geqf (discardTangent a) (discardTangent b)

recursive
  let _sin : Float -> Float
    = lam a.
      if isfloat a then sin a
      else
        match _uc a with (e, ap, at) in
        _uc (e, _sin (_uc ap), _mulf (_cos (_uc ap)) (_uc at))

  let _cos : Float -> Float
    = lam a.
      if isfloat a then cos a
      else
        match _uc a with (e, ap, at) in
        _uc (e, _cos (_uc ap), _negf (_mulf (_sin (_uc ap)) (_uc at)))
end

recursive let _exp : Float -> Float
  = lam a.
    if isfloat a then exp a
    else
      match _uc a with (e, ap, at) in
      let ap = _exp (_uc ap) in
      _uc (e, ap, _mulf ap (_uc at))
end

recursive let _log : Float -> Float
  = lam a.
    if isfloat a then log a
    else
      match _uc a with (e, ap, at) in
      _uc (e, _log (_uc ap), _divf (_uc at) (_uc ap))
end

recursive let _sqrt : Float -> Float
  = lam a.
    if isfloat a then sqrt a
    else
      match _uc a with (e, ap, at) in
      _uc (e, _sqrt (_uc ap), _divf (_uc at) (_mulf 2. (_sqrt (_uc ap))))
end

recursive let _powi : Float -> Int -> Float
  = lam a. lam n.
    if isfloat a then pow a (int2float n)
    else
      match _uc a with (e, ap, at) in
      _uc
        (e,
         _powi (_uc ap) n,
         _mulf (_mulf (int2float n) (_powi (_uc ap) (subi n 1))) (_uc at))
end

recursive let _powf : Float -> Float -> Float
  = lam a. lam b.
    let dfda = lam a. lam b. _mulf b (_powf a (_subf b 1.)) in
    let dfdb = lam a. lam b.
      if _eqf a 0. then
        if _gtf b 0. then 0. else divf 0. 0. /- nan -/
      else
        _mulf (_powf a b) (_log a)
    in
    switch (isfloat a, isfloat b)
    case (true, true) then pow a b
    case (true, _) then
      match _uc b with (e, bp, bt) in
      _uc (e, _powf a (_uc bp), _mulf (dfdb a (_uc bp)) (_uc bt))
    case (_, true) then
      match _uc a with (e, ap, at) in
      _uc (e, _powf (_uc ap) b, _mulf (dfda (_uc ap) b) (_uc at))
    case _ then
      match _uc (a, b) with ((ea, ap, at), (eb, bp, bt)) in
      if ltf ea eb then
        _uc (eb, _powf a (_uc bp), _mulf (dfdb a (_uc bp)) (_uc bt))
      else
        if ltf eb ea then
          _uc (ea, _powf (_uc ap) b, _mulf (dfda (_uc ap) b) (_uc at))
        else
          _uc
            (ea,
             _powf (_uc ap) (_uc bp),
             _addf
               (_mulf (dfda (_uc ap) (_uc bp)) (_uc at))
               (_mulf (dfdb (_uc ap) (_uc bp)) (_uc bt)))
    end
end

let _pow : Float -> Float -> Float
  = lam a. lam b.
    if isfloat b then
      let n = ceilfi b in
      if eqi n (floorfi b) then _powi a n
      else _powf a b
    else _powf a b

recursive let float2string_ : Float -> String
  = lam a.
    if isfloat a then float2string a
    else
      match _uc a with (e, ap, at) in
      foldl concat "(" [
        float2string_ (_uc ap), " +e[",
        float2string e, "]", float2string_ (_uc ap), ")"
      ]
end

let addf : Float -> Float -> Float = _addf
let mulf : Float -> Float -> Float = _mulf
let subf : Float -> Float -> Float = _subf
let divf : Float -> Float -> Float = _divf
let negf : Float -> Float = _negf
let eqf : Float -> Float -> Bool = _eqf
let neqf : Float -> Float -> Bool = _neqf
let ltf : Float -> Float -> Bool = _ltf
let leqf : Float -> Float -> Bool = _leqf
let gtf : Float -> Float -> Bool = _gtf
let geqf : Float -> Float -> Bool = _geqf
let sin : Float -> Float = _sin
let cos : Float -> Float = _cos
let exp : Float -> Float = _exp
let log : Float -> Float = _log
let sqrt : Float -> Float = _sqrt
let pow : Float -> Float -> Float = _pow
let float2string : Float -> String = float2string_

let diff : ([Float] -> [Float]) -> [Float] -> [Float] -> [Float]
  = lam f. lam xs. lam dxs.
    let e = geneps () in
    let xs = create (length xs) (lam i. (e, get xs i, get dxs i)) in
    map (_uc tangent e) (f (_uc xs))

let diff1 : (Float -> Float) -> Float -> Float = lam f. lam x.
  get (diff (lam x. [f (get x 0)]) [x] [1.]) 0

mexpr ()
