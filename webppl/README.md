# Examples of Safety Issues

## Example 1. Deterministic parameter outside of range

This is a simple example, where we sample from a distribution with a
parameter that is clearly outside of its support.

```
$ webppl safety-1.wppl
Error: Parameter "a" should be of type "real (0, Infinity)".
    at /usr/local/lib/node_modules/webppl/src/dists/base.js:119

118|                     if (type && !type.check(ad.valueRec(params[p]))) {
119|                         throw new Error('Parameter "' + p + '" should be of type "' + type.desc + '".');
     ------------------------------^
120|                     }

    at safety-1.wppl:4

3|     var a = 0
4|     var t = exponential({a: a})
   ------------^
5| }
```

## Example 2. Support of random parameter outside of range

Here, the parameter a is a random variable sampled from a distribution
whose support is partly outside of the range of allowable values. The
execution is sometimes successful, sometimes not.

```
$ webppl safety-2.wppl 
0.36604904245520853
0

17:19 viktsend@stealth
~/Dropbox/Work/miking-dppl/webppl
$ webppl safety-2.wppl 
-0.896078823733028

Error: Parameter "a" should be of type "real (0, Infinity)".
    at /usr/local/lib/node_modules/webppl/src/dists/base.js:119

118|                     if (type && !type.check(ad.valueRec(params[p]))) {
119|                         throw new Error('Parameter "' + p + '" should be of type "' + type.desc + '".');
     ------------------------------^
120|                     }

    at safety-2.wppl:12

11|     
12|     var t = exponential(
    ------------^
13|     {


17:19 viktsend@stealth
~/Dropbox/Work/miking-dppl/webppl
```

## Example 3. Support of random parameter is inside the range but due to rounding we get the occassional problems

Technically, the distribution from which `a` is sampled has support on
(0, 0.5) but sometimes a number so close to 0 is sampled that it is
effectively 0 after rounding.

```
$ webppl safety-3.wppl 
4.078315292499078e-56
4.727285052306297e-125
1.4164235936814247e-219
1.2882297539194267e-231
0

Error: Parameter "a" should be of type "real (0, Infinity)".
    at /usr/local/lib/node_modules/webppl/src/dists/base.js:119

118|                     if (type && !type.check(ad.valueRec(params[p]))) {
119|                         throw new Error('Parameter "' + p + '" should be of type "' + type.desc + '".');
     ------------------------------^
120|                     }

    at safety-3.wppl:17

16|     
17|     var t = exponential(
    ------------^
18|     {


17:48 viktsend@stealth
~/Dropbox/Work/miking-dppl/webppl
```


## TODO Further examples

There is another safety issue, which has to do with variables entering
a space of very low probability and the program spending a lot of time
there. In the ClaDS models, this can happen, if by change the
diversification rate is sampled to be very high, then the program will
begin simulating a lot of side trees, which it then conditions to be
extinct, which is extremely unlikely. What we did is to include
"guards", where if the diversification rate exceeds a threshold, we
yield -Infinity weight.
