Fast implementation of type inference for simply typed lambda calculus. A lot of the optimizations were based on [smalltt][0].

Here are a few things I tested that were, surprisingly, slightly slower than using `Maybe` in `Infer`:
- hopefully safe custom exceptions like smalltt
- completely unsafe custom exceptions (`raiseIO# () s`)
- continuation-passing style (`(a -> IO Type) -> IO Type`)

[0]: https://github.com/AndrasKovacs/smalltt
