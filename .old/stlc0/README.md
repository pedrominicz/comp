### Goal

Understand and write an abstract machine for executing functional programs. Should be close to a virtual machine for an imperative language (e.g. Lua). That is, instead of manipulating the abstract syntax tree, it should execute compiled bytecode.

### Todo

There are three Krivine machines:
- `.CEK-machine-eval.hs`
- `.Krivine-machine-eval.hs`
- `.strict-Krivine-machine-eval.hs`
All of them should work, that is, normalize the input to weak head normal form. To test that, I need a function that reifies closures.

- [x] Add reification
- [x] Compare the three Krivine machines
  The call-by-name Krivine machine does less work to reach a weak head normal form. Both call-by-value machines do the same amount of work.

I probably want to convert terms to administrative normal form (ANF) before implementing primitive operations.

- [x] Revert to basic Krivine abstract machine
- [x] Make it strict
- [x] Read fast curry paper

- [x] Add let expressions

Time for the big change. At the end of this, the abstract machine will use expressions in ANF.

- [x] Add `Syntax` module and type
- [x] Change parser to the new `Syntax` type
  Don't use `ShortByteString` (for now at least)
- [x] Add ANF normalizer
- [x] Add ANF to nameless ANF conversion
- [ ] Check for unbound variables before ANF conversion

- [x] Parse integers and primitive operations
- [x] A-normalize integers and primitive operations
- [x] Eval integers and primitive operations
  Section 6 of [Deriving a Lazy Abstract Machine](~/tmp/amlazy5.pdf)
- [ ] ZINC abstract machine
- [ ] Add recursive let expressions

### Resources
- [The ZINC experiment: an economical implementation of the ML language](https://hal.inria.fr/inria-00070049/document)
- [Deriving the Full-Reducing Krivine Machine from the Small-Step Operational Semantics of Normal Order](https://oa.upm.es/30153/1/30153nogueiraINVES_MEM_2013.pdf)
- [On the Correctness and Efficiency of the Krivine Machine](http://www.ccs.neu.edu/home/wand/papers/wand-friedman-03.ps)
- [Improving the Lazy Krivine Machine](https://legacy.cs.indiana.edu/ftp/techreports/TR581.pdf)
- [The Essence of Compiling with Continuations](https://www.researchgate.net/publication/2392886_The_Essence_of_Compiling_with_Continuations)
- [Making a fast curry: push/enter vs. eval/apply for higher-order languages](https://www.cs.tufts.edu/comp/150FP/archive/simon-peyton-jones/eval-apply-jfp.pdf)
- [Compiling without Continuations](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/11/join-points-pldi17.pdf)
- [Compiling with Continuations, Continued](https://www.microsoft.com/en-us/research/wp-content/uploads/2007/10/compilingwithcontinuationscontinued.pdf)
- [The Compiler Backend: Bytecode and Native code](https://dev.realworldocaml.org/compiler-backend.html)
- [Memory Representation of Values](https://dev.realworldocaml.org/runtime-memory-layout.html)
- [Haskell Benchmarking, Profiling, and Optimization](https://www.youtube.com/watch?v=vR_5F-kRIlY)
