# samoYEET

## Overview

Samll interpreter written in `haskell` for one of the university courses. The language presented here is based mostly on the `Latte language` available [here](https://www.mimuw.edu.pl/~ben/Zajecia/Mrj2019/Latte/). 

The interpreter's name orinates from a beautiful breed of dogs (Samoyed) and a [yeet](https://www.dictionary.com/e/slang/yeet/) keyword which is used instead of a standard print function (which consequently, preserves 1 character per one print invocation, pretty neat, huh?).

## Motivation

Em, that was kinda obligatory, however, aside from that I would recommend writing an interpreter to people who want to get into functional programming. It can introduce the concept of functional programming and monads pretty well, this interpreter has a lot of bloatware, though, so I recommend trying a smaller one with just basics.

## Grammar

I used BNFC converter to generate grammar, though, a [specific version](https://github.com/mbenke/bnfc.git) is needed so that `functor` keyword would work properly.

## Main features
- c with python like syntax (we like types here, sorry)
- variables
- comparisons, arithmetic (based on C’s with convenient string add operator)
- static binding
- printing to the user and on the stderr if there is any error (runtime/type)
- while loop (with loop control features like break and continue)
- there are recursive functions with two possible ways to pass variables (reference/value)
- runtime error handling
- functions can return basic types as well as functions, they cannot take reference to
another function, though (it does not make sense anyway)
- there is a static typecheck phase before interpreting a program
- functions can exist in another functions on any level of nesting
- closures are implemented in `javascriptlike` way
- anonymous functions aka lambdas
- higher order functions

## Examples
```
int fun(int y) {
    if (y == 1) {
    return 1;
    }
    
  return y * fun(y - 1);
}

yeet fun(10); // 10!
```

```
Fun<int(int)> fun(int t) {
  int g(int y) {
    if (y == 1) {
      return 1;
    }
    
    return (y + t) * g(y - 1);
  }
  
  return g;
}
```

## More examples

So as not to bloat this readme, there are more examples in `examples/good` and `examples/bad`, `good examples` are valid programs that will be interpreted properly, `bad examples` aka `invalid ones` will throw an error with an explanation. All test are written in for of `xx-yy-explanation`, the following list can help you look for a specific example :
- `xx-yy-z` - `xx` means feature, `yy` a specific example of a feature, `z` is a short explanation
- `00-...` - look at the explanation in the name, those are the things that could not be categorized easily
- `01-...` - introduce int, bool, string, types generally
- `02-...` - arithmetic, comparisons
- `03-...` - variables, assignment
- `04-...` - printing to the console (all goes to stdout)
- `05-...` - loops, conditions
- `06-...` - functions, recursive functions
- `07-...` - pass by value and pass by reference
- `09-...` - static binding
- `10-...` - runtime errors handling
- `11-...` - functions on more types
- `12-...` - static type checking
- `13-...` - nested functions definitions
- `16-...` - loop control keywords (break/continue)
- `17-...` - closures, lambdas, higher order functions
