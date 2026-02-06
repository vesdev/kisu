<p align="center">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="./logo/dark.svg">
    <img
      src="./logo/light.svg"
      height="240px"
    >
  </picture>
</p>

<p align="center">
Lazy purely functional configuration language
</p>

Current implementation is dynamically typed and turing complete.
The plan is to add opt-in turing completeness by feature-gating general recursion through a typesystem.
This will give termination safety for configuration usage while still allowing general purpose programming.

Current syntax example:
```haskell
fib = |fib, n|:
    if n == 0 then 0
    else if n == 1 then 1
    else (fib fib n - 1) + (fib fib n - 2);
    
fib fib 5
```

