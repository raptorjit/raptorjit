# RaptorJIT optimization guide

The first step to optimize an application is to characterize its most
significant performance problem. First profile the application with
`vmprofile` and then interpret the results using this step-by-step
method:

```lua
-- RaptorJIT optimization procedure expressed as pseudo-code.
function optimize ()
   -- Spending nontrivial time in the interpreter? That's a bug - stop it!
   if vmprofile.interpreted > 1% then
      eliminate_interpreter()
   -- Spend time in the garbage collector? That's usually a bug too - stop it!
   elseif vmprofile.garbagecollection > 1% then
      eliminate_allocation()
   -- If you are not stuck in interpreter or GC then take a deeper look...
   else
      -- Spend time in JIT code? Check how efficient it is.
      if vmprofile.head + vmprofile.loop > moderate then 
         optimize_jit()
      end
      -- Spend time in the FFI? Check how efficient that is.
      if vmprofile.ffi > moderate then
          optimize_ffi()
      end
   end
end
```

The control flow is significant in this method. For example, if the
application is spending time in the interpreter then _that_ is the
problem. You would need to eliminate this interpreter use before it
makes sense look into other issues like garbage collection (which is
often a secondary effect of running interpreted.)

## Eliminate interpreter mode

If the application is spending time in the interpreter then we need to diagnose why. We can do this by looking for _trace aborts_ in the JIT log to see where the JIT has attempted to compile a trace and why these attempts have failed.

Step-by-step:

1. Identify the relevant aborts.
2. Diagnose their root cause.
3. Rewrite the code to avoid the aborts.

This takes some practice. Trace aborts are not always relevant to performance: if the code is only executed during startup, or is later traced successfully, then the abort may not affect overall performance.

### HOWTO with LuaJIT tools

Consider this example program:

```lua
function f()
   return function () end
end

for i = 1, 1000 do
   f()
end
```

These three features are relevant to the JIT:

1. Line 1 is a potential start of a JIT trace: a function.
2. Line 5 is a potential start of a JIT trace: a loop.
3. Line 2 will cause a JIT abort because it creates a new closure. Closures are created by the `FNEW` bytecode which is NYI.

Here is what happens when we run with `-jv`:

```
$ luajit -jv test.lua
[TRACE --- test.lua:5 -- NYI: bytecode 51 at test.lua:2]
[TRACE --- test.lua:5 -- NYI: bytecode 51 at test.lua:2]
[TRACE --- test.lua:1 -- NYI: bytecode 51 at test.lua:2]
[TRACE --- test.lua:5 -- NYI: bytecode 51 at test.lua:2]
[TRACE --- test.lua:1 -- NYI: bytecode 51 at test.lua:2]
[TRACE --- test.lua:5 -- NYI: bytecode 51 at test.lua:2]
[TRACE --- test.lua:1 -- NYI: bytecode 51 at test.lua:2]
[TRACE --- test.lua:5 -- NYI: bytecode 51 at test.lua:2]
[TRACE --- test.lua:1 -- NYI: bytecode 51 at test.lua:2]
```

This shows the JIT repeatedly attempting to compile a trace, either
from the loop at `test.lua:5` or the function at `test.lua:1`, and
each time it aborts at `test.lua:2` with the reason `NYI: bytecode
51`. So we can conclude that neither the loop nor the function is
being successfully JITed because each one reaches code that cannot be
JITed.

If we run with `-jdump` we can see more detail:

```
$ luajit -jdump test.lua
[TRACE --- test.lua:5 -- NYI: bytecode 51 at test.lua:2]
---- TRACE 1 start test.lua:5
0007  GGET     4   1      ; "f"
0008  CALL     4   1   1
0000  . FUNCF    1          ; test.lua:1
0001  . FNEW     0   0      ; test.lua:2
---- TRACE 1 abort test.lua:2 -- NYI: bytecode 51
...
```

Here we see a more detailed view of the JIT starting a trace from the loop at `test.lua:5` and successfully tracing three bytecodes (`GGET`, `CALL`, FUNCF`) before aborting on the last one (`FNEW`).

So what should we do? The program needs to be rewritten to avoid the trace abort. One solution would be to pre-create a closure and return that instead of creating a new one each time:

```lua
local closure = function () end

function f()
   return closure
end

for i = 1, 1000 do
   f()
end
```

Here is what happens now when we run with `-jv`:

```
[TRACE   1 test2.lua:7 loop]
```

Great: The JIT now successfully created a (looping) trace starting
from the `for` loop that is now at `test2.lua:7`. Problem solved!

### HOWTO with RaptorJIT tools

To be written...

## Eliminate garbage collection

Garbage collection is usually caused by frequent calls to code that has been JIT compiled with _unsunk allocations_. 

1. Examine the JIT IR code for each trace that `vmprofile` identifies as hot.
2. Look for [allocation instructions](http://wiki.luajit.org/SSA-IR-2.0#allocations) that are not sunk.
3. Rewrite the code to avoid the unsunk allocations.

There are various ways that objects can be allocated: tables created
with `{...}` syntax, FFI objects created with `ffi.new()` or
`ffi.string()`, boxed values being created by assigning FFI pointers
or 64-bit numbers to local variables. The JIT will often _sink_ these
allocations by detecting that they are short-lived - never escape from
the trace - and never need to be created in memory. This is great when
it works: sunk allocations are extemely efficient. However, if the JIT
is not able to detect this then the allocation will be unsunk and a
performance liability.

Common ways that unsunk allocations are eliminated:

- Limit the scope of local variables that contain values that need to be boxed. Scope can be limited by defining the variables in small subroutines or inside short `begin ... end` blocks. This can help the JIT to sink the allocations.
- Reuse tables and FFI objects instead of creating new ones.

## Optimize JIT 

Here are some rough ideas for names of potential optimization techniques:

- Stay "on trace"
  - Use consistent FFI types
  - Use branchless operations
  - Hoist unbiased branches outside of loops
- Create the right traces
  - Avoid FFI type explosion
  - Clone specialized single-use prototypes
  - Flush traces after workload change
- Specialize traces to eliminate instructions and break dependency chains
  - Specialize on local variable values in low-instance-count closure
  - Specialize on values in immutable FFI metatype

