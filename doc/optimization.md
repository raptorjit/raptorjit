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
   -- Spend time in the garbage collector? That's a bug too - stop it!
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

If your application is spending significant time in the interpreter
then you need to look for _blacklisted_ code.

Code is blacklisted when repeated attempts at JIT compilation all fail.

1. Identify the blacklisted code that you think is most likely to be responsible. This is probably code that is frequently called by the application. (Blacklisted code that is rarely called is unlikely to be a problem.)
2. Look at the _trace aborts_ that made attempts to JIT this code fail. On which bytecode did they abort, and for what reason? The most common reason is that execution reached an [NYI bytecode](http://wiki.luajit.org/NYI) that the JIT cannot compile, for example creating a new closure (`FNEW`), calling a function that takes a variable number of arguments (`VARG`), calling the `print()` built-in function, etc.
3. Rewrite the code to avoid the aborts. The new version should successfully compile. If it does not then repeat this process with the new code.

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

_This will be the really fun part! Understand what JIT code looks like, how to make it more efficient, and what tricks we can use that C/C++ hackers with static compilers can't :-)_

