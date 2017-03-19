-- module ----------------------------------------------------------------------

module Slides exposing
  ( Slide(..)
  , getSlide
  , length
  )

-- import ----------------------------------------------------------------------

import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown
import Bulk
import Node
import Graph

-- types -----------------------------------------------------------------------

type Slide msg
  = Content (Html msg)
  | DemoTime (Maybe String) (Maybe Node.MemGraph)

-- functions/values ------------------------------------------------------------

slide : String -> String -> Slide msg
slide cls md =
  Content (Markdown.toHtml [class cls] md)

slides : List (Slide msg)
slides =
  [
  slide "center" """
# *~ Nothing Dot Foo ~*<hr>Garbage Collection<br><big>Visualized</big>
"""
  , slide "" """
# What is Garbage Collection?

Garbage collection is the process where the runtime automatically frees unused memory, instead of requiring the programmer to manually deallocate memory.
"""
  , slide "" """
<div class="big-img"><img src="/static/img/thinking.png" alt=""></div>
"""
  , slide "" """
# Garbage Collection

 * You don't control when it runs
 * Your program pauses while it runs, causing jank
 * Building a better GC is the topic of ongoing research
 * Advanced implementations are complex
   - Incremental strategies to minimize jank
   - Run GC off main thread and/or in multiple threads
 * Despite the above, semantics are simple!
"""
  , slide "" """
# How does it Know What's Garbage and What Isn't?

It uses an algorithm called *mark and sweep*, which is based on the concept of *reachability*.
"""
  , slide "" """
# Reachability: `foo => bar`

Reachability means `foo` has a reference to `bar`, such that if you have `foo`, you can get `bar`. An example of this is object property access, as in: `foo.bar`

If something is reachable, it's (potentially) in use.

In mark and sweep, objects are retained in memory that are reachable from other objects that are retained in memory.
"""
  , slide "" """
<div class="big-img"><img src="/static/img/chicken-or-egg.png" alt=""></div>
"""
  , slide "center" """
# (Disregard the infinite regress for now; it just means there's a base case somewhere.)
"""
  , DemoTime (Just """var foo = {
  bar: {
    baz: "hello",
  },
};
""") (Just Bulk.fooBarBaz)
  , slide "center" """
# Variable<br>Environments
"""
  , slide "" """
# Variable Environments

Objects containing variables local to a function-run.
"""
  , slide "" """
# Variable Environments

* Not directly referenceable from code; used by runtime
* `var foo = false` creates an entry in a var env
* A new var env is created every time a function runs
* **They're 1st class citizens in the reachability graph**
"""
  , DemoTime (Just """function logNum(x) {
  console.log(x);
}

logNum(1);
logNum(2);
logNum(3);
logNum(4);
logNum(5);
logNum(6);
""") (Just Bulk.multipleVarEnv)
  , slide "" """
# The Global Environment

* Has a loop-back ref called `window` or `global`
* ...which is why you can reference it directly
* ...and why `window.window.window` works
* Receives undeclared assignments: `data = {}`
* Is otherwise just another variable environment
* It's as if entire program wrapped in giant IIFE
"""
  , slide "" """
# Mark & Sweep Base Case

A value is retained in memory if it's reachable from another value that's retained in memory, **or from a garbage collection root**.
"""
  , slide "" """
# Mark & Sweep <small>(In a Nutshell)</small>

 1. Your program pauses
 2. Mark: GC performs graph traversal, starting at roots and flagging everything it finds
 4. Sweep: Scans entire heap, de-allocates everything un-flagged, resets flags
 5. Your program resumes
"""
  , slide "" """
# What are the GC Roots?

The GC roots include the global variable environment, plus whichever variable environment you happen to be running in at the moment. (Plus a few other things.)
"""
  , DemoTime (Nothing) (Just Bulk.globalGraph)
  , slide "center" """
# <big>Scope Chaining</big><br>And Closures and Stuff
"""
  , slide "" """
# Closure

When a function object is allocated, it receives a reference back to the variable environment where it was created.
"""
  , DemoTime (Just """function fn() {
  // ...
}
""") (Just Bulk.scopeChain1)
  , slide "" """
# The Scope Chain

When a function runs, a variable environment is created, which receives a reference to the function's parent var env.

Thus, a chain is built, with arrows pointing backwards from child to parent.

This is how code in a deeply-nested functions "sees" variables in higher scopes.
"""
  , DemoTime (Just """(function() {
  (function() {
    var x = 1;
    (function() {
      var y = 2;
      (function() {
        <== you are here
      })();
    })();
  })();
})();
""") (Just Bulk.scopeChain2)
  , slide "" """
# Closure & Reachability

In this sense, closure is similar to the concept of reachability. Both operate via the same mechanism, both describe what variables your code can see, and both inform what the GC retains in memory.
"""
  , DemoTime (Just "") (Just Bulk.justGlobal)
  , slide "" """
# Closure & Reachability

In this sense, closure is similar to the concept of reachability. Both operate via the same mechanism, and both describe what variables your code can see, and both inform what the GC retains in memory.
"""
  , DemoTime Nothing Nothing
  , slide "center" """
# <big>#!/its/over</big>
"""
  ]

length : Int
length =
  List.length slides

getSlide : Int -> Maybe (Slide msg)
getSlide idx =
  getSlide_ idx slides

getSlide_ : Int -> List (Slide msg) -> Maybe (Slide msg)
getSlide_ idx slides =
  case slides of
    slide :: rest ->
      if idx == 0 then Just slide
      else getSlide_ (idx - 1) rest
    [] ->
      Nothing
