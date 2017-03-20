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
  | ContinueDemo

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

Garbage collection is an automatic memory management strategy in which the runtime continually analyzes the state of the memory heap and de-allocates memory if it can guarantee it’s no longer in use by your program.
"""
  , slide "" """
# Garbage Collection is Bad

* Your program pauses, causing jitter and jank
* You don’t control when it runs
* Big heap sizes = big performance penalty
* Building a better GC is the topic of ongoing research
* Advanced implementations are complicated
"""
  , slide "" """
# Garbage Collection is Good

* Decreases language learning curve
* Increases developer productivity; let robots do the boring, repetitive work instead of humans
* Good > bad for most use cases
* Despite the implementation complexity, GC semantics are simple!
"""
  , slide "" """
# Why Learn About GC?

* Know what you can and can’t get away with
* Ability to debug memory leaks
* Maybe someday *you’ll* build a better GC
* You’re a curious person
"""
  , slide "center" """
# How does it Know What’s Garbage and What Isn’t?
"""
  , slide "" """
# How does it Know?

It uses an algorithm called *mark and sweep*, which is based on the concept of *reachability*.
"""
  , slide "" """
# Reachability: `foo => bar`

Reachability means `foo` has a reference to `bar`, such that if you have `foo`, you can get `bar`. An example of this is object property access, as in: `foo.bar`

**Objects are retained in memory if they’re reachable in this manner from your program.**
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
* `bar += baz` reads and modifies entries in a var env
* A new var env is created every time a function runs
* Your code always runs in a var env
* **Despite being implicit, they’re 1st class citizens in the reachability graph**
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
* It’s as if entire program wrapped in giant IIFE
"""
  , slide "" """
# How Mark & Sweep Works

A value is retained in memory if it’s reachable from another value that’s retained in memory, or from a garbage collection root.
"""
  , slide "" """
# Mark & Sweep Sequence

 1. Your program pauses
 2. Mark: GC performs graph traversal, starting at roots and flagging everything it finds
 4. Sweep: GC scans entire heap, de-allocates everything un-flagged, resets flags
 5. Your program resumes
"""
  , slide "" """
# What are the GC Roots?

The GC roots include the global variable environment, plus whichever variable environment you happen to be running in at the moment.
"""
  , DemoTime (Nothing) (Just Bulk.globalGraph)
  , slide "center" """
# <big>Scope Chaining</big><br>And Closures and Stuff
"""
  , slide "" """
# Closure

When a function object is allocated, it receives a reference back to the variable environment where it was created.
"""
  , DemoTime (Just """(function() {
  // ...
});
""") (Just Bulk.scopeChain1)
  , slide "" """
# The Scope Chain

When a function runs, a variable environment is created, which receives a reference to the function’s parent var env.

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
# Weak References

A *weak* reference is a special kind of reference that isn’t followed by the GC. Objects that are only reachable via weak references aren’t retained.

ES6 introduced `WeakMap` and `WeakSet` data structures which use weak references to allow stored objects to be GC’d.
"""
  , ContinueDemo
  , slide "" """
# Weak Data Structures

* `WeakMap` and `WeakSet`

* **Non-introspective:** Iterability or `.size()` would violate GC semantics and introduce indeterminacy.

* **No primitive keys:** For primitives, set membership is determined by value. For objects, by reference.
"""
  , slide "" """
# WeakSet Use Cases

* Instance-of-class enforcement for method functions.
* Storing flags on DOM nodes.
* Generally: private, long-running flagging or membership checks.
"""
  , slide "" """
# Instance Enforcement
<pre>const INSTANCES = new WeakSet();

class Foo {
  constructor() {
    INSTANCES.add(this);
  }
  doStuff() {
    if (!INSTANCES.has(this)) {
      throw new TypeError('not an instance!');
    }
  }
}
</pre>
"""
  , slide "" """
# DOM Node Tracking
<pre>const CLICKED = new WeakSet();

$(document.body).on('click', 'button', ev => {
  if (CLICKED.has(this)) {
    alert('already clicked this button!');
  } else {
    CLICKED.add(this);
  }
});
</pre>
"""
  , slide "" """
# WeakMap Use Cases

* Truly private instance data.
* Storing data on DOM nodes.
* Memoizing on non-primitive inputs.
* Generally: private, long-running data storage or annotations on objects.
"""
  , slide "" """
# Private Instance Data
<pre>const DATA = new WeakMap();

class User {
  constructor({ firstName, lastName }) {
    INSTANCES.set(this, { firstName, lastName });
  }
  fullName() {
    const { firstName, lastName } = DATA.get(this);
    return `${firstName} ${lastName}`;
  }
}
</pre>
"""
  , slide "" """
# Annotating DOM Nodes
<pre>const CLICK_TIMES = new WeakMap();

$(document.body).on('click', 'button', ev => {
  CLICK_TIMES.set(this, Date.now());
});
</pre>
"""
  , slide "" """
# Memoization
<pre>const CACHE = new WeakMap();

function findOutliers(series) {
  if (!CACHE.has(series)) {
    const outliers = analyze(series);
    CACHE.set(series, outliers);
  }
  return CACHE.get(series);
}
</pre>
"""
  , slide "center" """
# <big>fin.</big>
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
