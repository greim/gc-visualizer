-- module ----------------------------------------------------------------------

module Slides exposing
  ( Slide(..)
  , CharEdit(..)
  , getSlide
  , length
  )

-- import ----------------------------------------------------------------------

import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown
import Bulk
import Node
--import Graph

-- types -----------------------------------------------------------------------

type CharEdit
  = CharInsert Char
  | CharInsertRight Char
  | CharInserts (List Char)
  | CharInsertsRight (List Char)
  | CharHome
  | CharEnd
  | CharMoveBy Int
  | CharSeek String
  | CharSeekRight String

type StringEdit
  = StringInsert String
  | StringInsertRight String
  | StringInserts String
  | StringInsertsRight String
  | StringHome
  | StringEnd
  | StringMoveBy Int
  | StringSeek String
  | StringSeekRight String

type Slide msg
  = Content (Html msg)
  | DemoTime (Maybe String) (Maybe Node.MemGraph)
  | ContinueDemo
  | Edits (List CharEdit)

-- functions/values ------------------------------------------------------------

slide : String -> String -> Slide msg
slide cls md =
  Content (Markdown.toHtml [class cls] md)

slides : List (Slide msg)
slides =
  [ slide "center" """
# *~ Nothing Dot Foo ~*<hr>Garbage Collection<br><big>Visualized</big>
"""


  , slide "" """
# What is Garbage Collection?

Garbage collection is an automatic memory management strategy in which the language runtime continually analyzes the state of the memory heap and de-allocates objects if and when it can guarantee they’re no longer in use by your program.
"""


  , slide "center" """
# But how does it<br><big>Work?</big>
"""
  , DemoTime (Just """var foo = {
  bar: {
    baz: "hello",
  },
};

<== you are here
""") (Just Bulk.fooBarBaz)


  , slide "center" """
# There's Nothing<br>Dot Foo!
"""


  , ContinueDemo


  , slide "center" """
# Variable Environments:

Objects that contain variables local to a function-run.
"""


  , slide "" """
# Variable Environments

* Created every time a function runs
* Invisible
* Interact by creating, reading and updating vars
* Affect garbage collection
"""


  , DemoTime (Just """function logNum(x) {
  console.log(x);
}

logNum(1);
logNum(2);
logNum(3);

<== you are here
""") (Just Bulk.multipleVarEnv)


  , slide "" """
# The Global Environment

* Has loop-back ref called `window`
* Contains global variables
* Receives undeclared assignments
* Is otherwise just another variable environment
"""


  , slide "" """
# Garbage Collection Roots

The GC roots include the global variable environment, plus whichever variable environment you happen to be running in at the moment.
"""


  , DemoTime (Nothing) (Just Bulk.globalGraph)


  , slide "center" """
# Closures, The Scope Chain<br>and Garbage Collection
"""


  , slide "center" """
# When a function is allocated, it receives a reference back to the variable environment where it was created.
"""


  , DemoTime (Just """(function() {
  // ...
});

<== you are here
""") (Just Bulk.scopeChain1)


  , slide "center" """
# When a function runs, a variable environment is created which receives a reference to the function’s parent var env.
"""


  , DemoTime (Just """(function() {
  (function() {
    var x = 1;
    (function() {
      <== you are here
    })();
  })();
})();
""") (Just Bulk.scopeChain2)


  , DemoTime (Just "<== you are here") (Just Bulk.justGlobal)


  , Edits (createCharEdits
    [ StringHome
    , StringInsertsRight "\n\n"
    , StringInsert "new Set();"
    ])

  , Edits (createCharEdits
    [ StringHome
    , StringInsertRight " "
    , StringInsert "var items ="
    , StringSeek "\n"
    ])

  , Edits (createCharEdits
    [ StringInserts "\n\n"
    , StringInsert "(() => {});"
    ])

  , Edits (createCharEdits
    [ StringMoveBy -1
    , StringInsert "()"
    ])

  , Edits (createCharEdits
    [ StringMoveBy -4
    , StringInsertRight "\n"
    , StringInserts "\n  "
    , StringInsert "var foo = {...};"
    ])

  , Edits (createCharEdits
    [ StringInserts "\n  "
    , StringInsert "(() => {})();"
    ])

  , Edits (createCharEdits
    [ StringMoveBy -5
    , StringInsertsRight "\n  "
    , StringInserts "\n    "
    , StringInsert "var bar = [...];"
    ])

  , Edits (createCharEdits
    [ StringInserts "\n    "
    , StringInsert "(() => {})();"
    ])

  , Edits (createCharEdits
    [ StringMoveBy -5
    , StringInsertsRight "\n    "
    , StringInserts "\n      "
    , StringInsert "items.add({});"
    ])

  , Edits (createCharEdits
    [ StringInserts "\n      "
    , StringInsert "items.push(() => {});"
    ])

  , Edits (createCharEdits
    [ StringMoveBy -3
    , StringInsertsRight "\n      "
    , StringInserts "\n        "
    , StringInsert "// ......"
    ])


  , slide "" """
# Weak References

ES2015 introduced `WeakMap` and `WeakSet` data structures which *weakly reference* their contents.
"""


  , ContinueDemo


  , slide "" """
# WeakMap & WeakSet

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
<pre class="hilights"><em>const INSTANCES = new WeakSet();</em>

class Foo {
  constructor() {
    <em>INSTANCES.add(this);</em>
  }
  doStuff() {
    if (<em>!INSTANCES.has(this)</em>) {
      throw new TypeError('not an instance!');
    }
  }
}
</pre>
"""


  , slide "" """
# DOM Node Tracking
<pre class="hilights"><em>const CLICKED = new WeakSet();</em>

$(document.body).on('click', 'button', ev => {
  if (<em>CLICKED.has(this)</em>) {
    alert('already clicked this button!');
  } else {
    <em>CLICKED.add(this);</em>
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
<pre class="hilights"><em>const DATA = new WeakMap();</em>

class User {
  constructor({ firstName, lastName }) {
    <em>DATA.set(this, { firstName, lastName });</em>
  }
  fullName() {
    <em>const { firstName, lastName } = DATA.get(this);</em>
    return `${firstName} ${lastName}`;
  }
}
</pre>
"""


  , slide "" """
# Annotating DOM Nodes
<pre class="hilights"><em>const CLICK_TIMES = new WeakMap();</em>

$(document.body).on('click', 'button', ev => {
  <em>CLICK_TIMES.set(this, Date.now());</em>
});
</pre>
"""


  , slide "" """
# Memoization
<pre class="hilights"><em>const CACHE = new WeakMap();</em>

function findOutliers(series) {
  if (<em>!CACHE.has(series)</em>) {
    const outliers = analyze(series);
    <em>CACHE.set(series, outliers);</em>
  }
  <em>return CACHE.get(series);</em>
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

createCharEdits : List StringEdit -> List CharEdit
createCharEdits stringEdits =
  stringEdits
    |> List.map (\strEdit -> createCharEditsSingle strEdit [])
    |> List.concat

createCharEditsSingle : StringEdit -> List CharEdit -> List CharEdit
createCharEditsSingle strEdit results =
  case strEdit of
    StringHome -> [CharHome]
    StringEnd -> [CharEnd]
    StringMoveBy amount -> [CharMoveBy amount]
    StringSeek str -> [CharSeek str]
    StringSeekRight str -> [CharSeekRight str]
    StringInserts str -> [CharInserts (String.toList str)]
    StringInsertsRight str -> [CharInsertsRight (List.reverse (String.toList str))]
    StringInsert str ->
      String.toList str
        |> List.map (\ch -> CharInsert ch)
    StringInsertRight str ->
      String.toList str
        |> List.reverse
        |> List.map (\ch -> CharInsertRight ch)
