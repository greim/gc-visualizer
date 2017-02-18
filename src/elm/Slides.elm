-- module ----------------------------------------------------------------------

module Slides exposing (getSlide, slideNames)

-- import ----------------------------------------------------------------------

import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown

-- functions/values ------------------------------------------------------------

slides : List (Html msg)
slides =
  [
  Markdown.toHtml [class "center"]
  """
# *~ Nothing Dot Foo ~*<hr>Garbage Collection<br><big>Visualized</big>
"""
  , Markdown.toHtml []
  """
# What is Garbage Collection?

Garbage collection is how JavaScript gets rid of stuff you're not using. The way it knows whether to get rid of something is by using an algorithm.
"""
  , Markdown.toHtml []
  """
# The Algorithm...

A value is retained in memory if it's reachable from another value that's retained in memory.

But what do we mean by **reachable**?
"""
  , Markdown.toHtml []
  """
# Variable Environments

 * Every time a function executes, one is created.
 * Main program execution also creates a global one.
 * They contain variables local to that function call.
 * They're invisible; you can't get a reference to one.
 * Except global one; referenced as `window` or `global`.
 * They affect garbage collection.
"""
  , Markdown.toHtml []
  """
# The Complete Algorithm.

A value is retained in memory if it's reachable from another value that's retained in memory, or if it's reachable from a garbage collection **root**.
"""
  , Markdown.toHtml []
  """
# Mark & Sweep

 1. Your program pauses.
 2. Garbage collector marks every object on heap.
 3. Finds everything reachable from roots and un-marks it.
 4. Sweeps away anything still marked.
 5. Your program resumes.
"""
  , Markdown.toHtml []
  """
**Disclaimer:** Real-world garbage collectors are the topic of academic research and high-end VM optimization. The overall approach is similar to what's described here, but different strategies are explored for doing it incrementally and/or in a separate thread in order minimize pauses and keep the program running as smoothly as possible.
"""
  ]

slideNames : List (String, Int)
slideNames =
  let
    len = List.length slides
    range = List.range 0 (len - 1)
    names = List.map (\idx -> (toString (idx + 1), idx)) range
  in
    names

getSlide : Int -> Maybe (Html msg)
getSlide idx =
  getSlide_ idx slides

getSlide_ : Int -> List (Html msg) -> Maybe (Html msg)
getSlide_ idx slides =
  case slides of
    slide :: rest ->
      if idx == 0 then Just slide
      else getSlide_ (idx - 1) rest
    [] ->
      Nothing
