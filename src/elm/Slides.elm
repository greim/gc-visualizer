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

-- types -----------------------------------------------------------------------

type Slide msg
  = Content (Html msg)
  | DemoTime

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

Garbage collection is how JavaScript gets rid of stuff you're not using. It knows whether to get rid of something is because it uses a trusty algorithm.
"""
  , slide "" """
# The Incomplete Algorithm

A value is retained in memory if it's reachable from another value that's retained in memory.
"""
  , DemoTime
  , slide "" """
# Variable Environments:

 * They're objects that hold your variables.
 * They're created every time a function runs.
 * They're invisible; you can't get a reference to one.
 * They're significant WRT garbage collection.
"""
  , DemoTime
  , slide "" """
# The Complete Algorithm.

A value is retained in memory if it's reachable from another value that's retained in memory, or if it's reachable from a garbage collection **root**.
"""
  , slide "" """
# Mark & Sweep

 1. Your program pauses.
 2. Garbage collector marks every object on heap.
 3. Finds everything reachable from roots and un-marks it.
 4. Sweeps away anything still marked.
 5. Your program resumes.
"""
  , slide "" """
**Disclaimer:** Real-world garbage collectors are the topic of academic research and high-end VM optimization. The overall approach is similar to what's described here, but different strategies are explored for doing it incrementally and/or in a separate thread in order minimize pauses and keep the program running as smoothly as possible.
"""
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
