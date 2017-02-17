-- module ----------------------------------------------------------------------

module Node exposing
  ( Node
  , MemGraph
  , Marking(..)
  , Designation(..)
  , Ref(..)
  , logGraph
  )

-- import ----------------------------------------------------------------------

import Debug exposing (log)
import Graph exposing (Graph)

-- types -----------------------------------------------------------------------

type alias Node =
  { x : Int
  , y : Int
  , designation : Designation
  , isSelected : Bool
  , mark : Marking
  , label : String
  }

type Marking = Unmarked | Marked | None

type Designation = Reachable | Root | Normal

type Ref = Strong | Weak

type alias MemGraph = Graph Node Ref

-- functions/values ------------------------------------------------------------

logGraph : MemGraph -> Int
logGraph graph =
  let
    nodes = Graph.toNodeList graph
    edges = Graph.toEdgeList graph
    loggedNodes = logNodes nodes
    loggedEdges = logEdges edges
  in
    0

logNodes : List (Int, Node) -> Int
logNodes nodes =
  case nodes of
    thing :: rest ->
      let foo = log "(id, node)" thing
      in logNodes rest
    [] ->
      0

logEdges : List (Int, Ref, Int) -> Int
logEdges edges =
  case edges of
    thing :: rest ->
      let foo = log "(fromId, ref, toId)" thing
      in logEdges rest
    [] ->
      0
