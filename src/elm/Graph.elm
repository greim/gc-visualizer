module Graph exposing (Graph, empty, singleton, addNode, updateNode, getNode, removeNode, toNodeList, toEdgeList, addEdge, removeEdge, findConnected)
import Dict exposing (Dict)
import Set exposing (Set)
import Queue exposing (Queue)

type alias Graph a =
  { nodes : Dict Int a
  , edges : Dict Int (Set Int)
  , nextId : Int
  }

empty : Graph a
empty =
  Graph (Dict.empty) (Dict.empty) 1


singleton : a -> Graph a
singleton a =
  Graph (Dict.singleton 0 a) (Dict.empty) 1


addNode : a -> Graph a -> (Int, Graph a)
addNode a graph =
  let
    id = graph.nextId
    newNodes = Dict.insert id a graph.nodes
    newGraph = { graph | nodes = newNodes, nextId = id + 1 }
  in
    (id, newGraph)


updateNode : Int -> a -> Graph a -> Graph a
updateNode id node graph =
  case Dict.get id graph.nodes of
    Just oldNode ->
      let
        newNodes = Dict.insert id node graph.nodes
      in
        { graph | nodes = newNodes }
    Nothing ->
      graph


getNode : Int -> Graph a -> Maybe a
getNode id graph =
  Dict.get id graph.nodes


removeNode : Int -> Graph a -> Graph a
removeNode id graph =
  let
    newNodes = Dict.remove id graph.nodes
    filterFn = (\anId -> id /= anId)
    mapFn = (\from tos -> Set.filter filterFn tos)
    truncEdges = Dict.remove id graph.edges
    newEdges = Dict.map mapFn truncEdges
  in
    { graph | nodes = newNodes, edges = newEdges }


toNodeList : Graph a -> List (Int, a)
toNodeList graph =
  Dict.toList graph.nodes


toPairs : Int -> Set Int -> List (Int, Int)
toPairs from tos =
  let
    tosList = Set.toList tos
    mapFn = (\to -> (from, to))
  in
    List.map mapFn tosList


toEdgeList : Graph a -> List (Int, Int)
toEdgeList graph =
  let
    rawList = Dict.toList graph.edges
    foldFn = (\(from, tos) pairs -> List.append pairs (toPairs from tos))
  in
    List.foldl foldFn [] rawList


addEdge : Int -> Int -> Graph a -> Graph a
addEdge from to graph =
  case (Dict.get from graph.nodes, Dict.get to graph.nodes) of
    (Just f, Just t) ->
      let
        newTos = case Dict.get from graph.edges of
          Just tos -> Set.insert to tos
          Nothing -> Set.singleton to
        newEdges = Dict.insert from newTos graph.edges
      in
        { graph | edges = newEdges }
    _ ->
      graph


removeEdge : Int -> Int -> Graph a -> Graph a
removeEdge from to graph =
  case Dict.get from graph.edges of
    Just tos ->
      let
        newTos = Set.remove to tos
        newEdges = Dict.insert from newTos graph.edges
      in
        { graph | edges = newEdges }
    Nothing ->
      graph


findConnected : Int -> Graph a -> Set Int
findConnected id graph =
  let
    queue = Queue.singleton id
    results = Set.singleton id
  in
    findConnectedUgly queue results graph


findConnectedUgly : Queue Int -> Set Int -> Graph a -> Set Int
findConnectedUgly queue results graph =
  case Queue.deq queue of
    (Just id, smallerQueue) ->
      let
        edgeIds = case Dict.get id graph.edges of
          Nothing -> Set.empty
          Just t -> t
        unvisitedIds = Set.diff edgeIds results
        newResults = Set.union results unvisitedIds
        listTos = Set.toList unvisitedIds
        newQueue = Queue.enqAll listTos smallerQueue
      in
        findConnectedUgly newQueue newResults graph
    (Nothing, sameQueue) ->
      results
