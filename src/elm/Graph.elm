module Graph exposing (Graph, empty, singleton, addNode, updateNode, updateNodeFn, getNode, removeNode, toNodeList, toEdgeList, addEdge, removeEdge, findConnected, map)
import Dict exposing (Dict)
import Set exposing (Set)
import Queue exposing (Queue)

type alias Graph a b =
  { nodes : Dict Int a
  , edges : Dict Int (Dict Int b)
  , nextId : Int
  }

empty : Graph a b
empty =
  Graph (Dict.empty) (Dict.empty) 1


singleton : a -> Graph a b
singleton a =
  Graph (Dict.singleton 0 a) (Dict.empty) 1


addNode : a -> Graph a b -> (Int, Graph a b)
addNode a graph =
  let
    id = graph.nextId
    newNodes = Dict.insert id a graph.nodes
    newGraph = { graph | nodes = newNodes, nextId = id + 1 }
  in
    (id, newGraph)


updateNode : Int -> a -> Graph a b -> Graph a b
updateNode id node graph =
  case Dict.get id graph.nodes of
    Just oldNode ->
      let
        newNodes = Dict.insert id node graph.nodes
      in
        { graph | nodes = newNodes }
    Nothing ->
      graph


updateNodeFn : (a -> a) -> Int -> Graph a b -> Graph a b
updateNodeFn update id graph =
  case getNode id graph of
    Just node ->
      updateNode id (update node) graph
    Nothing ->
      graph


getNode : Int -> Graph a b -> Maybe a
getNode id graph =
  Dict.get id graph.nodes


removeNode : Int -> Graph a b -> Graph a b
removeNode id graph =
  let
    newNodes = Dict.remove id graph.nodes
    filterFn = (\anId -> id /= anId)
    mapFn = (\from tos -> Set.filter filterFn tos)
    truncEdges = Dict.remove id graph.edges
    newEdges = Dict.map mapFn truncEdges
  in
    { graph | nodes = newNodes, edges = newEdges }


toNodeList : Graph a b -> List (Int, a)
toNodeList graph =
  Dict.toList graph.nodes


map : (a -> a) -> Graph a b -> Graph a b
map mapFun graph =
  let
    newMapFun = (\id a -> mapFun a)
    newNodes = Dict.map newMapFun graph.nodes
  in
    { graph | nodes = newNodes }


toPairs : Int -> Set Int -> List (Int, Int)
toPairs from tos =
  let
    tosList = Set.toList tos
    mapFn = (\to -> (from, to))
  in
    List.map mapFn tosList


toEdgeList : Graph a b -> List (Int, Int, b)
toEdgeList graph =
  let
    rawList = Dict.toList graph.edges
    foldFn = (\(from, tos) pairs -> List.append pairs (toPairs from tos))
  in
    List.foldl foldFn [] rawList


addEdge : Int -> Int -> b -> Graph a b -> Graph a b
addEdge from to val graph =
  case (Dict.get from graph.nodes, Dict.get to graph.nodes) of
    (Just f, Just t) ->
      let
        newTos = case Dict.get from graph.edges of
          Just tos -> Dict.insert to val tos
          Nothing -> Dict.singleton to val
        newEdges = Dict.insert from newTos graph.edges
      in
        { graph | edges = newEdges }
    _ ->
      graph


removeEdge : Int -> Int -> Graph a b -> Graph a b
removeEdge from to graph =
  case Dict.get from graph.edges of
    Just tos ->
      let
        newTos = Dict.remove to tos
        newEdges = Dict.insert from newTos graph.edges
      in
        { graph | edges = newEdges }
    Nothing ->
      graph


findConnected : Int -> Graph a b -> Set Int
findConnected id graph =
  let
    queue = Queue.singleton id
    results = Set.singleton id
  in
    findConnectedUgly queue results graph


findConnectedUgly : Queue Int -> Set Int -> Graph a b -> Set Int
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
