import Html exposing (Html, div, button, br)
import Html.Attributes exposing (disabled)
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Keyed as Keyed
import Svg.Attributes exposing (..)
import Graph exposing (Graph)
import Json.Decode as Json
--import Debug exposing (log)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL ############################################################################

type Mode = Move | Add | Delete

type alias Node =
  { x : Int
  , y : Int
  , isRoot : Bool
  }

type alias Viewport =
  { width : Int
  , height : Int
  }

type alias PendingEdge =
  { from : Int
  , x : Int
  , y : Int
  }

type alias Model =
  { nodes : Graph Node
  , viewport : Viewport
  , mode : Mode
  , pendingEdge : Maybe PendingEdge
  , movingNode : Maybe Int
  }

init : (Model, Cmd Msg)
init =
  let
    nodes = Graph.empty
    viewport = Viewport 1200 900
    mode = Add
    pendingEdge = Nothing
    movingNode = Nothing
    model = Model nodes viewport mode pendingEdge movingNode
    cmd = Cmd.none
  in
    (model, cmd)



-- UPDATE ###########################################################################

type Msg
  = Create Int Int
  | TrackPending Int Int
  | StartPending Int Int Int
  | EndPending Int
  | ClearPending
  | ChangeMode Mode
  | StartMoving Int Int Int
  | TrackMoving Int Int
  | EndMoving
  | RemoveNode Int
  | RemoveEdge Int Int
  | Clear
  | NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Create x y ->
      let
        node = Node x y False
        (id, nodes) = Graph.addNode node model.nodes
        noPending = case model.pendingEdge of
          Just p -> False
          Nothing -> True
        pendingEdge = if noPending then Just (PendingEdge id x y) else Nothing
        finalNodes = if noPending then nodes else addPending id model.pendingEdge nodes
        newModel = { model | nodes = finalNodes, pendingEdge = pendingEdge }
      in
        (newModel, Cmd.none)
    TrackPending x y ->
      case model.pendingEdge of
        Just pendingEdge ->
          let
            newPendingEdge = { pendingEdge | x = x, y = y }
            newModel = { model | pendingEdge = Just newPendingEdge }
          in
            (newModel, Cmd.none)
        Nothing ->
          (model, Cmd.none)
    StartPending from x y ->
      let
        pendingEdge = Just (PendingEdge from x y)
        newModel = { model | pendingEdge = pendingEdge }
      in
        (newModel, Cmd.none)
    EndPending to ->
      case model.pendingEdge of
        Just pendingEdge ->
          if pendingEdge.from /= to then
            let
              nodes = Graph.addEdge pendingEdge.from to model.nodes
              newPendingEdge = Nothing
              newModel = { model | nodes = nodes, pendingEdge = newPendingEdge }
            in
              (newModel, Cmd.none)
          else
            (model, Cmd.none)
        Nothing ->
          (model, Cmd.none)
    ClearPending ->
      ({ model | pendingEdge = Nothing }, Cmd.none)
    ChangeMode mode ->
      ({ model | mode = mode }, Cmd.none)
    StartMoving nodeId x y ->
      let
        movingNode = Just nodeId
        newModel = { model | movingNode = movingNode }
      in
        (newModel, Cmd.none)
    TrackMoving x y ->
      case model.movingNode of
        Just nodeId ->
          case Graph.getNode nodeId model.nodes of
            Just node ->
              let
                newNode = { node | x = x, y = y }
                newNodes = Graph.updateNode nodeId newNode model.nodes
                newModel = { model | nodes = newNodes }
              in
                (newModel, Cmd.none)
            Nothing ->
              (model, Cmd.none)
        Nothing ->
          (model, Cmd.none)
    EndMoving ->
      let
        movingNode = Nothing
        newModel = { model | movingNode = movingNode }
      in
        (newModel, Cmd.none)
    RemoveNode nodeId ->
      let
        nodes = Graph.removeNode nodeId model.nodes
        newModel = { model | nodes = nodes }
      in
        (newModel, Cmd.none)
    RemoveEdge fromId toId ->
      let
        nodes = Graph.removeEdge fromId toId model.nodes
        newModel = { model | nodes = nodes }
      in
        (newModel, Cmd.none)
    Clear ->
      let
        newModel = { model | nodes = Graph.empty }
      in
        (newModel, Cmd.none)
    NoOp ->
      (model, Cmd.none)

addPending : Int -> Maybe PendingEdge -> Graph Node -> Graph Node
addPending to pendingEdge graph =
  case pendingEdge of
    Just pendingEdge ->
      Graph.addEdge pendingEdge.from to graph
    Nothing ->
      graph



-- SUBSCRIPTIONS ####################################################################

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW #############################################################################

view : Model -> Html Msg
view model =
  let
    widthStr = toString model.viewport.width
    heightStr = toString model.viewport.height
    box = "0 0 " ++ widthStr ++ " " ++ heightStr
    widthPx = widthStr ++ "px"
    heightPx = heightStr ++ "px"
    pendingEdgeLine = pendingLine model.pendingEdge model.nodes
    svgClass = case model.mode of
      Add -> "adding"
      Move -> "moving"
      Delete -> "deleting"
  in
    div []
      [ svg
        [ viewBox box
        , width widthPx
        , height heightPx
        , backdropMouseDown model.mode
        , backdropMouseMove model.mode model.pendingEdge
        , backdropMouseUp model.mode model.pendingEdge
        , class svgClass
        ]
        [ pendingEdgeLine
        , Keyed.node "g" [] (edges model.mode model.nodes)
        , Keyed.node "g" [] (nodes model.mode model.nodes)
        ]
      , button [onClick Clear] [text "Clear"]
      , button [onClick (ChangeMode Add), disabled (model.mode == Add)] [text "Add"]
      , button [onClick (ChangeMode Move), disabled (model.mode == Move)] [text "Move"]
      , button [onClick (ChangeMode Delete), disabled (model.mode == Delete)] [text "Delete"]
      , br [] []
      , text ("nodes: " ++ (toString (List.length (Graph.toNodeList model.nodes))))
      , br [] []
      , text ("edges: " ++ (toString (List.length (Graph.toEdgeList model.nodes))))
      ]

pendingLine : Maybe PendingEdge -> Graph Node -> Svg Msg
pendingLine pEdge graph =
  case pEdge of
    Just pendingEdge ->
      case Graph.getNode pendingEdge.from graph of
        Just node ->
          let
            fromX = toString node.x
            fromY = toString node.y
            toX = toString pendingEdge.x
            toY = toString pendingEdge.y
          in
            g
              [ class "pending" ]
              [ line [x1 fromX, y1 fromY, x2 toX, y2 toY, class "edge"] []
              , circle [cx toX, cy toY, r "20", class "node"] []
              ]
        Nothing ->
          g [] []
    Nothing ->
      g [] []

nodes : Mode -> Graph Node -> List (String, Svg Msg)
nodes mode graph =
  let
    nodes = Graph.toNodeList graph
    mapFun = (\(id, node) -> (toString id, circle
      [ cx (toString node.x)
      , cy (toString node.y)
      , r "20", class "node"
      , nodeMouseDown mode (id, node)
      , nodeMouseMove mode (id, node)
      , nodeMouseUp mode (id, node)
      ]
      []
    ))
  in
    List.map mapFun nodes

toLine : (Int, Int) -> Mode -> Graph Node -> (String, Svg Msg)
toLine (fromId, toId) mode graph =
  let
    from = Graph.getNode fromId graph
    to = Graph.getNode toId graph
  in
    case (from, to) of
      (Just fromNode, Just toNode) ->
        let
          fromX = toString fromNode.x
          fromY = toString fromNode.y
          toX = toString toNode.x
          toY = toString toNode.y
          key = (toString fromId) ++ "->" ++ (toString toId)
          theLine = line
            [ x1 fromX
            , y1 fromY
            , x2 toX
            , y2 toY
            , lineMouseDown mode fromId toId
            , class "edge"
            ]
            []
        in
          (key, theLine)
      _ ->
        ("none", g [] [])

edges : Mode -> Graph Node -> List (String, Svg Msg)
edges mode graph =
  let
    pairs = Graph.toEdgeList graph
    mapFun = (\pair -> toLine pair mode graph)
  in
    List.map mapFun pairs

getXY : (Int -> Int -> Int -> Msg) -> Json.Decoder Msg
getXY toVal =
  let
    getX = Json.field "clientX" Json.int
    getY = Json.field "clientY" Json.int
    getWhich = Json.field "which" Json.int
  in
    Json.map3 toVal getX getY getWhich

backdropMouseDown : Mode -> Attribute Msg
backdropMouseDown mode =
  case mode of
    Add ->
      on "mousedown" (getXY (\x y which -> Create x y))
    Move ->
      on "mousedown" (Json.succeed NoOp)
    Delete ->
      on "mousedown" (Json.succeed NoOp)

backdropMouseMove : Mode -> Maybe PendingEdge -> Attribute Msg
backdropMouseMove mode pEdge =
  case mode of
    Add ->
      case pEdge of
        Just pendingEdge ->
          on "mousemove" (getXY (\x y which -> if which == 1 then TrackPending x y else ClearPending))
        Nothing ->
          on "mousemove" (Json.succeed NoOp)
    Move ->
      on "mousemove" (getXY (\x y which -> if which == 1 then TrackMoving x y else EndMoving))
    Delete ->
      on "mousemove" (Json.succeed NoOp)

backdropMouseUp : Mode -> Maybe PendingEdge -> Attribute Msg
backdropMouseUp mode pEdge =
  case mode of
    Add ->
      on "mouseup" (getXY (\x y which -> Create x y))
    Move ->
      on "mouseup" (Json.succeed EndMoving)
    Delete ->
      on "mouseup" (Json.succeed NoOp)

nodeMouseDown : Mode -> (Int, Node) -> Attribute Msg
nodeMouseDown mode (nodeId, node) =
  case mode of
    Add ->
      onWithOptions "mousedown" (Options True True) (getXY (\x y which -> StartPending nodeId x y))
    Move ->
      onWithOptions "mousedown" (Options True True) (getXY (\x y which -> StartMoving nodeId x y))
    Delete ->
      on "mousedown" (Json.succeed (RemoveNode nodeId))

nodeMouseMove : Mode -> (Int, Node) -> Attribute Msg
nodeMouseMove mode (nodeId, node) =
  case mode of
    Add ->
      onWithOptions "mousemove" (Options True True) (Json.succeed (TrackPending node.x node.y))
    Move ->
      on "mousemove" (Json.succeed NoOp)
    Delete ->
      on "mousemove" (Json.succeed NoOp)

nodeMouseUp : Mode -> (Int, Node) -> Attribute Msg
nodeMouseUp mode (nodeId, node) =
  case mode of
    Add ->
      onWithOptions "mouseup" (Options True True) (Json.succeed (EndPending nodeId))
    Move ->
      on "mouseup" (Json.succeed NoOp)
    Delete ->
      on "mouseup" (Json.succeed NoOp)

lineMouseDown : Mode -> Int -> Int -> Attribute Msg
lineMouseDown mode fromId toId =
  case mode of
    Add ->
      on "mousedown" (Json.succeed NoOp)
    Move ->
      on "mousedown" (Json.succeed NoOp)
    Delete ->
      onMouseDown (RemoveEdge fromId toId)
