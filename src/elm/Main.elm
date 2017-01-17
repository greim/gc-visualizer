import Html exposing (Html, div, button, br)
import Html.Attributes exposing (disabled)
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Keyed as Keyed
import Svg.Attributes exposing (..)
import Graph exposing (Graph)
import Json.Decode as Json
import Task
import Process
import Time exposing (Time)
import Set exposing (Set)
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

type Mark = Marked | Unmarked | None

type alias Node =
  { x : Int
  , y : Int
  , isRoot : Bool
  , mark : Mark
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
    viewport = Viewport 1200 600
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
  | StartPending Int Int Int Bool
  | EndPending Int
  | ClearPending
  | ChangeMode Mode
  | StartMoving Int Int Int
  | TrackMoving Int Int
  | EndMoving
  | RemoveNode Int
  | RemoveEdge Int Int
  | Clear
  | Unmark
  | MarkStart
  | Mark (List Int)
  | SweepStart
  | Sweep (List Int)
  | Done
  | NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Create x y ->
      let
        node = Node x y False None
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
    StartPending from x y isMeta ->
      case Graph.getNode from model.nodes of
        Just node ->
          let
            newNode = if isMeta then { node | isRoot = True } else node
            nodes = Graph.updateNode from newNode model.nodes
            pendingEdge = Just (PendingEdge from x y)
            newModel = { model | pendingEdge = pendingEdge, nodes = nodes }
          in
            (newModel, Cmd.none)
        Nothing ->
          (model, Cmd.none)
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
    Unmark ->
      let
        nodes = Graph.map (\node -> { node | mark = Unmarked }) model.nodes
        newModel = { model | nodes = nodes }
      in
        (newModel, Cmd.none)
    MarkStart ->
      let
        ids = Graph.toNodeList model.nodes
          |> List.filter (\(id, node) -> node.isRoot)
          |> List.map (\(id, node) -> Graph.findConnected id model.nodes)
          |> List.map (\set -> Set.toList set)
          |> List.concat
      in
        (model, Task.perform Mark (Task.succeed ids))
    Mark nodes ->
      case nodes of
        id :: rest ->
          let
            updateFn = (\node -> { node | mark = Marked })
            nodes = model.nodes |> Graph.updateNodeFn updateFn id
            newModel = { model | nodes = nodes }
          in
            (newModel, delay 20 (Mark rest))
        [] ->
          (model, Cmd.none)
    SweepStart ->
      let
        ids = Graph.toNodeList model.nodes
          |> List.filter (\(id, node) -> node.mark /= Marked)
          |> List.map (\(id, node) -> id)
      in
        (model, Task.perform Sweep (Task.succeed ids))
    Sweep nodes ->
      case nodes of
        id :: rest ->
          let
            newNodes = Graph.removeNode id model.nodes
            newModel = { model | nodes = newNodes }
          in
            (newModel, delay 20 (Sweep rest))
        [] ->
          (model, Cmd.none)
    Done ->
      let
        nodes = Graph.map (\node -> { node | mark = None }) model.nodes
        newModel = { model | nodes = nodes }
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

delay : Time -> msg -> Cmd msg
delay time msg =
  Process.sleep time
    |> Task.andThen (always (Task.succeed msg))
    |> Task.perform identity



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
      , button [onClick Unmark] [text "Prep"]
      , button [onClick MarkStart] [text "Mark"]
      , button [onClick SweepStart] [text "Sweep"]
      , button [onClick Done] [text "Done"]
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
            theArrow = arrow node.x node.y pendingEdge.x pendingEdge.y "arrow" (onClick NoOp)
          in
            g
              [ class "pending" ]
              [ theArrow
              , g [ class "node" ] [ circle [cx toX, cy toY, r "20"] [] ]
              ]
        Nothing ->
          g [] []
    Nothing ->
      g [] []

nodes : Mode -> Graph Node -> List (String, Svg Msg)
nodes mode graph =
  Graph.toNodeList graph
    |> List.map (\(id, node) -> (toString id, createNode id mode node))

createNode : Int -> Mode -> Node -> Svg Msg
createNode id mode node =
  let
    xStr = toString node.x
    yStr = toString node.y
    c = circle
      [ cx xStr
      , cy yStr
      , r "20"
      , nodeMouseDown mode (id, node)
      , nodeMouseMove mode (id, node)
      , nodeMouseUp mode (id, node)
      ]
      []
  in
    if node.isRoot then
      g [class (nodeClass node)] [c, circle [cx xStr, cy yStr, r "5", Svg.Attributes.style "pointer-events:none"] []]
    else
      g [class (nodeClass node)] [c]

nodeClass : Node -> String
nodeClass node =
  case node.mark of
    Marked -> "marked node"
    Unmarked -> "unmarked node"
    None -> "node"

toArrow : (Int, Int) -> Mode -> Graph Node -> (String, Svg Msg)
toArrow (fromId, toId) mode graph =
  let
    from = Graph.getNode fromId graph
    to = Graph.getNode toId graph
  in
    case (from, to) of
      (Just fromNode, Just toNode) ->
        let
          attr = lineMouseDown mode fromId toId
          arrClass = (edgeClass fromNode toNode)
          arr = arrow fromNode.x fromNode.y toNode.x toNode.y arrClass attr
          key = (toString fromId) ++ "->" ++ (toString toId)
        in
          (key, arr)
      _ ->
        ("none", g [] [])

edgeClass : Node -> Node -> String
edgeClass node1 node2 =
  case (node1.mark, node2.mark) of
    (Marked, Marked) -> "marked arrow"
    (None, None) -> "arrow"
    _ -> "unmarked arrow"

edges : Mode -> Graph Node -> List (String, Svg Msg)
edges mode graph =
  let
    pairs = Graph.toEdgeList graph
    mapFun = (\pair -> toArrow pair mode graph)
  in
    List.map mapFun pairs

getXYExtra : (Int -> Int -> Int -> Bool -> Bool -> Msg) -> Json.Decoder Msg
getXYExtra toVal =
  let
    getX = Json.field "clientX" Json.int
    getY = Json.field "clientY" Json.int
    getWhich = Json.field "which" Json.int
    getShift = Json.field "shiftKey" Json.bool
    getMeta = Json.field "metaKey" Json.bool
  in
    Json.map5 toVal getX getY getWhich getShift getMeta

getXY : (Int -> Int -> Msg) -> Json.Decoder Msg
getXY toVal =
  let
    getX = Json.field "clientX" Json.int
    getY = Json.field "clientY" Json.int
  in
    Json.map2 toVal getX getY

backdropMouseDown : Mode -> Attribute Msg
backdropMouseDown mode =
  case mode of
    Add ->
      on "mousedown" (getXY (\x y -> Create x y))
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
          on "mousemove" (getXYExtra (\x y which isShift isMeta -> if which == 1 then TrackPending x y else ClearPending))
        Nothing ->
          on "mousemove" (Json.succeed NoOp)
    Move ->
      on "mousemove" (getXYExtra (\x y which isShift isMeta -> if which == 1 then TrackMoving x y else EndMoving))
    Delete ->
      on "mousemove" (Json.succeed NoOp)

backdropMouseUp : Mode -> Maybe PendingEdge -> Attribute Msg
backdropMouseUp mode pEdge =
  case mode of
    Add ->
      on "mouseup" (getXY (\x y -> Create x y))
    Move ->
      on "mouseup" (Json.succeed EndMoving)
    Delete ->
      on "mouseup" (Json.succeed NoOp)

nodeMouseDown : Mode -> (Int, Node) -> Attribute Msg
nodeMouseDown mode (nodeId, node) =
  case mode of
    Add ->
      let
        opts = Options True True
        fun = (\x y which isShift isMeta -> StartPending nodeId x y isMeta)
      in
        onWithOptions "mousedown" opts (getXYExtra fun)
    Move ->
      let
        opts = Options True True
        fun = (\x y -> StartMoving nodeId x y)
      in
        onWithOptions "mousedown" opts (getXY fun)
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

arrow : Int -> Int -> Int -> Int -> String -> Attribute a -> Html a
arrow x1i y1i x2i y2i cls attr =
  let
    x1s = toString x1i
    x2s = toString x2i
    y1s = toString y1i
    y2s = toString y2i
    aHead = arrowHead x1i y1i x2i y2i
    xDiff = toFloat (x2i - x1i)
    yDiff = toFloat (y2i - y1i)
    hyp = sqrt (xDiff * xDiff + yDiff * yDiff)
    tooShort = hyp < 100
    angle = atan2 yDiff xDiff
    tr1 = translateStr x2i y2i
    rot = rotateStr angle
    tr2 = translateStr -22 0
    arrowHeadTransform = tr1 ++ rot ++ tr2
  in
    g
      [ class cls, attr ]
      [ line [x1 x1s, x2 x2s, y1 y1s, y2 y2s ] []
      , aHead
      ]

arrowHead : Int -> Int -> Int -> Int -> Html a
arrowHead x1i y1i x2i y2i =
  let
    xDiff = toFloat (x2i - x1i)
    yDiff = toFloat (y2i - y1i)
    hyp = sqrt (xDiff * xDiff + yDiff * yDiff)
    tooShort = hyp < 50
    angle = atan2 yDiff xDiff
    tr1 = translateStr x2i y2i
    rot = rotateStr angle
    tr2 = translateStr -22 0
    arrowHeadTransform = tr1 ++ rot ++ tr2
  in
    if tooShort then
      g [] []
    else
      g
        [ class "arrow-head", transform arrowHeadTransform ]
        [ line [x1 "0", y1 "0", x2 "-20", y2 "-15"] []
        , line [x1 "0", y1 "0", x2 "-20", y2 "15"] []
        ]

translateStr : Int -> Int -> String
translateStr x y =
  let
    xs = toString x
    ys = toString y
  in
    "translate(" ++ xs ++ "," ++ ys ++ ")"

rotateStr : Float -> String
rotateStr rads =
  let
    degStr = toString (rads * radConvert)
  in
    "rotate(" ++ degStr ++ ")"

radConvert : Float
radConvert =
  360.0 / (2 * pi)
