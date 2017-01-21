import Html exposing (Html, div, button, br, span)
import Html.Attributes exposing (disabled)
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Keyed as Keyed
import Svg.Attributes exposing (..)
import Graph exposing (Graph)
import Json.Decode as Json
import Window
import Task
import Process
import Time exposing (Time)
import Set exposing (Set)
import V
import History exposing (History)
import Dom

--import Debug exposing (log)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL ############################################################################

type Mode = Move | Add | Delete | Label

type Mark = Marked | Unmarked | None

type alias Node =
  { x : Int
  , y : Int
  , isRoot : Bool
  , mark : Mark
  , label : String
  }

type alias PendingEdge =
  { from : Int
  , x : Int
  , y : Int
  }

type alias Model =
  { history : History (Graph Node)
  , viewport : Window.Size
  , mode : Mode
  , pendingEdge : Maybe PendingEdge
  , movingNode : Maybe Int
  , labelingNode : Maybe Int
  , showCode : Bool
  , codeSize : Int
  , code : String
  }

init : (Model, Cmd Msg)
init =
  let
    history = History.init Graph.empty
    viewport = Window.Size 0 0
    mode = Add
    pendingEdge = Nothing
    movingNode = Nothing
    labelingNode = Nothing
    showCode = False
    codeSize = 20
    code = "// type here...\n"
    model = Model history viewport mode pendingEdge movingNode labelingNode showCode codeSize code
    cmd = Task.perform Resize Window.size
  in
    (model, cmd)



-- UPDATE ###########################################################################

type Msg
  = StartOnNothing Int Int
  | EndOnNothing Int Int
  | StartOnNode Int Int Int Bool
  | EndOnNode Int
  | TrackStretch Int Int
  | ClearPending
  | ChangeMode Mode
  | StartMoving Int Int Int
  | TrackMoving Int Int
  | EndMoving
  | RemoveNode Int
  | RemoveEdge Int Int
  | StartLabeling Int
  | TrackLabeling String
  | EndLabeling
  | Clear
  | Unmark
  | MarkStart
  | Mark (List Int)
  | SweepStart
  | Sweep (List Int)
  | Done
  | Resize Window.Size
  | Undo
  | Redo
  | ToggleShowCode
  | ChangeCodeSize Int
  | ChangeCode String
  | NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    modelNodes = History.peek model.history
  in
    case msg of

      StartOnNothing x y ->
        let
          node = Node x y False None ""
          (id, newNodes) = Graph.addNode node modelNodes
          pendingEdge = Just (PendingEdge id x y)
          newHistory = History.push "start-nothing" newNodes model.history
          newModel = { model | history = newHistory, pendingEdge = pendingEdge }
        in
          (newModel, Cmd.none)

      EndOnNothing x y ->
        let
          node = Node x y False None ""
          (id, intermediateNodes) = Graph.addNode node modelNodes
          finalNodes = addPending id model.pendingEdge intermediateNodes
          newHistory = case model.pendingEdge of
            Just pendingEdge ->
              History.push "end-nothing" finalNodes model.history
            Nothing ->
              model.history
          newModel = { model | history = newHistory, pendingEdge = Nothing }
        in
          (newModel, Cmd.none)

      StartOnNode from x y isMeta ->
        case Graph.getNode from modelNodes of
          Just node ->
            case isMeta of
              True ->
                let
                  newNode = { node | isRoot = True }
                  newNodes = Graph.updateNode from newNode modelNodes
                  pendingEdge = Just (PendingEdge from x y)
                  newHistory = History.push "start-node" newNodes model.history
                  newModel = { model | pendingEdge = pendingEdge, history = newHistory }
                in
                  (newModel, Cmd.none)
              False ->
                let
                  pendingEdge = Just (PendingEdge from x y)
                  newHistory = History.break model.history
                  newModel = { model | pendingEdge = pendingEdge, history = newHistory }
                in
                  (newModel, Cmd.none)
          Nothing ->
            (model, Cmd.none)

      EndOnNode to ->
        case model.pendingEdge of
          Just pendingEdge ->
            if pendingEdge.from /= to then
              let
                newNodes = Graph.addEdge pendingEdge.from to modelNodes
                newHistory = History.push "end-node" newNodes model.history
                newModel = { model | pendingEdge = Nothing, history = newHistory }
              in
                (newModel, Cmd.none)
            else
              let
                newModel = { model | pendingEdge = Nothing }
              in
                (newModel, Cmd.none)
          Nothing ->
            (model, Cmd.none)

      TrackStretch x y ->
        case model.pendingEdge of
          Just pendingEdge ->
            let
              newPendingEdge = { pendingEdge | x = x, y = y }
              newModel = { model | pendingEdge = Just newPendingEdge }
            in
              (newModel, Cmd.none)
          Nothing ->
            (model, Cmd.none)

      ClearPending ->
        ({ model | pendingEdge = Nothing }, Cmd.none)

      ChangeMode mode ->
        ({ model | mode = mode }, Cmd.none)

      StartMoving nodeId x y ->
        case Graph.getNode nodeId modelNodes of
          Just node ->
            let
              movingNode = Just nodeId
              newNode = { node | x = x, y = y }
              newNodes = Graph.updateNode nodeId newNode modelNodes
              newHistory1 = History.break model.history
              newHistory2 = History.push "move" newNodes newHistory1
              newModel = { model | movingNode = movingNode, history = newHistory2 }
            in
              (newModel, Cmd.none)
          Nothing ->
            (model, Cmd.none)

      TrackMoving x y ->
        case model.movingNode of
          Just nodeId ->
            case Graph.getNode nodeId modelNodes of
              Just node ->
                let
                  newNode = { node | x = x, y = y }
                  newNodes = Graph.updateNode nodeId newNode modelNodes
                  newHistory = History.push "move" newNodes model.history
                  newModel = { model | history = newHistory }
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
          newNodes = Graph.removeNode nodeId modelNodes
          newHistory = History.push "remove-node" newNodes model.history
          newModel = { model | history = newHistory }
        in
          (newModel, Cmd.none)

      RemoveEdge fromId toId ->
        let
          newNodes = Graph.removeEdge fromId toId modelNodes
          newHistory = History.push "remove-edge" newNodes model.history
          newModel = { model | history = newHistory }
        in
          (newModel, Cmd.none)

      StartLabeling nodeId ->
        let
          newLabelingNode = Just nodeId
          newHistory = History.break model.history
          newModel = { model | labelingNode = newLabelingNode }
        in
          (newModel, Task.attempt (always NoOp) (Dom.focus "label-input-field"))

      TrackLabeling label ->
        case model.labelingNode of
          Just nodeId ->
            case Graph.getNode nodeId modelNodes of
              Just node ->
                let
                  newNode = { node | label = label }
                  newNodes = Graph.updateNode nodeId newNode modelNodes
                  newHistory = History.push "label" newNodes model.history
                  newModel = { model | history = newHistory }
                in
                  (newModel, Cmd.none)
              Nothing ->
                (model, Cmd.none)
          Nothing ->
            (model, Cmd.none)

      EndLabeling ->
        let
          newModel = { model | labelingNode = Nothing }
        in
          (newModel, Cmd.none)

      Clear ->
        let
          --newHistory = History.push "clear" Graph.empty model.history
          newHistory = History.init Graph.empty
          newModel = { model | history = newHistory, code = "// type here...\n" }
        in
          (newModel, Cmd.none)

      Unmark ->
        let
          newNodes = Graph.map (\node -> { node | mark = Unmarked }) modelNodes
          newHistory = History.push "unmark" newNodes model.history
          newModel = { model | history = newHistory }
        in
          (newModel, Cmd.none)

      MarkStart ->
        let
          ids = Graph.toNodeList modelNodes
            |> List.filter (\(id, node) -> node.isRoot)
            |> List.map (\(id, node) -> Graph.findConnected id modelNodes)
            |> List.map (\set -> Set.toList set)
            |> List.concat
        in
          (model, Task.perform Mark (Task.succeed ids))

      Mark nodes ->
        case nodes of
          id :: rest ->
            let
              updateFn = (\node -> { node | mark = Marked })
              newNodes = modelNodes |> Graph.updateNodeFn updateFn id
              newHistory = History.push "mark" newNodes model.history
              newModel = { model | history = newHistory }
            in
              (newModel, delay 20 (Mark rest))
          [] ->
            (model, Cmd.none)

      SweepStart ->
        let
          ids = Graph.toNodeList modelNodes
            |> List.filter (\(id, node) -> node.mark /= Marked)
            |> List.map (\(id, node) -> id)
        in
          (model, Task.perform Sweep (Task.succeed ids))

      Sweep nodes ->
        case nodes of
          id :: rest ->
            let
              newNodes = Graph.removeNode id modelNodes
              newHistory = History.push "sweep" newNodes model.history
              newModel = { model | history = newHistory }
            in
              (newModel, delay 20 (Sweep rest))
          [] ->
            (model, Cmd.none)

      Done ->
        let
          newNodes = Graph.map (\node -> { node | mark = None }) modelNodes
          newHistory = History.push "done" newNodes model.history
          newModel = { model | history = newHistory }
        in
          (newModel, Cmd.none)

      Resize viewport ->
        let
          newModel = { model | viewport = viewport }
        in
          (newModel, Cmd.none)

      Undo ->
        let
          newHistory = History.pop model.history
          newModel = { model | history = newHistory }
        in
          (newModel, Cmd.none)

      Redo ->
        let
          newHistory = History.unpop model.history
          newModel = { model | history = newHistory }
        in
          (newModel, Cmd.none)

      ToggleShowCode ->
        let
          newShowCode = not model.showCode
          newModel = { model | showCode = newShowCode }
        in
          (newModel, Cmd.none)

      ChangeCodeSize px ->
        let
          newModel = { model | codeSize = px }
        in
          (newModel, Cmd.none)

      ChangeCode code ->
        let
          newModel = { model | code = code }
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
  Window.resizes Resize



-- VIEW #############################################################################

view : Model -> Html Msg
view model =
  let
    modelNodes = History.peek model.history
    widthStr = toString model.viewport.width
    heightStr = toString model.viewport.height
    box = "0 0 " ++ widthStr ++ " " ++ heightStr
    widthPx = widthStr ++ "px"
    heightPx = heightStr ++ "px"
    pending = pendingInfo model.pendingEdge modelNodes
    svgClass = case model.mode of
      Add -> "adding"
      Move -> "moving"
      Delete -> "deleting"
      Label -> "labeling"
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
        [ pendingArrow pending
        , pendingNode pending
        , Keyed.node "g" [] (arrows model.mode modelNodes)
        , Keyed.node "g" [] (nodes model.mode modelNodes)
        ]
      , if model.mode == Label then labelInput model.labelingNode modelNodes else div [] []
      , div
        [ id "code"
        , class (if model.showCode then "shown" else "not-shown")
        ]
        [ button [class "code-toggle", onClick ToggleShowCode] [icon (if model.showCode then "chevron-left" else "chevron-right")]
        , button [onClick (ChangeCodeSize (model.codeSize + 2)), class "code-size", id "code-size-bigger"] [icon "plus-circle"]
        , button [onClick (ChangeCodeSize (model.codeSize - 2)), class "code-size", id "code-size-smaller"] [icon "minus-circle"]
        , Html.textarea [onInput ChangeCode, Html.Attributes.value model.code, Html.Attributes.style [("font-size",(toString model.codeSize) ++ "px")]] []
        ]
      , div
        [ id "modes"
        ]
        [ button [onClick (ChangeMode Add), disabled (model.mode == Add)] [icon "mouse-pointer"]
        , button [onClick (ChangeMode Move), disabled (model.mode == Move)] [icon "arrows"]
        , button [onClick (ChangeMode Label), disabled (model.mode == Label)] [icon "tag"]
        , button [onClick (ChangeMode Delete), disabled (model.mode == Delete)] [icon "remove"]
        , span [class "undo-redo"]
          [ button [onClick Undo, disabled (not <| History.hasItems model.history)] [icon "undo"]
          , button [onClick Redo, disabled (not <| History.hasFuture model.history)] [icon "rotate-right"]
          ]
        ]
      , div
        [ id "actions"
        ]
        [ button [onClick Unmark] [text "Start"]
        , button [onClick MarkStart] [text "Mark"]
        , button [onClick SweepStart] [text "Sweep"]
        , button [onClick Done] [text "Done"]
        , button [onClick Clear] [text "Clear"]
        ]
      , div
        [ id "info"
        ]
        [ text ("node count: " ++ (toString (List.length (Graph.toNodeList modelNodes))))
        , br [] []
        , text ("edge count: " ++ (toString (List.length (Graph.toEdgeList modelNodes))))
        , br [] []
        , text ("history past: " ++ (toString (History.length model.history)))
        , br [] []
        , text ("history future: " ++ (toString (History.futureLength model.history)))
        ]
      ]

labelInput : Maybe Int -> Graph Node -> Html Msg
labelInput maybeNodeId graph =
  case maybeNodeId of
    Just nodeId ->
      case Graph.getNode nodeId graph of
        Just node ->
          Html.form
            [ Html.Attributes.id "label-input"
            , Html.Events.onSubmit EndLabeling
            , Html.Attributes.style
              [ ("left", (toString node.x) ++ "px")
              , ("top", (toString node.y) ++ "px")
              ]
            ]
            [ Html.input
              [ Html.Attributes.id "label-input-field"
              , Html.Attributes.type_ "text"
              , Html.Attributes.value node.label
              , Html.Events.onInput TrackLabeling
              ]
              []
            ]
        node ->
          div [] []
    Nothing ->
      div [][]

icon : String -> Html Msg
icon ico =
  Html.i [class ("fa fa-" ++ ico)] []

pendingInfo : Maybe PendingEdge -> Graph Node -> Maybe (PendingEdge, Node)
pendingInfo maybePendingEdge graph =
  case maybePendingEdge of
    Just pendingEdge ->
      case Graph.getNode pendingEdge.from graph of
        Just node -> Just (pendingEdge, node)
        Nothing -> Nothing
    Nothing -> Nothing

pendingArrow : Maybe (PendingEdge, Node) -> Svg Msg
pendingArrow pendingInfo =
  case pendingInfo of
    Just (pendingEdge, node) ->
      let
        x1 = node.x
        y1 = node.y
        x2 = pendingEdge.x
        y2 = pendingEdge.y
      in
        V.pendingArrow x1 y1 x2 y2
    Nothing ->
      g [] []

pendingNode : Maybe (PendingEdge, Node) -> Svg Msg
pendingNode pendingInfo =
  case pendingInfo of
    Just (pendingEdge, node) ->
      let
        x = pendingEdge.x
        y = pendingEdge.y
      in
        V.pendingNode x y
    Nothing ->
      g [] []

nodes : Mode -> Graph Node -> List (String, Svg Msg)
nodes mode graph =
  Graph.toNodeList graph
    |> List.map (\(id, node) -> createNode id mode node)

arrows : Mode -> Graph Node -> List (String, Svg Msg)
arrows mode graph =
  Graph.toEdgeList graph
    |> List.map (\pair -> createArrow pair mode graph)

createNode : Int -> Mode -> Node -> (String, Svg Msg)
createNode id mode node =
  let
    nodeFn = case node.mark of
      Unmarked -> V.unmarkedNode node.isRoot
      Marked -> V.markedNode node.isRoot
      None -> V.node node.isRoot
  in
    ( toString id
    , nodeFn node.x node.y node.label
        [ nodeMouseDown mode (id, node)
        , nodeMouseMove mode (id, node)
        , nodeMouseUp mode (id, node)
        ]
    )

createArrow : (Int, Int) -> Mode -> Graph Node -> (String, Svg Msg)
createArrow (fromId, toId) mode graph =
  let
    from = Graph.getNode fromId graph
    to = Graph.getNode toId graph
  in
    case (from, to) of
      (Just fromNode, Just toNode) ->
        let
          arrowFn = case (fromNode.mark, toNode.mark) of
            (Marked, Marked) -> V.markedArrow
            (None, None) -> V.arrow
            _ -> V.unmarkedArrow
          attr = lineMouseDown mode fromId toId
          arr = arrowFn fromNode.x fromNode.y toNode.x toNode.y [attr]
          key = (toString fromId) ++ "->" ++ (toString toId)
        in
          (key, arr)
      _ ->
        ("none", g [] [])

arrowFn : Node -> Node -> String
arrowFn node1 node2 =
  case (node1.mark, node2.mark) of
    (Marked, Marked) -> "marked arrow"
    (None, None) -> "arrow"
    _ -> "unmarked arrow"

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
    Add -> on "mousedown" (getXY (\x y -> StartOnNothing x y))
    Move -> on "mousedown" (Json.succeed NoOp)
    Delete -> on "mousedown" (Json.succeed NoOp)
    Label -> on "mousedown" (Json.succeed NoOp)

backdropMouseMove : Mode -> Maybe PendingEdge -> Attribute Msg
backdropMouseMove mode pEdge =
  case mode of
    Add ->
      case pEdge of
        Just pendingEdge ->
          on "mousemove" (getXYExtra (\x y which isShift isMeta -> if which == 1 then TrackStretch x y else ClearPending))
        Nothing ->
          on "mousemove" (Json.succeed NoOp)
    Move ->
      on "mousemove" (getXYExtra (\x y which isShift isMeta -> if which == 1 then TrackMoving x y else EndMoving))
    Delete ->
      on "mousemove" (Json.succeed NoOp)
    Label ->
      on "mousemove" (Json.succeed NoOp)

backdropMouseUp : Mode -> Maybe PendingEdge -> Attribute Msg
backdropMouseUp mode pEdge =
  case mode of
    Add -> on "mouseup" (getXY (\x y -> EndOnNothing x y))
    Move -> on "mouseup" (Json.succeed EndMoving)
    Delete -> on "mouseup" (Json.succeed NoOp)
    Label -> on "mouseup" (Json.succeed EndLabeling)

nodeMouseDown : Mode -> (Int, Node) -> Attribute Msg
nodeMouseDown mode (nodeId, node) =
  case mode of
    Add ->
      let
        opts = Options True True
        fun = (\x y which isShift isMeta -> StartOnNode nodeId x y isMeta)
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
    Label ->
      on "mousedown" (Json.succeed (StartLabeling nodeId))

nodeMouseMove : Mode -> (Int, Node) -> Attribute Msg
nodeMouseMove mode (nodeId, node) =
  case mode of
    Add -> onWithOptions "mousemove" (Options True True) (Json.succeed (TrackStretch node.x node.y))
    Move -> on "mousemove" (Json.succeed NoOp)
    Delete -> on "mousemove" (Json.succeed NoOp)
    Label -> on "mousemove" (Json.succeed NoOp)

nodeMouseUp : Mode -> (Int, Node) -> Attribute Msg
nodeMouseUp mode (nodeId, node) =
  case mode of
    Add -> onWithOptions "mouseup" (Options True True) (Json.succeed (EndOnNode nodeId))
    Move -> on "mouseup" (Json.succeed NoOp)
    Delete -> on "mouseup" (Json.succeed NoOp)
    Label -> on "mouseup" (Json.succeed NoOp)

lineMouseDown : Mode -> Int -> Int -> Attribute Msg
lineMouseDown mode fromId toId =
  case mode of
    Add -> on "mousedown" (Json.succeed NoOp)
    Move -> on "mousedown" (Json.succeed NoOp)
    Delete -> onMouseDown (RemoveEdge fromId toId)
    Label -> on "mousedown" (Json.succeed NoOp)
