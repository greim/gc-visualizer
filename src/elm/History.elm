module History exposing (History, push, pop, unpop, peek, init, hasItems, length, hasFuture, futureLength, break)

import Set exposing (Set)
--import Debug exposing (log)

type alias History a =
  { first : a
  , items : List (String, a)
  , future : List (String, a)
  }

init : a -> History a
init item =
  History item [] []

push : String -> a -> History a -> History a
push key item history =
  --let k = log "key" key in
  case history.items of
    (prevKey, prevItem) :: rest ->
      let
        newItems = case Set.member (prevKey, key) collapsibles of
          True -> (key, item) :: rest
          False -> (key, item) :: history.items
      in
        { history | items = newItems, future = [] }
    [] ->
      let
        newItems = (key, item) :: history.items
      in
        { history | items = newItems, future = [] }

break : History a -> History a
break history =
  case history.items of
    (prevKey, prevItem) :: rest ->
      let newItems = ("", prevItem) :: rest
      in { history | items = newItems }
    [] ->
      history

pop : History a -> History a
pop history =
  case history.items of
    first :: rest ->
      { history | items = rest, future = first :: history.future }
    [] ->
      history

unpop : History a -> History a
unpop history =
  case history.future of
    first :: newFuture ->
      let
        newItems = first :: history.items
      in
        { history | items = newItems, future = newFuture }
    [] ->
      history

peek : History a -> a
peek history =
  case List.head history.items of
    Just (key, item) ->
      item
    Nothing ->
      history.first

hasItems : History a -> Bool
hasItems history =
  List.isEmpty history.items |> not

length : History a -> Int
length history =
  List.length history.items

hasFuture : History a -> Bool
hasFuture history =
  List.isEmpty history.future |> not

futureLength : History a -> Int
futureLength history =
  List.length history.future

collapsibles : Set (String, String)
collapsibles =
  Set.fromList
    [ ("start-nothing", "end-nothing")
    , ("start-nothing", "end-node")
    , ("start-node", "end-node")
    , ("start-node", "end-nothing")
    , ("move", "move")
    , ("mark", "mark")
    , ("sweep", "sweep")
    ]
