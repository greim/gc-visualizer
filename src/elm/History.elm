module History exposing (History, push, pop, unpop, peek, init, hasItems, length, hasFuture, futureLength)

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
  case history.items of
    (prevKey, prevItem) :: rest ->
      case (prevKey == key, prevItem == item) of
        (False, False) ->
          let
            newItems = (key, item) :: history.items
          in
            { history | items = newItems, future = [] }
        (True, False) ->
          let
            newItems = (key, item) :: rest
          in
            { history | items = newItems, future = [] }
        _ ->
          history
    [] ->
      let
        newItems = (key, item) :: history.items
      in
        { history | items = newItems, future = [] }

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
