-- module ----------------------------------------------------------------------

module Editor exposing
  ( Editor
  , blank
  , create
  , insert
  , insertRight
  , delete
  , deleteRight
  , toString
  , toStrings
  , home
  , end
  , size
  , moveTo
  , moveBy
  , seek
  , seekRight
  , append
  , prepend
  )

-- types -----------------------------------------------------------------------

type alias Editor =
  { before : List Char
  , after : List Char
  }

-- functions/values ------------------------------------------------------------

blank : Editor
blank = Editor [] []

create : String -> Editor
create str =
  Editor [] (String.toList str)

insert : Char -> Editor -> Editor
insert ch editor =
  { editor | before = (ch :: editor.before) }

insertRight : Char -> Editor -> Editor
insertRight ch editor =
  { editor | after = (ch :: editor.after) }

toString : Editor -> String
toString editor =
  let
    beforeRev = List.reverse editor.before
    before = String.fromList beforeRev
    after = String.fromList editor.after
  in
    before ++ after

toStrings : Editor -> (String, String)
toStrings editor =
  let
    beforeRev = List.reverse editor.before
    before = String.fromList beforeRev
    after = String.fromList editor.after
  in
    (before, after)

home : Editor -> Editor
home editor =
  moveTo 0 editor

end : Editor -> Editor
end editor =
  moveTo (size editor) editor

cursorRight : Editor -> Editor
cursorRight editor =
  case editor.after of
    [] ->
      editor
    ch :: rest ->
      { editor | after = rest, before = ch :: editor.before }

cursorLeft : Editor -> Editor
cursorLeft editor =
  case editor.before of
    [] ->
      editor
    ch :: rest ->
      { editor | before = rest, after = ch :: editor.after }

size : Editor -> Int
size editor =
  List.length editor.before + List.length editor.after

moveTo : Int -> Editor -> Editor
moveTo idx editor =
  let
    len = size editor
    currentIdx = List.length editor.before
  in
    if idx == currentIdx then
      editor
    else
      moveBy (idx - currentIdx) editor

moveBy : Int -> Editor -> Editor
moveBy amount editor =
  if amount == 0 then
    editor
  else if amount < 0 then
    let
      posAmount = abs amount
      portion = List.take posAmount editor.before
      newBefore = List.drop posAmount editor.before
      revPortion = List.reverse portion
      newAfter = revPortion ++ editor.after
    in
      { editor | before = newBefore, after = newAfter }
  else
    let
      portion = List.take amount editor.after
      newAfter = List.drop amount editor.after
      revPortion = List.reverse portion
      newBefore = revPortion ++ editor.before
    in
      { editor | before = newBefore, after = newAfter }

seek : String -> Editor -> Editor
seek str editor =
  let
    contents = toString editor
  in
    case String.indices str contents of
      [] ->
        editor
      idx :: rest ->
        moveTo idx editor

seekRight : String -> Editor -> Editor
seekRight str editor =
  let
    newEditor = seek str editor
  in
    moveBy (String.length str) newEditor

delete : Editor -> Editor
delete editor =
  case editor.before of
    [] -> editor
    char :: rest -> { editor | before = rest }

deleteRight : Editor -> Editor
deleteRight editor =
  case editor.after of
    [] -> editor
    char :: rest -> { editor | after = rest }

append : Char -> Editor -> Editor
append ch editor =
  { editor | after = editor.after ++ [ch] }

prepend : Char -> Editor -> Editor
prepend ch editor =
  { editor | before = editor.before ++ [ch] }
