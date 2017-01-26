module Main exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onClick )
import Array exposing (..)
import Debug exposing (log)
import Time  exposing (Time, millisecond)
import Random exposing (..)

gridSize : Int
gridSize = 25 -- SET SIZE 

-- APP
main : Program Never Model Msg
main =
  Html.program { init = init, view = view, update = update, subscriptions = subscriptions }


-- MODEL
type alias Point = {
  index : Int,
  x : Int,
  y : Int,
  status : Int
}

type alias Model = {
  size : Int,
  points : Array Point,
  isPlaying : Bool
}

init : (Model, Cmd Msg)
init = ({
  size = gridSize,
  isPlaying = False,
  points = Array.initialize (gridSize * gridSize) (\idx -> {
      index = idx,
      x = idx % gridSize,
      y = (idx // gridSize) % gridSize,
      status = 0
    })
  }, Cmd.none)

placeholder : Point
placeholder = {
    x = 0,
    y = 0,
    index = 0,
    status = 0
  }


-- UTILS
getStatus: Point -> String
getStatus point =
  if point.status == 1 then
    "alive"
  else
    "dead"  

setNewPoints: Model -> Array Point
setNewPoints model = 
  Array.map (\point -> {point | status = (countAndChange point model)}) model.points


mapRows : Int -> Array Point -> List (Html Msg)
mapRows idx points =
  if idx < 0 then
    []
  else -- Recursive case, generate tr with tds mapped to points in slice of points array
    List.singleton (tr [] 
      (List.map(\point -> td [class (getStatus point), onClick (ToggleCell point.index)] [] ) (Array.toList (Array.slice (gridSize * (idx - 1)) (gridSize * idx) points))))
    ++ (mapRows (idx - 1) points)

getStatusByCoords: Int -> Int -> Array Point -> Int
getStatusByCoords x y points = 
  let coord = Maybe.withDefault placeholder (Array.get 0 (Array.filter (\p -> (p.x == x && p.y == y)) points))
  in
    .status coord 

countAndChange: Point -> Model -> Int
countAndChange point mod =
  let 
    upL = getStatusByCoords (point.x - 1) (point.y - 1) mod.points 
    upC = getStatusByCoords point.x  (point.y - 1) mod.points
    upR = getStatusByCoords (point.x + 1) (point.y - 1) mod.points 
    downL = getStatusByCoords (point.x - 1) (point.y + 1) mod.points
    downC = getStatusByCoords point.x  (point.y + 1) mod.points  
    downR = getStatusByCoords (point.x + 1) (point.y + 1) mod.points
    left = getStatusByCoords (point.x - 1) point.y  mod.points
    right = getStatusByCoords (point.x + 1) point.y mod.points
    total = upL + upC + upR + left + right + downL + downC + downR         
  in                        
    if point.status == 1 then
      if (total == 2 || total == 3) then 1 --alive
      else 0 --dead
    else 
      if total == 3 then 1
      else 0
    
-- UPDATE
type Msg = ToggleCell Int 
          | Step
          | Tick Time
          | TogglePlay
          | NewBoard (List Int) 
          | Reset 
          | Clear 
          | NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ToggleCell idx ->
      let 
        point = 
          Maybe.withDefault placeholder (Array.get idx model.points)
        newStatus = 
          if point.status == 0 then 1
          else 0  
        newPoints = 
          Array.set idx {point | status = newStatus } model.points  
      in
        ({model | points = newPoints}, Cmd.none)
    Step ->
      ({model | isPlaying = False, points = (setNewPoints model)}, Cmd.none)
    Tick newTime ->
      if (model.isPlaying) then
        ({model | points = (setNewPoints model)}, Cmd.none)
      else
        (model, Cmd.none)     
    TogglePlay ->
      ({model | isPlaying = (not model.isPlaying)}, Cmd.none)  
    NewBoard newBoard -> 
      let 
        statusArray = Array.fromList newBoard
        newPoints = 
          Array.map (\point -> { point | status = Maybe.withDefault 0 (Array.get (point.index) statusArray ) } ) model.points
      in
        ({ model | points = newPoints } ,Cmd.none)
    Reset -> 
        (model, Random.generate NewBoard (Random.list (model.size * model.size) (Random.int 0 1 ) ) )
    Clear ->
      ({model | isPlaying = False, points = ( Array.map (\point -> {point | status = 0}) model.points ) }, Cmd.none)      
    NoOp -> (model, Cmd.none)

-- VIEW
view : Model -> Html Msg
view model =
  div [ class "container", style [("margin-top", "30px"), ( "text-align", "center" )] ][    -- inline CSS (literal)
    h1 [] [text "Game of Life"],
    div [id "control_panel"] [
      button [id "step_btn", classList [("btn", True), ("btn-success", True)], onClick Step] [text "Step"],
      button [id "play_btn", classList [("btn", True), ("btn-primary", True)], onClick TogglePlay] [text "Play"],
      button [id "reset_btn", classList [("btn", True), ("btn-warning", True)], onClick Reset] [text "Reset"],
      button [id "clear_btn", classList [("btn", True), ("btn-info", True)], onClick Clear] [text "Clear"]
    ],
    table [id "board"] [
      tbody [] (mapRows model.size model.points)
    ]
  ]

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every (100 * millisecond) Tick


