module Main exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onClick )
import Array exposing (..)
import Debug exposing (log)
import Time  exposing (Time, millisecond)
import Random exposing (..)

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
 points : Array Point,
 isPlaying : Bool
}

init : (Model, Cmd Msg)
init = ({
  isPlaying = False,
  points = Array.initialize 144 (\idx -> {
      index = idx,
      x = idx % 12,
      y = (idx // 12) % 12,
      status = 0
    })
  }, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every (500 * millisecond) Tick

-- UTILS
getStatus: Point -> String
getStatus point =
  if point.status == 1 then
    "alive"
  else
    "dead"  

placeholder : Point
placeholder = {
    x = 0,
    y = 0,
    index = 0,
    status = 0
  }

getStatusByCoords: Int -> Int -> Array Point -> Int
getStatusByCoords x y points = 
  let coord = Maybe.withDefault placeholder (Array.get 0 (Array.filter (\p -> (p.x == x && p.y == y)) points))
  in
    .status coord 

countAndChange: Point -> Model -> Int
countAndChange point mod =
  let 
    upL = if (point.y > 0) then 
            getStatusByCoords (point.x - 1) (point.y - 1) mod.points
          else 0 
    upC = if (point.y > 0) then 
            getStatusByCoords point.x  (point.y - 1) mod.points
          else 0
    upR = if (point.y > 0) then 
            getStatusByCoords (point.x + 1) (point.y - 1) mod.points
          else 0  
    downL = if (point.y < 11 ) then 
              getStatusByCoords (point.x - 1) (point.y + 1) mod.points
            else 0
    downC = if (point.y < 11) then 
              getStatusByCoords point.x  (point.y + 1) mod.points
            else 0  
    downR = if (point.y < 11) then 
              getStatusByCoords (point.x + 1) (point.y + 1) mod.points
            else 0
    left = if (point.x > 0) then 
            getStatusByCoords (point.x - 1) point.y  mod.points
          else 0 
    right = if (point.x < 11) then 
              getStatusByCoords (point.x + 1) point.y mod.points
            else 0
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
      let
        newPoints =
          Array.map (\point -> {point | status = (countAndChange point model)}) model.points
      in
        ({model | points = newPoints}, Cmd.none)
    Tick newTime ->
      if (model.isPlaying) then
        let
          newPoints =
            Array.map (\point -> {point | status = (countAndChange point model)}) model.points
        in
          ({model | points = newPoints}, Cmd.none)
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
        (model, Random.generate NewBoard (Random.list 144 (Random.int 0 1 ) ) )
    Clear ->
      ({model | isPlaying = False, points = ( Array.map (\point -> {point | status = 0}) model.points ) }, Cmd.none)      
    NoOp -> (model, Cmd.none)

-- VIEW
view : Model -> Html Msg
view model =
  div [ class "container", style [("margin-top", "30px"), ( "text-align", "center" )] ][    -- inline CSS (literal)
    h1 [] [text "Game of Life"],
    table [id "board"] [
      tbody [] [
        tr [] 
          (List.map(\point -> td [class (getStatus point), onClick (ToggleCell point.index)] [] ) (Array.toList (Array.slice 0 12 model.points))),
        tr [] 
          (List.map(\point -> td [class (getStatus point), onClick (ToggleCell point.index)] [] ) (Array.toList (Array.slice 12 24 model.points))),
        tr [] 
          (List.map(\point -> td [class (getStatus point), onClick (ToggleCell point.index)] [] ) (Array.toList (Array.slice 24 36 model.points))),
        tr [] 
          (List.map(\point -> td [class (getStatus point), onClick (ToggleCell point.index)] [] ) (Array.toList (Array.slice 36 48 model.points))),
        tr [] 
          (List.map(\point -> td [class (getStatus point), onClick (ToggleCell point.index)] [] ) (Array.toList (Array.slice 48 60 model.points))),
        tr [] 
          (List.map(\point -> td [class (getStatus point), onClick (ToggleCell point.index)] [] ) (Array.toList (Array.slice 72 84 model.points))),
        tr [] 
          (List.map(\point -> td [class (getStatus point), onClick (ToggleCell point.index)] [] ) (Array.toList (Array.slice 84 96 model.points))),
        tr [] 
          (List.map(\point -> td [class (getStatus point), onClick (ToggleCell point.index)] [] ) (Array.toList (Array.slice 96 108 model.points))),
                  tr [] 
          (List.map(\point -> td [class (getStatus point), onClick (ToggleCell point.index)] [] ) (Array.toList (Array.slice 108 120 model.points))),
                  tr [] 
          (List.map(\point -> td [class (getStatus point), onClick (ToggleCell point.index)] [] ) (Array.toList (Array.slice 120 132 model.points))),
                  tr [] 
          (List.map(\point -> td [class (getStatus point), onClick (ToggleCell point.index)] [] ) (Array.toList (Array.slice 132 144 model.points)))

      ]
    ],
    div [id "control_panel"] [
      button [id "step_btn", classList [("btn", True), ("btn-success", True)], onClick Step] [text "Step"],
      button [id "play_btn", classList [("btn", True), ("btn-primary", True)], onClick TogglePlay] [text "Play"],
      button [id "reset_btn", classList [("btn", True), ("btn-warning", True)], onClick Reset] [text "Reset"],
      button [id "clear_btn", classList [("btn", True), ("btn-info", True)], onClick Clear] [text "Clear"]
    ]
  ]

