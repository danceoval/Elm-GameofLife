module Main exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onClick )
import Array exposing (..)
import Debug exposing (log)


-- APP
main : Program Never Model Msg
main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL
type alias Point = {
  index : Int,
  x : Int,
  y : Int,
  status : Int
}

type alias Model = {
 points : Array Point
}

model : Model
model = {
  points = Array.initialize 64 (\idx -> {
      index = idx,
      x = idx % 8,
      y = (idx // 8) % 8,
      status = 0
    })
  }

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
    downL = if (point.y < 7 ) then 
              getStatusByCoords (point.x - 1) (point.y + 1) mod.points
            else 0
    downC = if (point.y < 7) then 
              getStatusByCoords point.x  (point.y + 1) mod.points
            else 0  
    downR = if (point.y < 7) then 
              getStatusByCoords (point.x + 1) (point.y + 1) mod.points
            else 0
    left = if (point.x > 0) then 
            getStatusByCoords (point.x - 1) point.y  mod.points
          else 0 
    right = if (point.x < 7) then 
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
          | NoOp

update : Msg -> Model -> Model
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
        {model | points = newPoints}
    Step ->
      let
        newPoints =
          Array.map (\point -> {point | status = (countAndChange point model)}) model.points
      in
        {model | points = newPoints}        
    NoOp -> model


view : Model -> Html Msg
view model =
  div [ class "container", style [("margin-top", "30px"), ( "text-align", "center" )] ][    -- inline CSS (literal)
    h1 [] [text "Game of Life"],
    table [id "board"] [
      tbody [] [
        tr [] 
          (List.map(\point -> td [class (getStatus point), onClick (ToggleCell point.index)] [] ) (Array.toList (Array.slice 0 8 model.points))),
        tr [] 
          (List.map(\point -> td [class (getStatus point), onClick (ToggleCell point.index)] [] ) (Array.toList (Array.slice 8 16 model.points))),
        tr [] 
          (List.map(\point -> td [class (getStatus point), onClick (ToggleCell point.index)] [] ) (Array.toList (Array.slice 16 24 model.points))),
        tr [] 
          (List.map(\point -> td [class (getStatus point), onClick (ToggleCell point.index)] [] ) (Array.toList (Array.slice 24 32 model.points))),
        tr [] 
          (List.map(\point -> td [class (getStatus point), onClick (ToggleCell point.index)] [] ) (Array.toList (Array.slice 32 40 model.points))),
        tr [] 
          (List.map(\point -> td [class (getStatus point), onClick (ToggleCell point.index)] [] ) (Array.toList (Array.slice 40 48 model.points))),
        tr [] 
          (List.map(\point -> td [class (getStatus point), onClick (ToggleCell point.index)] [] ) (Array.toList (Array.slice 48 56 model.points))),
        tr [] 
          (List.map(\point -> td [class (getStatus point), onClick (ToggleCell point.index)] [] ) (Array.toList (Array.slice 56 64 model.points)))      
      ]
    ],
    div [id "control_panel"] [
      button [id "step_btn", classList [("btn", True), ("btn-success", True)], onClick Step] [text "Step"],
      button [id "play_btn", classList [("btn", True), ("btn-primary", True)]] [text "Play"],
      button [id "reset_btn", classList [("btn", True), ("btn-warning", True)]] [text "Reset"],
      button [id "clear_btn", classList [("btn", True), ("btn-info", True)]] [text "Clear"]
    ]
  ]

