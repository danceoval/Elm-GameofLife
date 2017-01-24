module Main exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onClick )
import Array exposing (..)



-- APP
main : Program Never Model Msg
main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL
type alias Point = {
  index : Int,
  x : Int,
  y : Int,
  status : Bool
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
      status = False
    })
  }

-- UTILS
getStatus: Point -> String
getStatus point =
  if point.status == True then
    "alive"
  else
    "dead"  

placeholder : Point
placeholder = {
    x = 0,
    y = 0,
    index = 0,
    status = False
  }

-- UPDATE
type Msg = ToggleCell Int 
          | NoOp

update : Msg -> Model -> Model
update msg model =
  case msg of
    ToggleCell idx ->
      let 
        point = 
          Maybe.withDefault placeholder (Array.get idx model.points)
        newPoints = 
          Array.set idx {point | status = (not point.status) } model.points  
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
      button [id "step_btn", classList [("btn", True), ("btn-success", True)]] [text "Step"],
      button [id "play_btn", classList [("btn", True), ("btn-primary", True)]] [text "Play"],
      button [id "reset_btn", classList [("btn", True), ("btn-warning", True)]] [text "Reset"],
      button [id "clear_btn", classList [("btn", True), ("btn-info", True)]] [text "Clear"]
    ]
  ]

