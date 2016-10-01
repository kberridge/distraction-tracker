import Html exposing (Html, button, div, text)
import Html.App as App
import Html.Events exposing (onClick)
import Task
import Time exposing(Time)
import Date
import Debug

main =
  App.program { init = init, view = view, update = update, subscriptions = (\_ -> Sub.none) }

-- MODEL
type alias Distraction =
  { startTime : Time
  , endTime : Maybe Time
  , distractionType : Maybe String
  }

type alias Model = 
  { distractions : List Distraction
  }

init : (Model, Cmd a)
init =
  ( { distractions = []
    }
  , Cmd.none)

-- UPDATE
type Msg
  = GetTimeAndThen ModelMsg
  | GotTime ModelMsg Time

type ModelMsg 
  = Distracted
  | BackToWork

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GetTimeAndThen wrappedMsg ->
      (model, Task.perform assertNeverHandler (GotTime wrappedMsg) Time.now)

    GotTime wrappedMsg time ->
      let
        (newModel, cmd) = modelUpdate wrappedMsg time model
      in
        (newModel, Cmd.map GetTimeAndThen cmd)
      
modelUpdate : ModelMsg -> Time -> Model -> (Model, Cmd a)
modelUpdate msg time model =
  case msg of
    Distracted ->
      let 
        newModel = 
          {model | distractions = (buildDistraction time) :: model.distractions}
      in
        (newModel, Cmd.none)

    BackToWork ->
      let 
        current =
          List.head model.distractions
        ended =
          case current of
            Nothing -> Nothing
            Just d -> Just { d | endTime = Just time }
        newDistractions = 
          case ended of
            Nothing -> model.distractions
            Just distraction -> distraction :: List.drop 1 model.distractions 
      in
        ({model | distractions = newDistractions}, Cmd.none)

buildDistraction : Time -> Distraction
buildDistraction time =
  { startTime = time
  , endTime = Maybe.Nothing
  , distractionType = Maybe.Nothing
  }

assertNeverHandler : a -> b
assertNeverHandler =
    (\_ -> Debug.crash "This should never happen")

-- VIEW
type ViewMode 
  = WorkingMode
  | DistractedMode Distraction

getViewMode : Model -> ViewMode
getViewMode model =
  let
    h = List.head model.distractions
  in
    case h of
      Nothing -> 
        WorkingMode
      Just d ->
        case d.endTime of
          Nothing ->
            DistractedMode d
          Just et ->
            WorkingMode

view : Model -> Html Msg
view model =
  let
    viewMode = getViewMode model
  in
    case viewMode of
      WorkingMode ->
        viewWorkingMode model.distractions
      DistractedMode distraction ->
        viewDistractedMode distraction

viewWorkingMode distractions =
  div []
    [ button [ onClick (GetTimeAndThen Distracted) ] [ text "Distracted" ]
    , div [] 
        [ Html.h1 [] [text "Distractions"]
        , Html.ul [] (List.map viewDistraction distractions)
        ] 
    ]

viewDistraction distraction =
  Html.li []
    [ text (timeToString distraction.startTime) ]

viewDistractedMode distraction =
  div []
    [ text ("You got distracted at " ++ timeToString distraction.startTime) 
    , button [ onClick (GetTimeAndThen BackToWork) ] [ text "Back to Work" ]
    ]

timeToString time =
  let
    date = Date.fromTime time
  in
    toString date