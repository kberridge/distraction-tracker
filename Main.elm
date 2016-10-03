import Html exposing (Html, button, div, text)
import Html.App as App
import Html.Events exposing (onClick)
import Task
import Time exposing(Time)
import Date
import Date.Format
import Debug

main : Program Never
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

distractionTypes : List String
distractionTypes = 
  [ "Unscheduled Meeting"
  , "Scheduled Meeting"
  , "Incoming Bug"
  , "Design Question"
  , "Implementation Question"
  , "Management Question"
  , "Other"
  ]

init : (Model, Cmd a)
init =
  ( { distractions = []
    }
  , Cmd.none)

-- UPDATE
type Msg
  = GetTimeAndThen ModelMsg
  | GotTime ModelMsg Time
  | SelectDistractionType String

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

    SelectDistractionType distractionType ->
      let
        current =
          List.head model.distractions
        updatedCurrent =
          case current of
            Nothing -> Nothing
            Just d -> Just {d | distractionType = Just distractionType}
        newDistractions =
          case updatedCurrent of
            Nothing -> model.distractions
            Just uped -> uped :: List.drop 1 model.distractions
      in
        ({model | distractions = newDistractions}, Cmd.none)
      
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

viewWorkingMode : List Distraction -> Html Msg
viewWorkingMode distractions =
  div []
    [ button [ onClick (GetTimeAndThen Distracted) ] [ text "Distracted" ]
    , div [] 
        [ Html.h1 [] [text "Distractions"]
        , Html.ul [] (List.map viewDistraction distractions)
        ] 
    ]

viewDistraction : Distraction -> Html Msg
viewDistraction distraction =
  let
    typeDisplay = 
      case distraction.distractionType of
        Nothing -> ""
        Just dt -> " - " ++ dt
  in
    Html.li []
      [ text ((timeToString distraction.startTime) ++ typeDisplay) ]

viewDistractedMode : Distraction -> Html Msg
viewDistractedMode distraction =
  div []
    [ text ("You got distracted at " ++ timeToString distraction.startTime) 
    , Html.ul [] (List.map (viewDistractionType distraction) distractionTypes)
    , button [ onClick (GetTimeAndThen BackToWork) ] [ text "Back to Work" ]
    ]

viewDistractionType : Distraction -> String -> Html Msg
viewDistractionType distraction distractionType =
  let
    currentlySelected =
      case distraction.distractionType of
        Nothing ->
          False
        Just dt ->
          distractionType == dt
    selectedIndicator =
      if currentlySelected then
        "[x]"
      else
        ""
  in
    Html.li[]
      [ Html.a [ onClick (SelectDistractionType distractionType) ] [text (selectedIndicator ++ distractionType)] ]

timeToString : Time -> String
timeToString time =
  Date.Format.format "%I:%M%p" (Date.fromTime time)