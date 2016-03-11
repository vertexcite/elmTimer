import Time exposing (..)
import Html exposing (div, button, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (disabled)


type alias Model = { startedTime : Float, currentTime : Float, running : Bool }

model0 = { startedTime = 0, currentTime = 0, running = False }

type Action = Update Time | Start Time | Stop | Reset Time

update : Action -> Model -> Model
update a m =
  case a of
    Update currentTime ->
      if not m.running then m
      else { m | currentTime = currentTime }
    Start currentTime -> { m | running = True, currentTime = currentTime, startTime = currentTime }
    Stop  -> { m | running = False }
    Reset currentTime -> { m | startedTime = currentTime, currentTime = currentTime }

buttonsMailbox = Signal.mailbox Start

view m =
  div []
    [ button [ onClick buttonsMailbox.address <| Start m.currentTime, disabled m.running ] [ text "start" ]
    , div [] [ text <| toString <| m.currentTime - m.startedTime ]
    , button [ onClick buttonsMailbox.address Stop , disabled <| not m.running ] [ text "stop" ]
    , button [ onClick buttonsMailbox.address <| Reset m.currentTime ] [ text "reset" ]
    ]

incrementSignal = Signal.map Update (every second)
allButtonsSignal = buttonsMailbox.signal
actionSignal = Signal.merge incrementSignal allButtonsSignal
model = Signal.foldp update model0 actionSignal

main = Signal.map view model
