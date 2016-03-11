import Time exposing (..)
import Html exposing (div, button, text)
import Html.Events exposing (onClick)


type alias Model = { counter : Int, running : Bool }

model0 = {counter = 0, running = True }

type Action = Increment | Start | Stop | Reset

update : Action -> Model -> Model
update a m =
  case a of
    Increment ->
      if not m.running then m
      else { m | counter = m.counter + 1 }
    Start -> { m | running = True }
    Stop  -> { m | running = False }
    Reset -> { m | counter = 0 }

buttonsMailbox = Signal.mailbox Start

view m =
  div []
    [ button [ onClick buttonsMailbox.address Start ] [ text "start" ]
    , div [] [ text (toString m.counter) ]
    , button [ onClick buttonsMailbox.address Stop ] [ text "stop" ]
    , button [ onClick buttonsMailbox.address Reset ] [ text "reset" ]
    ]

incrementSignal = Signal.map (always Increment) (every second)
allButtonsSignal = buttonsMailbox.signal
actionSignal = Signal.merge incrementSignal allButtonsSignal
model = Signal.foldp update model0 actionSignal

main = Signal.map view model
