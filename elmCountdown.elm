import Time exposing (..)
import Html exposing (div, button, text)
import Html.Events exposing (onClick)


type alias Model = { counter : Int, running : Bool }

model0 = {counter = 0, running = True }

type Action = Increment | Start | Stop | Reset

update : Action -> Model -> Model
update a m =
  case a of
    Incrememt -> 
      if not m.running then m
      else { m | counter = m.counter + 1 }
    Start -> { m | running = True }
    Stop  -> { m | running = False }
    Reset -> { m | counter = 0 }

startMailbox = Signal.mailbox False
stopMailbox  = Signal.mailbox False
resetMailbox = Signal.mailbox False

view m =
  div []
    [ button [ onClick startMailbox.address True ] [ text "start" ]
    , div [] [ text (toString m.counter) ]
    , button [ onClick stopMailbox.address True ] [ text "stop" ]
    , button [ onClick resetMailbox.address True ] [ text "reset" ]
    ]


actionMaker : Float -> Bool -> Bool -> Bool -> Action
actionMaker _ start stop reset =
  if start then Start
     else if stop then Stop
          else if reset then Reset
               else Increment

actionSignal =
  Signal.map4 actionMaker (every second) startMailbox.signal stopMailbox.signal resetMailbox.signal

model = Signal.foldp update model0 actionSignal

main = Signal.map view model
