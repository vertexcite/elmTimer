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

startMailbox = Signal.mailbox 0
stopMailbox  = Signal.mailbox 0
resetMailbox = Signal.mailbox 0

clock t start stop reset =
  div []
    [ button [ onClick startMailbox.address 1 ] [ text "start" ]
    , div [] [ text (toString t) ]
    , button [ onClick stopMailbox.address 1 ] [ text "stop" ]
    , button [ onClick resetMailbox.address 1 ] [ text "reset" ]
    ]


model = Signal.foldp update model0 myMailbox.signal

main =
  Signal.map4 clock (every second) startMailbox.signal stopMailbox.signal resetMailbox.signal
