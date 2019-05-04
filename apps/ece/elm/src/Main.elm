import Browser
import Html exposing (Html, button, div, node, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)
import Http
import Json.Decode as JD


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Msg
    = GotCounter (Result Http.Error Int)
    | Increment
    | Decrement

type Model
  = Failure
  | Loading
  | Success String
  | Counter Int


init : () -> (Model, Cmd Msg)
init _ =
    ( Loading, get_counter )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
      
    GotCounter result ->
        case result of
            Ok n ->
                (Counter n, Cmd.none)
            Err _ ->
                (Failure, Cmd.none)

    Increment ->
        (model, increment)

    Decrement ->
      (model, decrement)



subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



view : Model -> Html Msg
view model =
    div [] [
        nav_bar,
        div [ class "container" ] [
            counter_card model
        ]
    ]


nav_bar =
    node "nav" [ class "navbar", class "navbar-light", class "bg-light" ] [
        text "ece is Erlang Cowboy Elm"
    ]


counter_card model =
    div [ class "card" ] [
        div [ class "card-header" ] [
            text "The Counter"
        ],
        div [ class "card-body" ] [
            counter model
        ]
    ]


counter model =
    case model of
        Failure -> text "Error"
        Loading -> text "Loading..."
        Success _ -> text "Success"
        Counter n -> counter_and_buttons n


counter_and_buttons : Int -> Html Msg
counter_and_buttons n =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (String.fromInt n) ]
    , button [ onClick Increment ] [ text "+" ]
    ]


increment : Cmd Msg
increment =
  Http.get
      { url = "counter/increment"
      , expect = Http.expectJson GotCounter counter_json_decode
      }


decrement : Cmd Msg
decrement =
  Http.get
      { url = "counter/decrement"
      , expect = Http.expectJson GotCounter counter_json_decode
      }



get_counter : Cmd Msg
get_counter =
  Http.get
      { url = "counter"
      , expect = Http.expectJson GotCounter counter_json_decode
      }

counter_json_decode : JD.Decoder Int
counter_json_decode =
    JD.field "counter" JD.int
