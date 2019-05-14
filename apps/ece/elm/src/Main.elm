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
    | Ignore (Result Http.Error ())

type alias Model =
    { counter : Maybe Int
    , failure : String
    }


init : () -> (Model, Cmd Msg)
init _ =
    ( {counter = Nothing, failure = ""}, initial_get_counter )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of

        GotCounter result ->
            case result of
                Ok n ->
                    ({model | counter = Just n}, watch_counter <| Just n)
                Err _ ->
                    ({model | failure = "Failure"}, watch_counter model.counter)

        Increment ->
            (model, increment)

        Decrement ->
            (model, decrement)

        Ignore _->
            (model, Cmd.none)



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


counter_card : Model -> Html Msg
counter_card model =
    div [ class "card" ] [
        div [ class "card-header" ] [
            text "The Counter"
        ],
        div [ class "card-body" ] [
            counter model
        ]
    ]


counter : Model -> Html Msg
counter model =
    case model.counter of
        Nothing -> div [] []
        Just v -> counter_and_buttons v


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
      , expect = Http.expectWhatever Ignore
--      , expect = Http.expectJson GotCounter counter_json_decode
      }


decrement : Cmd Msg
decrement =
  Http.get
      { url = "counter/decrement"
      , expect = Http.expectWhatever Ignore
      }


initial_get_counter : Cmd Msg
initial_get_counter =
  Http.get
      { url = "counter"
      , expect = Http.expectJson GotCounter counter_json_decode
      }

watch_counter : Maybe Int -> Cmd Msg
watch_counter n =
    case n of
        Nothing ->
            initial_get_counter
        Just v ->
            Http.get
                { url = "counter/watch?counter=" ++ String.fromInt v
                , expect = Http.expectJson GotCounter counter_json_decode
                }

counter_json_decode : JD.Decoder Int
counter_json_decode =
    JD.field "counter" JD.int
