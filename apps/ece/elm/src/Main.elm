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


type Counter
  = Int


type Msg
    = Get
    | GotCounter (Result Http.Error Int)
    | Increment
    | Decrement
    | GotText (Result Http.Error String)

type Model
  = Failure
  | Loading
  | Success String
  | Counter Int


init : () -> (Model, Cmd Msg)
init _ =
    ( Loading, get_counter )

xinit _ =
  ( Loading
  , Http.get
      { url = "counter"
      , expect = Http.expectString GotText
      }
  )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Get ->
        (model, Cmd.none)

    GotCounter result ->
        case result of
            Ok n ->
                (Counter n, Cmd.none)
            Err _ ->
                (Failure, Cmd.none)


    Increment ->
        increment model

    Decrement ->
      decrement model

    GotText result ->
      case result of
        Ok fullText ->
            (Counter 1000, Cmd.none)
--          (Success fullText, Cmd.none)

        Err _ ->
          (Failure, Cmd.none)



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


increment : Model -> (Model, Cmd Msg)
increment model =
    case model of
        Failure -> (Failure, Cmd.none)
        Loading -> (Loading, Cmd.none)
        Success _ -> (Counter 0, Cmd.none)
        Counter n -> (Counter (n + 1), Cmd.none)


decrement : Model -> (Model, Cmd Msg)
decrement model =
    case model of
        Failure -> (Failure, Cmd.none)
        Loading -> (Loading, Cmd.none)
        Success _ -> (Counter 0, Cmd.none)
        Counter n -> (Counter (n - 1), Cmd.none)



get_counter : Cmd Msg
get_counter =
  Http.get
      { url = "counter"
      , expect = Http.expectJson GotCounter counter_json_decode
      }

counter_json_decode : JD.Decoder Int
counter_json_decode =
    JD.field "counter" JD.int
