import Browser
import Html exposing (button, div, node, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)


main =
  Browser.sandbox { init = 0, update = update, view = view }


type Msg = Increment | Decrement


update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1


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
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (String.fromInt model) ]
    , button [ onClick Increment ] [ text "+" ]
    ]
