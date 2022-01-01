module Main exposing (..)
import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

-- MAIN

main =
    Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type State = Waiting
           | Result

type alias Model = { display: String, opr: (Float -> Float), state: State }

init : Model
init = { display = "0", opr = \m -> m, state = Waiting}

-- UPDATE

type Msg
    = Value String
    | Operator (Float -> Float -> Float)
    | Executer (Float -> Float)

update : Msg -> Model -> Model
update msg model =
    case msg of
    Value v -> 
        case model.state of
        Waiting -> { model | display = normalizeDigits <| updateInputDigit v model.display}
        Result -> { model | display = normalizeDigits v, state = Waiting}

    Operator func ->
        case model.state of
        Waiting ->
            case String.toFloat model.display of
            Just a -> { model | display = "0", opr = (func <| model.opr a) }
            _ -> init
        Result ->
            case String.toFloat model.display of
            Just a -> { display = "0", opr = (func (model.opr a)), state = Waiting }
            _ -> init 

    Executer func ->
        case String.toFloat model.display of
        Just a -> { display = (normalizeDigits << String.fromFloat << func << model.opr) a, opr = \m -> m, state = Result }
        _ -> init

-- VIEW

view : Model -> Html Msg
view model =
    div []
    [ div [ class "field" ]
        [ text model.display ]
    , div [ class "btn-wrap" ]
        [ viewBtnGen (Value "1") "1"
        , viewBtnGen (Value "2") "2"
        , viewBtnGen (Value "3") "3"
        , viewBtnGen (Operator (+)) "+"
        ]
    , div [ class "btn-wrap" ]
        [ viewBtnGen (Value "4") "4"
        , viewBtnGen (Value "5") "5"
        , viewBtnGen (Value "6") "6"
        , viewBtnGen (Operator (-)) "-"
        ]
    , div [ class "btn-wrap" ]
        [ viewBtnGen (Value "7") "7"
        , viewBtnGen (Value "8") "8"
        , viewBtnGen (Value "9") "9"
        , viewBtnGen (Operator (*)) "*"
        , viewBtnGen (Operator (/)) "/"
        ]
    , div [ class "btn-wrap" ]
        [ viewBtnGen (Value "00") "00"
        , viewBtnGen (Value "0") "0"
        , viewBtnGen (Executer updateClsExecuter) "C"
        , viewBtnGen (Executer updateEqExecuter) "="
        ]
    ]


viewBtnGen : Msg -> String -> Html Msg
viewBtnGen msg fieldTxt = 
    button [ onClick (msg), class "btn-flat-border"  ] [ text fieldTxt ]

normalizeDigits : String -> String 
normalizeDigits s =
    String.left 12
    (
    case String.toFloat s of
    Nothing -> s 
    Just v -> String.fromFloat v
    )

updateInputDigit : String -> String -> String
updateInputDigit c s = s ++ c

updateClsExecuter : Float -> Float
updateClsExecuter _ = 0

updateEqExecuter : Float -> Float
updateEqExecuter a = a
