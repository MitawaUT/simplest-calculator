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
           | Operated
           | Result

type alias Model = { display: String, opr: (Float -> Float), state: State, operator: String }

init : Model
init = { display = "0", opr = \m -> m, state = Waiting, operator = "  " }

-- UPDATE

type Msg
    = Value String
    | Operator (Float -> Float -> Float) String
    | Executer (Float -> Float) String

update : Msg -> Model -> Model
update msg model =
    case msg of
    Value v -> 
        case model.state of
        Waiting -> { model | display = normalizeDigits <| updateInputDigit v model.display}
        _ -> { model | display = normalizeDigits v, state = Waiting}

    Operator func s ->
        case String.toFloat model.display of
        Just a -> 
            case model.state of
            Waiting ->
                { display = (normalizeDigits << String.fromFloat) <| model.opr a, opr = (func <| model.opr a), state = Operated, operator = s }    
            Operated ->
                { model | opr = func a, state = Operated, operator = s }
            Result ->
                { model | opr = (func <| model.opr a), state = Operated, operator = s }
        _ -> { init | operator = "E" }

    Executer func s ->
        case String.toFloat model.display of
        Just a -> 
            case model.state of
            Waiting -> { display = (normalizeDigits << String.fromFloat << func << model.opr) a, opr = \m -> m, state = Result, operator = s }
            _ -> { display = (normalizeDigits << String.fromFloat << func ) a, opr = \m -> m, state = Result, operator = s }
        _ -> { init | operator = "E" }

-- VIEW

view : Model -> Html Msg
view model =
    div []
    [ div [ class "field" ]
        [ div[ class "operator" ] [text model.operator]
        , div[ ] [text model.display]
        ]
    , div [ class "btn-wrap" ]
        [ viewBtnGen (Value) "1"
        , viewBtnGen (Value) "2"
        , viewBtnGen (Value) "3"
        , viewBtnGen (Operator (+)) "+"
        ]
    , div [ class "btn-wrap" ]
        [ viewBtnGen (Value) "4"
        , viewBtnGen (Value) "5"
        , viewBtnGen (Value) "6"
        , viewBtnGen (Operator (-)) "-"
        ]
    , div [ class "btn-wrap" ]
        [ viewBtnGen (Value) "7"
        , viewBtnGen (Value) "8"
        , viewBtnGen (Value) "9"
        , viewBtnGen (Operator (*)) "*"
        , viewBtnGen (Operator (/)) "/"
        ]
    , div [ class "btn-wrap" ]
        [ viewBtnGen (Value) "00"
        , viewBtnGen (Value) "0"
        , viewBtnGen (Executer updateClsExecuter) "C"
        , viewBtnGen (Executer updateEqExecuter) "="
        ]
    ]


viewBtnGen : (String -> Msg) -> String -> Html Msg
viewBtnGen msg_ fieldTxt = 
    button [ onClick (msg_ fieldTxt), class "btn-flat-border"  ] [ text fieldTxt ]

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
