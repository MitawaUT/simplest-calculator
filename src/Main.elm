module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Html exposing (..)
import Html.Attributes exposing (..)

--  (Html, button, div, text,)
import Html.Events exposing (onClick)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL
type State = Inputting
           | Operating
           | Outputting

type Model = Model { memory: String, display: String, state: State, opr: (Model -> Model) }


init : Model
init = Model { memory = "0", display = "0", state = Inputting, opr = \m -> m}
    



-- UPDATE


type Msg
    = Value String
    | Operator (Model -> Model)
    | Executer (Model -> Model)


update : Msg -> Model -> Model
update msg model =
    case msg of
    Value v ->
        case model of
        Model m ->
            case m.state of
            -- Operating -> { model | display = updateAddDigit v model.current}
            Outputting -> Model { m | memory = "0", display = v, state = Inputting }
            _ -> Model { m | display = updateAddDigit v m.display }

    Operator func ->
        case model of  
        Model m -> 
            case m.state of
            Inputting -> Model { memory = m.display, display = "0", state = Operating, opr = func }
            Operating -> operatedModel (m.opr (Model m)) func
            Outputting -> Model { memory = m.display, display = "0", state = Operating, opr = func }

    Executer func ->
        case model of  
        Model m -> 
            case m.state of
            _ -> func (Model m)



-- VIEW


view : Model -> Html Msg
view model =
    case model of
    Model m ->
        div []
        [ div [ class "field" ]
            [ text m.display
            ]
        , div [ class "btn-wrap" ]
            [ viewBtnGen (Value "1") "1"
            , viewBtnGen (Value "2") "2"
            , viewBtnGen (Value "3") "3"
            , viewBtnGen (Operator updateAddOperator) "+"
            ]
        , div [ class "btn-wrap" ]
            [ viewBtnGen (Value "4") "4"
            , viewBtnGen (Value "5") "5"
            , viewBtnGen (Value "6") "6"
            , viewBtnGen (Operator updateSubOperator) "-"
            ]
        , div [ class "btn-wrap" ]
            [ viewBtnGen (Value "7") "7"
            , viewBtnGen (Value "8") "8"
            , viewBtnGen (Value "9") "9"
            , viewBtnGen (Operator updateMultOperator) "*"
            , viewBtnGen (Operator updateQuotOperator) "/"
            ]
        , div [ class "btn-wrap" ]
            [ viewBtnGen (Value "00") "00"
            , viewBtnGen (Value "0") "0"
            , viewBtnGen (Executer updateClsOperator) "C"
            , viewBtnGen (Executer updateEqOperator) "="
            ]
        ]


viewBtnGen : Msg -> String -> Html Msg
viewBtnGen msg fieldTxt = 
    button [ onClick (msg), class "btn-flat-border"  ] [ text fieldTxt ]

normalizeDigits : String -> String 
normalizeDigits s =
    String.left 12
    (case String.toFloat s of
            Nothing ->
                s 
            Just v -> String.fromFloat v
    )

updateAddDigit : String -> String -> String
updateAddDigit c s = 
    normalizeDigits (s ++ c)

operatedModel : Model -> (Model -> Model) -> Model
operatedModel model func =
    case model of
    Model m ->
        Model {m | opr = func}

updateClsOperator : Model -> Model 
updateClsOperator model = 
    case model of
    Model m ->
        Model { memory = "0", display = "0", state = Inputting, opr = \lam -> lam}

updateAddOperator : Model -> Model 
updateAddOperator model = 
    case model of
    Model m ->
        case List.map String.toFloat [m.memory, m.display] of
            [Just a, Just b] -> 
                Model { m | memory = normalizeDigits (String.fromFloat (a + b)), display = "0", opr = \lam -> lam }
            _ -> model

updateSubOperator : Model -> Model 
updateSubOperator model = 
    case model of
    Model m ->
        case List.map String.toFloat [m.memory, m.display] of
            [Just a, Just b] -> 
                Model { m | memory = normalizeDigits (String.fromFloat (a - b)), display = "0", opr = \lam -> lam }
            _ -> model

updateMultOperator : Model -> Model 
updateMultOperator model = 
    case model of
    Model m ->
        case List.map String.toFloat [m.memory, m.display] of
            [Just a, Just b] -> 
                Model { m | memory = normalizeDigits (String.fromFloat (a * b)), display = "0", opr = \lam -> lam }
            _ -> model

updateQuotOperator : Model -> Model 
updateQuotOperator model = 
    case model of
    Model m ->
        case List.map String.toFloat [m.memory, m.display] of
            [Just a, Just b] -> 
                Model { m | memory = normalizeDigits (String.fromFloat (a / b)), display = "0", opr = \lam -> lam }
            _ -> model

updateEqOperator : Model -> Model 
updateEqOperator model = 
    case model of
    Model m ->
        case m.state of
            Inputting -> Model { m | memory = m.display, display = m.display, state = Outputting }
            _ ->
                let opm = m.opr (Model m)
                in 
                    case opm of
                    Model mm ->
                        Model { mm | display = mm.memory, state = Outputting }
