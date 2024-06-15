module Main exposing (..)

import Browser
import Browser.Dom exposing (Error(..))
import Html exposing (Html, button, div, i, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.Events.Extra.Touch as Touch


type Operator
    = Multiply
    | Divide
    | Add
    | Subtract


type alias Model =
    { xReg : Maybe Float
    , yReg : Maybe Float
    , operator : Maybe Operator
    , error : Maybe String
    , decimalPressed : Bool
    }


init : Model
init =
    { xReg = Nothing
    , yReg = Nothing
    , operator = Nothing
    , error = Nothing
    , decimalPressed = False
    }


type Msg
    = PressedAC
    | PressedDigit Int
    | PressedDecimal
    | PressedEqual
    | PressedOp Operator
    | Error String
    | Noop


update : Msg -> Model -> Model
update msg model =
    case msg of
        PressedAC ->
            init

        PressedDigit i ->
            case ( model.yReg, model.operator ) of
                ( Nothing, Just _ ) ->
                    { model
                        | yReg = model.xReg
                        , xReg = Just <| toFloat i
                    }

                _ ->
                    { model
                        | xReg =
                            case model.xReg of
                                Nothing ->
                                    Just <| toFloat i

                                Just x ->
                                    let
                                        xString =
                                            String.fromFloat x

                                        xStringWithDecimal =
                                            xString
                                                ++ (if model.decimalPressed && not (String.contains "." xString) then
                                                        "."

                                                    else
                                                        ""
                                                   )

                                        newString =
                                            xStringWithDecimal ++ String.fromInt i
                                    in
                                    String.toFloat newString
                    }

        PressedDecimal ->
            { model | decimalPressed = True }

        PressedOp op ->
            { model | operator = Just op }

        PressedEqual ->
            let
                res =
                    case ( model.operator, model.xReg, model.yReg ) of
                        ( Nothing, _, _ ) ->
                            model.xReg

                        ( _, Nothing, _ ) ->
                            model.xReg

                        ( _, _, Nothing ) ->
                            model.xReg

                        ( Just Divide, Just y, Just x ) ->
                            Just <| x / y

                        ( Just Subtract, Just y, Just x ) ->
                            Just <| x - y

                        ( Just Multiply, Just y, Just x ) ->
                            Just <| x * y

                        ( Just Add, Just y, Just x ) ->
                            Just <| x + y
            in
            if res == Nothing then
                model

            else
                { model | xReg = res, yReg = Nothing }

        Error e ->
            { model | error = Just e }

        Noop ->
            model


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


view : Model -> Html Msg
view model =
    div [ class "bg-black w-[420px] 96 py-4 px-4 resize-none" ]
        [ display model
        , div [ class "space-x-4 space-y-4" ] [ keyAC model, keyFn "±", keyFn "%", keyOp model Divide ]
        , div [ class "space-x-4 space-y-4" ] [ keyDigit "7", keyDigit "8", keyDigit "9", keyOp model Multiply ]
        , div [ class "space-x-4 space-y-4" ] [ keyDigit "4", keyDigit "5", keyDigit "6", keyOp model Subtract ]
        , div [ class "space-x-4 space-y-4" ] [ keyDigit "1", keyDigit "2", keyDigit "3", keyOp model Add ]
        , div [ class "space-x-4 space-y-4" ] [ keyWide "0", keyDecimal, keyEqual model ]
        ]


display : Model -> Html Msg
display model =
    let
        regDisplay =
            case ( model.error, model.xReg ) of
                ( Just e, _ ) ->
                    e

                ( Nothing, Just x ) ->
                    let
                        xDisp =
                            String.fromFloat x

                        split =
                            String.split "." xDisp

                        ( beforeDecimal, afterDecimal ) =
                            case split of
                                [ b, a ] ->
                                    ( b, a )

                                [ b ] ->
                                    ( b, "" )

                                _ ->
                                    ( "", "" )

                        beforeDecimalFormatted =
                            let
                                beforeDecimalNumber =
                                    String.length beforeDecimal
                            in
                            if beforeDecimalNumber < 4 then
                                beforeDecimal

                            else if beforeDecimalNumber == 4 then
                                String.left 1 beforeDecimal
                                    ++ ","
                                    ++ String.dropLeft 1 beforeDecimal

                            else if beforeDecimalNumber == 5 then
                                String.left 2 beforeDecimal
                                    ++ ","
                                    ++ String.dropLeft 2 beforeDecimal

                            else if beforeDecimalNumber == 6 then
                                String.left 3 beforeDecimal
                                    ++ ","
                                    ++ String.dropLeft 3 beforeDecimal

                            else if beforeDecimalNumber == 7 then
                                String.left 1 beforeDecimal
                                    ++ ","
                                    ++ String.slice 1 4 beforeDecimal
                                    ++ ","
                                    ++ String.dropLeft 4 beforeDecimal

                            else if beforeDecimalNumber == 8 then
                                String.left 2 beforeDecimal
                                    ++ ","
                                    ++ String.slice 2 5 beforeDecimal
                                    ++ ","
                                    ++ String.dropLeft 5 beforeDecimal

                            else
                                String.left 3 beforeDecimal
                                    ++ ","
                                    ++ String.slice 3 6 beforeDecimal
                                    ++ ","
                                    ++ String.dropLeft 6 beforeDecimal
                    in
                    if model.decimalPressed && afterDecimal == "" then
                        beforeDecimalFormatted ++ "."

                    else if afterDecimal == "" then
                        beforeDecimalFormatted

                    else
                        beforeDecimalFormatted ++ "." ++ afterDecimal

                ( Nothing, Nothing ) ->
                    "0"

        regDisplayLen =
            String.length regDisplay
    in
    div [ class "h-32" ]
        [ div
            [ class <|
                "text-white  py-4 px-4 text-right "
                    ++ (if regDisplayLen < 8 then
                            "text-8xl"

                        else if regDisplayLen < 12 then
                            "text-6xl"

                        else
                            "text-4xl"
                       )
            ]
            [ text regDisplay
            ]
        ]


keyDigitWithWidth : Bool -> String -> Html Msg
keyDigitWithWidth wide ch =
    let
        digit : Maybe Int
        digit =
            String.toInt ch
    in
    button
        [ class <|
            "bg-gray-500  text-white text-4xl py-4 px-4 h-20 rounded-full"
                ++ (if wide then
                        " w-44 text-left ps-7"

                    else
                        " w-20"
                   )
        , Touch.onStart <|
            \_ ->
                case digit of
                    Just d ->
                        PressedDigit d

                    Nothing ->
                        Error <| "Not a digit"
        ]
        [ text ch ]


keyWide : String -> Html Msg
keyWide ch =
    keyDigitWithWidth True ch


keyDigit : String -> Html Msg
keyDigit ch =
    keyDigitWithWidth False ch


keyFn : String -> Html Msg
keyFn ch =
    button
        [ class "bg-gray-300 text-black text-2xl py-4 px-4 size-20 rounded-full"
        , onClick Noop
        ]
        [ text ch ]


keyAC : Model -> Html Msg
keyAC model =
    button
        [ class "bg-gray-300 text-black text-2xl py-4 px-4 size-20 rounded-full"
        , Touch.onStart (\_ -> PressedAC)
        ]
        [ if model.xReg == Nothing then
            text "AC"

          else
            text "C"
        ]


keyEqual : Model -> Html Msg
keyEqual model =
    button
        [ class "bg-gray-300 text-black text-2xl py-4 px-4 size-20 rounded-full"
        , Touch.onStart (\_ -> PressedEqual)
        ]
        [ text "="
        ]


keyDecimal : Html Msg
keyDecimal =
    button
        [ class "bg-gray-500 text-white text-4xl py-4 px-4 size-20 rounded-full"
        , Touch.onStart (\_ -> PressedDecimal)
        ]
        [ text "." ]


keyOp : Model -> Operator -> Html Msg
keyOp model op =
    let
        buttonColor =
            case model.operator of
                Nothing ->
                    "bg-orange-400 text-white "

                Just modelOperator ->
                    if modelOperator == op then
                        "bg-white text-orange-400 "

                    else
                        "bg-orange-400 text-white "
    in
    button
        [ class <| buttonColor ++ "text-4xl py-4 px-4 size-20 rounded-full"
        , Touch.onStart (\_ -> PressedOp op)
        , onClick <| PressedOp op
        ]
        [ text <|
            case op of
                Multiply ->
                    "×"

                Divide ->
                    "÷"

                Add ->
                    "+"

                Subtract ->
                    "-"
        ]


divide : String
divide =
    "÷"


plusOrMinus : String
plusOrMinus =
    "±"
