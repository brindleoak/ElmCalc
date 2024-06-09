module Main exposing (..)

import Browser
import Element exposing (Element, alignRight, centerX, centerY, column, el, height, padding, px, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)


type alias Model =
    { xReg : Maybe Float
    , yReg : Maybe Float
    , error : Maybe String
    , decimalPressed : Bool
    }


init : Model
init =
    { xReg = Nothing
    , yReg = Nothing
    , error = Nothing
    , decimalPressed = False
    }


type Msg
    = PressedAC
    | PressedDigit Int
    | PressedDecimal
    | Error String


update : Msg -> Model -> Model
update msg model =
    case msg of
        PressedAC ->
            init

        PressedDigit i ->
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

        Error e ->
            { model | error = Just e }


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


view : Model -> Html Msg
view model =
    Element.layout [ Background.color (rgb255 0 0 0) ]
        (myRowOfStuff model)


myRowOfStuff : Model -> Element Msg
myRowOfStuff model =
    el [] <|
        column [ spacing 15, padding 30, Background.color (rgb255 0 0 0) ]
            [ display model
            , row [ centerY, spacing 15 ] [ keyAC model, keyFn "±", keyFn "%", keyOp "÷" ]
            , row [ centerY, spacing 15 ] [ keyDigit "7", keyDigit "8", keyDigit "9", keyOp "x" ]
            , row [ centerY, spacing 15 ] [ keyDigit "4", keyDigit "5", keyDigit "6", keyOp "-" ]
            , row [ centerY, spacing 15 ] [ keyDigit "1", keyDigit "2", keyDigit "3", keyOp "+" ]
            , row [ centerY, spacing 15 ] [ keyWide "0", keyDecimal, keyOp "=" ]
            , el [ Font.color (rgb255 255 255 255) ] (text "Simon has a sticky bum")
            ]


display : Model -> Element msg
display model =
    row [ Font.color (rgb255 255 255 255), Font.size 110, alignRight ]
        [ text <|
            case ( model.error, model.xReg ) of
                ( Just e, _ ) ->
                    e

                ( Nothing, Just x ) ->
                    let
                        s =
                            String.fromFloat x
                    in
                    s
                        ++ (if model.decimalPressed && not (String.contains "." s) then
                                "."

                            else
                                ""
                           )

                ( Nothing, Nothing ) ->
                    "0"
        ]


keyDigitWithWidth : Bool -> String -> Element Msg
keyDigitWithWidth wide ch =
    let
        digit : Maybe Int
        digit =
            String.toInt ch
    in
    Input.button
        [ Background.color (rgb255 50 50 50)
        , width <|
            if wide then
                px 215

            else
                px 100
        , height <| px 100
        , Border.rounded 150
        ]
        { onPress =
            Just <|
                case digit of
                    Just d ->
                        PressedDigit d

                    Nothing ->
                        Error <| "Not a digit"
        , label = el [ Font.size 40, centerX, Font.color (rgb255 255 255 255) ] (text ch)
        }


keyWide : String -> Element Msg
keyWide ch =
    keyDigitWithWidth True ch


keyDigit : String -> Element Msg
keyDigit ch =
    keyDigitWithWidth False ch


keyFn : String -> Element Msg
keyFn ch =
    Input.button
        [ Background.color (rgb255 190 190 190)
        , width <| px 100
        , height <| px 100
        , Border.rounded 150
        ]
        { onPress = Nothing
        , label = el [ Font.size 40, centerX, centerY ] (text ch)
        }


keyAC : Model -> Element Msg
keyAC model =
    Input.button
        [ Background.color (rgb255 190 190 190)
        , width <| px 100
        , height <| px 100
        , Border.rounded 150
        ]
        { onPress = Just PressedAC
        , label =
            el [ Font.size 40, centerX, centerY ]
                (if model.xReg == Nothing then
                    text "AC"

                 else
                    text "C"
                )
        }


keyDecimal : Element Msg
keyDecimal =
    Input.button
        [ Background.color (rgb255 190 190 190)
        , width <| px 100
        , height <| px 100
        , Border.rounded 150
        ]
        { onPress = Just PressedDecimal
        , label =
            el [ Font.size 40, centerX, centerY ] (text ".")
        }


keyOp : String -> Element Msg
keyOp ch =
    Input.button
        [ Background.color (rgb255 255 165 0)
        , width <| px 100
        , height <| px 100
        , Border.rounded 150
        ]
        { onPress = Nothing
        , label = el [ Font.size 40, centerX, centerY, Font.color (rgb255 255 255 255) ] (text ch)
        }


divide : String
divide =
    "÷"


plusOrMinus : String
plusOrMinus =
    "±"
