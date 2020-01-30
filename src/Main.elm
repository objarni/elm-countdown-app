module Main exposing (main)

import Browser
import Element exposing (Element)
import Element.Background as Bg
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Time


type alias Model =
    { now : Time.Posix
    , page : Page
    }


type Page
    = InputPage { minutes : Int }
    | CountdownPage { endTime : Time.Posix }


type Msg
    = SetMinutesTo Int
    | GotoCountdown Int
    | Tick Time.Posix


init : () -> ( Model, Cmd a )
init _ =
    ( { now = Time.millisToPosix 0
      , page = InputPage { minutes = 10 }
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd a )
update msg model =
    case msg of
        SetMinutesTo m ->
            let
                croppedMinutes =
                    if m > 300 then
                        300

                    else
                        m
            in
            ( { model
                | page = InputPage { minutes = croppedMinutes }
              }
            , Cmd.none
            )

        GotoCountdown minutes ->
            let
                nowMs =
                    Time.posixToMillis model.now

                countdownMs =
                    minutes * 60 * 1000
            in
            ( { model
                | page =
                    CountdownPage
                        { endTime = Time.millisToPosix <| nowMs + countdownMs
                        }
              }
            , Cmd.none
            )

        Tick time ->
            ( { model | now = time }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick


view : Model -> Html Msg
view model =
    Element.layout [ Bg.image "bg.jpg" ]
        (Element.column
            [ Element.centerX ]
            [ case model.page of
                InputPage { minutes } ->
                    viewInputBox minutes

                CountdownPage countdown ->
                    viewCountdown model.now countdown.endTime
            , Element.text <| String.fromInt (Time.toSecond Time.utc model.now)
            ]
        )


viewInputBox : Int -> Element Msg
viewInputBox minutes =
    let
        minuteParse x =
            case String.toInt x of
                Just n ->
                    n

                Nothing ->
                    minutes
    in
    Element.el
        [ Element.centerX -- these two centers the div
        , Element.centerY
        , Element.width (Element.px 150)
        , Element.height (Element.px 150)
        ]
        (Element.el
            -- these two centers the Element.text in div
            [ Element.centerX, Element.centerY ]
            (Element.column
                [ Bg.color (Element.rgba 1 1 1 1)
                , Element.padding 20
                , Element.spacing 10
                ]
                [ Input.text
                    []
                    { onChange = \x -> SetMinutesTo (minuteParse x)
                    , text = String.fromInt minutes
                    , placeholder = Nothing
                    , label = Input.labelAbove [] (Element.text "How many minutes?")
                    }
                , Input.button
                    [ Element.centerX, Element.centerY ]
                    { onPress = Just <| GotoCountdown minutes
                    , label =
                        Element.el []
                            (Element.text "Start countdown")
                    }
                ]
            )
        )


viewCountdown : Time.Posix -> Time.Posix -> Element Msg
viewCountdown now endTime =
    let
        countdownMs =
            Time.posixToMillis endTime - Time.posixToMillis now

        countdown =
            Time.millisToPosix countdownMs

        pad n =
            String.padLeft 2 '0' n

        minutes =
            Time.toMinute Time.utc countdown

        seconds =
            Time.toSecond Time.utc countdown
    in
    Element.el
        [ Element.centerX -- these two centers the div
        , Element.centerY
        , Bg.color (Element.rgba 1 1 1 0.8)
        , Element.width (Element.px 350)
        , Element.height (Element.px 150)
        ]
        (Element.el
            -- these two centers the Element.text in div
            [ Element.centerX
            , Element.centerY
            ]
            (Element.row
                [ Font.size 58 ]
                [ Element.text <| pad <| String.fromInt minutes
                , Element.text " : "
                , Element.text <| pad <| String.fromInt seconds
                ]
            )
        )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
