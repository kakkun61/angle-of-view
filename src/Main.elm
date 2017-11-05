module Main exposing (..)

import Html exposing (Html, div, input, program, text)
import Html.Attributes as Html exposing (id, max, min, step, type_)
import Html.Events exposing (on, targetValue)
import Svg exposing (Svg, line, rect, svg)
import Svg.Attributes exposing (fill, height, stroke, strokeWidth, width, x, y)
import String exposing (toInt)
import Json.Decode as Json

type alias Model = { focalLength : Int
                   , photoreceiverWidth : Float
                   , photoreceiverHeight : Float
                   }

type Msg = NoOp
         | FocalLength Int

chartWidth : Float
chartWidth = 500

chartHeight : Float
chartHeight = 1200

init : ( Model, Cmd Msg )
init = ( { focalLength = 30, photoreceiverWidth = 23.4, photoreceiverHeight = 16.7 }, Cmd.none )

view : Model -> Svg Msg
view model = 
    div []
        [ div []
              [ text "Focal Length:"
              , input [ on "input" (targetValue
                                       |> Json.andThen (\strVal ->
                                              case toInt strVal of
                                                  Err msg -> Json.fail msg
                                                  Ok val -> Json.succeed <| FocalLength val
                                          )
                                   )
                      , type_ "range"
                      , Html.min "1"
                      , Html.max "1000"
                      , step "1"
                      ] []
              , text <| toString model.focalLength
              ]
        , drawChart model
        ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
        FocalLength fl ->
            ( { model | focalLength = fl }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

main : Program Never Model Msg
main =
    program { init = init
            , view = view
            , update = update
            , subscriptions = subscriptions
            }

drawChart : Model -> Svg Msg
drawChart { focalLength, photoreceiverWidth, photoreceiverHeight } =
    svg
        [ id "chart" ]
        [ rect
            [ fill "black"
            , x <| toString  <| (chartWidth - 10 * photoreceiverWidth) / 2
            , y "10"
            , width <| toString <| 10 * photoreceiverWidth
            , height "2"
            ]
            []
        ]
