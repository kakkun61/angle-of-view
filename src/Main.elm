module Main exposing (..)

import Html exposing (Html, div, input, option, program, select, text)
import Html.Attributes as Html exposing (id, max, min, step, type_, value)
import Html.Events exposing (on, targetValue)
import Svg exposing (Svg, g, line, rect, svg)
import Svg.Attributes exposing (fill, height, stroke, strokeWidth, width, x, x1, x2, y, y1, y2)
import String exposing (toInt)
import Json.Decode as Json

type alias Model = { focalLength : Int
                   , photoreceiverRect : Rect
                   }

type Msg = NoOp
         | FocalLength Int
         | Photoreceiver Rect

type alias Point = { x : Float
                   , y : Float
                   }

type alias Rect = { width : Float
                  , height : Float
                  }

chartRect : Rect
chartRect = { width = 500, height = 12000 }

apsCLabel : String
apsCLabel = "ApsC"

apsCRect : Rect
apsCRect = { width = 23.4, height = 16.7 }

full35mmLabel : String
full35mmLabel = "Full35mm"

full35mmRect : Rect
full35mmRect = { width = 36, height = 24 }

init : ( Model, Cmd Msg )
init = ( { focalLength = 30, photoreceiverRect = apsCRect }, Cmd.none )

view : Model -> Svg Msg
view model = 
    div []
        [ div 
              []
              [ text "Focal Length [mm]:"
              , input [ on "input" (targetValue
                                        |> Json.andThen (\strVal ->
                                               case toInt strVal of
                                                   Err msg -> Json.fail msg
                                                   Ok val -> Json.succeed <| FocalLength val
                                          )
                                   )
                      , type_ "range"
                      , Html.min "1"
                      , Html.max "600"
                      , step "1"
                      ] []
              , text <| toString model.focalLength
              ]
        , div
              []
              [ text <| "Photoreceiver size:"
              , select
                    [ on "change" (targetValue |>
                                       Json.andThen (\name ->
                                           case photoreceiverPreset name of
                                               Nothing -> Json.fail "invalid photoreceiver type"
                                               Just rect -> Json.succeed <| Photoreceiver rect
                                       )
                                  )
                    ]
                    [ option
                          [ value apsCLabel ]
                          [ text "APS-C" ]
                    , option
                          [ value full35mmLabel ]
                          [ text "35 mm" ]
                    ]
              ]
        , div
              []
              [ text <| "Horizontal Angle [degree]:" ++ (toString <| horizontalAngle (toFloat model.focalLength) model.photoreceiverRect)
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
        Photoreceiver rect ->
            ( { model | photoreceiverRect = rect }, Cmd.none )

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
drawChart model =
    let
        drawPhotoreceiver : Model -> Svg Msg
        drawPhotoreceiver { focalLength, photoreceiverRect } =
            rect
                [ fill "black"
                , x <| toString  <| (chartRect.width - 10 * photoreceiverRect.width) / 2
                , y "10"
                , width <| toString <| 10 * photoreceiverRect.width
                , height "2"
                ]
                []
        drawLightEdge : Model -> Svg Msg
        drawLightEdge { focalLength, photoreceiverRect } =
            let
                point1 = { x = (chartRect.width - 10 * photoreceiverRect.width) / 2
                         , y = 12
                         }
                point2 = { x = (chartRect.width - 10 * photoreceiverRect.width) / 2 + chartRect.height / 2 * photoreceiverRect.width / toFloat focalLength
                         , y = chartRect.height + point1.y
                         }
            in
                g
                    [ stroke "black"
                    , strokeWidth "1"
                    ]
                    [ line
                          [ x1 <| toString point1.x
                          , y1 <| toString point1.y
                          , x2 <| toString point2.x
                          , y2 <| toString point2.y
                          ]
                          []
                    , line
                          [ x1 <| toString <| chartRect.width - point1.x
                          , y1 <| toString point1.y
                          , x2 <| toString <| chartRect.width - point2.x
                          , y2 <| toString point2.y
                          ]
                          []
                    ]
    in
        svg
            [ id "chart" ]
            [ drawPhotoreceiver model
            , drawLightEdge model
            ]

horizontalAngle : Float -> Rect -> Float
horizontalAngle focalLength photoreceiver =
    radToDeg <| 2 * (atan2 photoreceiver.width <| 2 * focalLength)

radToDeg : Float -> Float
radToDeg rad = rad * 180 / pi

photoreceiverPreset : String -> Maybe Rect
photoreceiverPreset name = case name of
    "ApsC" -> Just apsCRect
    "Full35mm" -> Just full35mmRect
    _ -> Nothing
