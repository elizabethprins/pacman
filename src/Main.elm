module Main exposing
    ( Direction(..)
    , Model
    , Msg(..)
    , Position(..)
    , init
    , main
    , mazeHeight
    , mazeWidth
    , obstacles
    , toNewPosition
    )

import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (class, classList, style)
import Json.Decode as Decode
import Set exposing (Set)
import Svg
import Svg.Attributes



-- MAIN


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Flags =
    { tag : String
    , browser : Maybe String
    , currentVersion : Maybe String
    , requiredVersion : Maybe String
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown
            (keyDecoder toDirection
                |> Decode.andThen
                    (\maybeResult ->
                        case maybeResult of
                            Just result ->
                                Decode.succeed (GotDirection result)

                            Nothing ->
                                Decode.fail "Different key pressed"
                    )
            )
        , if model.isMoving then
            Browser.Events.onAnimationFrame (\_ -> ToNewPosition)

          else
            Sub.none
        ]


keyDecoder : (String -> a) -> Decode.Decoder a
keyDecoder toResult =
    Decode.map toResult (Decode.field "key" Decode.string)


toDirection : String -> Maybe Direction
toDirection string =
    case string of
        "ArrowLeft" ->
            Just Left

        "ArrowRight" ->
            Just Right

        "ArrowUp" ->
            Just Up

        "ArrowDown" ->
            Just Down

        _ ->
            Nothing



-- MODEL


type Direction
    = Left
    | Right
    | Up
    | Down


type Position
    = Position Int Int


type BrowserSupport
    = Current
    | Outdated
        { browser : String
        , currentVersion : String
        , requiredVersion : String
        }


type alias Model =
    { direction : Direction
    , turn : Float
    , position : Position
    , isMoving : Bool
    , visited : Set ( Int, Int )
    , gameEnd : GameEnd
    , browserSupport : BrowserSupport
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        browserSupport =
            case flags.tag of
                "Current" ->
                    Current

                "Outdated" ->
                    Outdated
                        { browser = Maybe.withDefault "" flags.browser
                        , currentVersion = Maybe.withDefault "" flags.currentVersion
                        , requiredVersion = Maybe.withDefault "" flags.requiredVersion
                        }

                _ ->
                    Current
    in
    ( { direction = Down
      , turn = turn Down
      , isMoving = False
      , position = Position pacManSpeed pacManSpeed
      , visited = Set.empty
      , gameEnd = StillPlaying
      , browserSupport = browserSupport
      }
    , Cmd.none
    )


type GameEnd
    = StillPlaying
    | GameOver
    | Winner



-- UPDATE


type Msg
    = NoOp
    | GotDirection Direction
    | ToNewPosition


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotDirection direction ->
            if direction == model.direction then
                ( model, Cmd.none )

            else
                let
                    newPosition =
                        toNewPosition direction model.position

                    (Position currentX currentY) =
                        model.position

                    gridPosition =
                        --  this makes sure pacman eats the dot he's on when he changes direction!
                        Position (snapToGrid currentX) (snapToGrid currentY)
                in
                if newPosition.isBlocked then
                    ( model, Cmd.none )

                else
                    ( { model
                        | direction = direction
                        , turn = turn direction
                        , position = newPosition.position
                        , visited = toVisited gridPosition model.visited
                        , isMoving = True
                      }
                    , Cmd.none
                    )

        ToNewPosition ->
            let
                { position, isBlocked } =
                    toNewPosition model.direction model.position

                visited =
                    toVisited position model.visited
            in
            ( { model
                | position = position
                , isMoving = not isBlocked
                , visited = visited
                , gameEnd = toGameEnd visited model.gameEnd
              }
            , Cmd.none
            )


toGameEnd : Set ( Int, Int ) -> GameEnd -> GameEnd
toGameEnd visited gameEnd =
    if Set.size visited + Set.size obstacles == Set.size grid then
        Winner

    else
        gameEnd


toVisited : Position -> Set ( Int, Int ) -> Set ( Int, Int )
toVisited (Position x y) visited =
    case ( modBy pacManSpeed x, modBy pacManSpeed y ) of
        ( 0, 0 ) ->
            Set.insert ( x // pacManSpeed, y // pacManSpeed ) visited

        _ ->
            visited


{-| Translates a Direction to a CSS angle value expressed in turns.
-}
turn : Direction -> Float
turn direction =
    case direction of
        Left ->
            0.75

        Right ->
            0.25

        Up ->
            0

        Down ->
            0.5


{-| Returns True if the Direction is Up or Down.
-}
isVertical : Direction -> Bool
isVertical direction =
    direction == Up || direction == Down


{-| Add or subtract to the Position's x or y coordinates.
-}
toNewPosition : Direction -> Position -> { position : Position, isBlocked : Bool }
toNewPosition direction (Position currentX currentY) =
    let
        ( x, y ) =
            if isVertical direction then
                ( snapToGrid currentX, currentY )

            else
                ( currentX, snapToGrid currentY )

        toNewCoordinates ( x_, y_ ) =
            if x_ < 0 then
                ( mazeWidth * pacManSpeed, y_ )

            else if x_ > mazeWidth * pacManSpeed then
                ( 0, y_ )

            else
                ( x_, y_ )

        isNextPosBlocked nextX nextY =
            Set.member ( nextX // pacManSpeed, nextY // pacManSpeed ) obstacles

        newCoordinates =
            case direction of
                Left ->
                    let
                        newX =
                            x - 1

                        nextGridPos =
                            snapToGrid (newX - (pacManSpeed // 2))
                    in
                    if isNextPosBlocked nextGridPos y then
                        Nothing

                    else
                        Just (toNewCoordinates ( newX, y ))

                Right ->
                    let
                        newX =
                            x + 1

                        nextGridPos =
                            snapToGrid (newX + (pacManSpeed // 2))
                    in
                    if isNextPosBlocked nextGridPos y then
                        Nothing

                    else
                        Just (toNewCoordinates ( newX, y ))

                Up ->
                    let
                        newY =
                            y - 1

                        nextGridPos =
                            snapToGrid (newY - (pacManSpeed // 2))
                    in
                    if isNextPosBlocked x nextGridPos then
                        Nothing

                    else
                        Just (toNewCoordinates ( x, newY ))

                Down ->
                    let
                        newY =
                            y + 1

                        nextGridPos =
                            snapToGrid (newY + (pacManSpeed // 2))
                    in
                    if isNextPosBlocked x nextGridPos then
                        Nothing

                    else
                        Just (toNewCoordinates ( x, newY ))
    in
    { position =
        newCoordinates
            |> Maybe.map (\( newX, newY ) -> Position newX newY)
            |> Maybe.withDefault (Position currentX currentY)
    , isBlocked = newCoordinates == Nothing
    }



-- MAZE


mazeWidth : Int
mazeWidth =
    26


mazeHeight : Int
mazeHeight =
    30


grid : Set ( Int, Int )
grid =
    let
        xs =
            List.range 0 mazeWidth

        ys =
            List.range 0 mazeHeight
    in
    xs
        |> List.concatMap (\x -> List.map (\y -> ( x, y )) ys)
        |> Set.fromList


step : Int
step =
    20


pacManSpeed : Int
pacManSpeed =
    10


type alias Rect =
    { x1 : Int
    , y1 : Int
    , x2 : Int
    , y2 : Int
    }


obstacles : Set ( Int, Int )
obstacles =
    let
        obstaclePoints : Rect -> List ( Int, Int )
        obstaclePoints rect =
            List.range rect.x1 rect.x2
                |> List.concatMap
                    (\x ->
                        List.range rect.y1 rect.y2
                            |> List.map (\y -> ( x, y ))
                    )

        -- Border walls
        borders =
            [ Rect 0 0 0 14 -- left wall
            , Rect 0 16 0 mazeHeight -- left wall
            , Rect 0 0 mazeWidth 0 -- top wall
            , Rect 0 mazeHeight mazeWidth mazeHeight -- bottom wall
            , Rect mazeWidth 0 mazeWidth 14 -- right wall
            , Rect mazeWidth 16 mazeWidth mazeHeight -- right wall
            ]

        -- Define obstacles for top-left quadrant only
        topLeftObstacles =
            [ Rect 2 2 5 4
            , Rect 7 2 11 4
            , Rect 13 1 13 6
            , Rect 12 8 12 11
            , Rect 9 11 11 11
            , Rect 2 6 5 6
            , Rect 7 6 7 11
            , Rect 9 6 11 6
            , Rect 8 8 10 9
            , Rect 2 8 5 9
            , Rect 2 11 3 12
            , Rect 5 11 5 12
            , Rect 1 14 5 14
            , Rect 9 13 13 15
            , Rect 7 13 7 14
            ]

        -- Helper to mirror y coordinates (vertical)
        mirrorY y =
            mazeHeight - y

        -- Helper to mirror x coordinates (horizontal)
        mirrorX x =
            mazeWidth - x

        -- Mirror vertically
        mirrorVertical : Rect -> Rect
        mirrorVertical rect =
            { rect
                | y1 = mirrorY rect.y2
                , y2 = mirrorY rect.y1
            }

        -- Mirror horizontally
        mirrorHorizontal : Rect -> Rect
        mirrorHorizontal rect =
            { rect
                | x1 = mirrorX rect.x2
                , x2 = mirrorX rect.x1
            }

        -- Create all mirrored versions
        allObstacles =
            topLeftObstacles
                |> List.concatMap
                    (\rect ->
                        [ rect -- top-left (original)
                        , mirrorHorizontal rect -- top-right
                        , mirrorVertical rect -- bottom-left
                        , mirrorVertical (mirrorHorizontal rect) -- bottom-right
                        ]
                    )
    in
    (borders ++ allObstacles)
        |> List.concatMap obstaclePoints
        |> Set.fromList


{-| Round the Position's x or y coordinate to a number
that correlates with a track on the grid.
-}
snapToGrid : Int -> Int
snapToGrid int =
    let
        n =
            int + pacManSpeed // 2
    in
    n - modBy pacManSpeed n



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        (Position x y) =
            model.position

        ( newX, newY ) =
            if isVertical model.direction then
                ( snapToGrid x, y )

            else
                ( x, snapToGrid y )

        toPx int =
            String.fromInt (step + int * step) ++ "px"

        toPacmanPos int =
            String.fromInt ((step // 2) + (int * step) // pacManSpeed) ++ "px"
    in
    { title = "Pacman"
    , body =
        [ main_
            [ class "main"
            , classList [ ( "-is-browser-outdated", model.browserSupport /= Current ) ]
            ]
            [ viewBrowserSupportWarning model.browserSupport
            , div
                [ class "maze"
                , style "height" (toPx mazeHeight)
                , style "width" (toPx mazeWidth)
                ]
                (List.concat
                    [ dots model
                    , [ div
                            [ class "pacman"
                            , classList [ ( "-is-moving", model.isMoving ) ]
                            , style "transform"
                                ("translate(-50%, -50%) translate("
                                    ++ toPacmanPos newX
                                    ++ ", "
                                    ++ toPacmanPos newY
                                    ++ ") rotate("
                                    ++ String.fromFloat model.turn
                                    ++ "turn)"
                                )
                            ]
                            []
                      , viewGameEnd model.gameEnd
                      ]
                    ]
                )
            ]
        ]
    }


dots : Model -> List (Html Msg)
dots { visited } =
    let
        toDotPos : Int -> String
        toDotPos int =
            String.fromInt ((step // 2) + int * step) ++ "px"
    in
    grid
        |> Set.toList
        |> List.map
            (\( x, y ) ->
                div
                    [ class "dot"
                    , classList
                        [ ( "-is-obstacle", Set.member ( x, y ) obstacles )
                        , ( "-is-visited", Set.member ( x, y ) visited )
                        ]
                    , style "left" (toDotPos x)
                    , style "top" (toDotPos y)
                    ]
                    []
            )


viewGameEnd : GameEnd -> Html Msg
viewGameEnd gameEnd =
    let
        message =
            case gameEnd of
                StillPlaying ->
                    Nothing

                GameOver ->
                    Just "Game Over!!"

                Winner ->
                    Just "You Win!!"
    in
    message
        |> Maybe.map (\m -> div [ class "game-end" ] [ text m ])
        |> Maybe.withDefault (text "")


viewBrowserSupportWarning : BrowserSupport -> Html Msg
viewBrowserSupportWarning browserSupport =
    case browserSupport of
        Current ->
            text ""

        Outdated details ->
            div [ class "warning" ]
                [ text <|
                    String.join " "
                        [ "Your"
                        , details.browser
                        , "version"
                        , details.currentVersion
                        , "is outdated. Please update to"
                        , details.requiredVersion
                        , "or later for the best experience."
                        ]
                ]
