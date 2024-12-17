module Main exposing
    ( Direction(..)
    , Model
    , Msg(..)
    , Position(..)
    , main
    , mazeHeight
    , mazeWidth
    , obstacles
    , toNewPosition
    )

import Browser
import Browser.Events
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, classList, style)
import Json.Decode as Decode
import Random
import Set exposing (Set)



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
    if model.gameEnd == StillPlaying then
        Sub.batch
            [ Browser.Events.onKeyDown (keyDecoder |> Decode.map GotDirection)
            , if model.isMoving then
                Browser.Events.onAnimationFrame (\_ -> ToNewPosition)

              else
                Sub.none
            , if model.isGhostMoving then
                model.ghosts
                    |> Dict.toList
                    |> List.map
                        (\( _, ghost ) ->
                            Browser.Events.onAnimationFrame (\_ -> MoveGhost ghost)
                        )
                    |> Sub.batch

              else
                Sub.none
            , Browser.Events.onVisibilityChange
                (\visibility ->
                    case visibility of
                        Browser.Events.Hidden ->
                            SetIsGhostMoving False

                        Browser.Events.Visible ->
                            SetIsGhostMoving True
                )
            ]

    else
        Sub.none


keyDecoder : Decode.Decoder Direction
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                case key of
                    "ArrowLeft" ->
                        Decode.succeed Left

                    "ArrowRight" ->
                        Decode.succeed Right

                    "ArrowUp" ->
                        Decode.succeed Up

                    "ArrowDown" ->
                        Decode.succeed Down

                    _ ->
                        Decode.fail "Different key pressed"
            )



-- MODEL


type Direction
    = Left
    | Right
    | Up
    | Down


type Position
    = Position Int Int


type GameEnd
    = StillPlaying
    | GameOver
    | Winner


type BrowserSupport
    = Current
    | Outdated
        { browser : String
        , currentVersion : String
        , requiredVersion : String
        }


type alias Ghost =
    { name : String
    , position : Position
    , direction : Direction
    , target : Position
    }


type alias Model =
    { direction : Direction
    , position : Position
    , isMoving : Bool
    , isGhostMoving : Bool
    , visited : Set ( Int, Int )
    , gameEnd : GameEnd
    , browserSupport : BrowserSupport
    , ghosts : Dict String Ghost
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
      , isMoving = False
      , isGhostMoving = True
      , position = Position pacManStep pacManStep
      , visited = Set.empty
      , gameEnd = StillPlaying
      , browserSupport = browserSupport
      , ghosts = initGhosts
      }
    , Cmd.none
    )


initGhosts : Dict String Ghost
initGhosts =
    Dict.fromList
        [ ( "Blinky"
          , { name = "Blinky"
            , position = Position (pacManStep * mazeWidth - pacManStep) pacManStep
            , direction = Left
            , target = Position pacManStep pacManStep
            }
          )
        , ( "Pinky"
          , { name = "Pinky"
            , position = Position pacManStep (pacManStep * mazeHeight - pacManStep)
            , direction = Left
            , target = Position pacManStep pacManStep
            }
          )
        ]



-- UPDATE


type Msg
    = GotDirection Direction
    | ToNewPosition
    | SetIsGhostMoving Bool
    | MoveGhost Ghost
    | NewGhostDirection Ghost Direction


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotDirection direction ->
            if direction == model.direction then
                ( model, Cmd.none )

            else
                let
                    newPosition =
                        toNewPosition direction gridPosition

                    gridPosition =
                        let
                            (Position currentX currentY) =
                                model.position
                        in
                        Position (snapToGrid currentX) (snapToGrid currentY)
                in
                if newPosition.isBlocked then
                    ( model, Cmd.none )

                else
                    ( { model
                        | direction = direction
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
                , gameEnd =
                    if Set.size visited + Set.size obstacles == Set.size grid then
                        Winner

                    else
                        model.gameEnd
              }
            , Cmd.none
            )

        SetIsGhostMoving bool ->
            ( { model | isGhostMoving = bool }
            , Cmd.none
            )

        MoveGhost ghost ->
            let
                newPosition =
                    toNewPosition ghost.direction ghost.position

                newGhosts =
                    Dict.update ghost.name
                        (Maybe.map (\g -> { g | position = newPosition.position }))
                        model.ghosts
            in
            if
                newPosition.isBlocked
                --|| isAtIntersection ghost.direction newPosition.position
            then
                -- Todo: implement rules for chase and scatter modes
                -- https://www.youtube.com/watch?v=ataGotQ7ir8
                let
                    randomDirection : Random.Generator Direction
                    randomDirection =
                        Random.uniform Left [ Right, Up, Down ]
                in
                ( model
                , Random.generate (NewGhostDirection ghost) randomDirection
                )

            else
                -- Continue in same direction
                let
                    ( isMoving, gameEnd ) =
                        if isCollision model.position newPosition.position then
                            ( False, GameOver )

                        else
                            ( model.isMoving, model.gameEnd )
                in
                ( { model
                    | ghosts = newGhosts
                    , isMoving = isMoving
                    , gameEnd = gameEnd
                  }
                , Cmd.none
                )

        NewGhostDirection ghost newDirection ->
            let
                newGhostPos =
                    toNewPosition newDirection ghost.position

                newGhosts =
                    Dict.update ghost.name
                        (Maybe.map
                            (\g ->
                                { g
                                    | position = newGhostPos.position
                                    , direction =
                                        if newGhostPos.isBlocked then
                                            ghost.direction

                                        else
                                            newDirection
                                }
                            )
                        )
                        model.ghosts
            in
            ( { model
                | ghosts = newGhosts
                , gameEnd =
                    if isCollision model.position newGhostPos.position then
                        GameOver

                    else
                        model.gameEnd
              }
            , Cmd.none
            )


toVisited : Position -> Set ( Int, Int ) -> Set ( Int, Int )
toVisited (Position x y) visited =
    case ( modBy pacManStep x, modBy pacManStep y ) of
        ( 0, 0 ) ->
            Set.insert ( x // pacManStep, y // pacManStep ) visited

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


{-| Aligns a coordinate to the nearest grid position based on pacManStep.
Used to ensure Pacman stays aligned with the maze grid.
-}
snapToGrid : Int -> Int
snapToGrid int =
    let
        n =
            int + pacManStep // 2
    in
    n - modBy pacManStep n


{-| Calculates the next position based on the current position and direction of movement.
Handles maze wrapping (going through walls) and collision detection with obstacles.
-}
toNewPosition : Direction -> Position -> { position : Position, isBlocked : Bool }
toNewPosition direction (Position x y) =
    let
        toNewCoordinates ( x_, y_ ) =
            if x_ < 0 then
                ( mazeWidth * pacManStep, y_ )

            else if x_ > mazeWidth * pacManStep then
                ( 0, y_ )

            else
                ( x_, y_ )

        isNextPosBlocked nextX nextY =
            Set.member ( nextX // pacManStep, nextY // pacManStep ) obstacles

        ( delta, checkPos ) =
            case direction of
                Left ->
                    ( ( -1, 0 )
                    , \( dx_, _ ) -> isNextPosBlocked (snapToGrid (x + dx_ - pacManStep // 2)) y
                    )

                Right ->
                    ( ( 1, 0 )
                    , \( dx_, _ ) -> isNextPosBlocked (snapToGrid (x + dx_ + (pacManStep - 1) // 2)) y
                    )

                Up ->
                    ( ( 0, -1 )
                    , \( _, dy_ ) -> isNextPosBlocked x (snapToGrid (y + dy_ - pacManStep // 2))
                    )

                Down ->
                    ( ( 0, 1 )
                    , \( _, dy_ ) -> isNextPosBlocked x (snapToGrid (y + dy_ + (pacManStep - 1) // 2))
                    )

        ( dx, dy ) =
            delta

        newCoordinates =
            if checkPos delta then
                Nothing

            else
                Just (toNewCoordinates ( x + dx, y + dy ))
    in
    { position =
        newCoordinates
            |> Maybe.map (\( newX, newY ) -> Position newX newY)
            |> Maybe.withDefault (Position x y)
    , isBlocked = newCoordinates == Nothing
    }


isCollision : Position -> Position -> Bool
isCollision (Position x1 y1) (Position x2 y2) =
    snapToGrid x1 == snapToGrid x2 && snapToGrid y1 == snapToGrid y2


{-| Helper function to check if position is at a grid intersection
-}
isAtIntersection : Direction -> Position -> Bool
isAtIntersection direction (Position x y) =
    -- let
    --     checkIntersection =
    --     case direction of
    --     Left ->
    -- in
    (modBy pacManStep x == 0 && modBy pacManStep y == 0)
        |> Debug.todo "isAtIntersection"



-- MAZE


mazeWidth : Int
mazeWidth =
    26


mazeHeight : Int
mazeHeight =
    30


grid : Set ( Int, Int )
grid =
    List.range 0 mazeWidth
        |> List.concatMap
            (\x ->
                List.range 0 mazeHeight
                    |> List.map (\y -> ( x, y ))
            )
        |> Set.fromList


step : Int
step =
    20


pacManStep : Int
pacManStep =
    10



-- OBSTACLES


type alias Rect =
    { x1 : Int
    , y1 : Int
    , x2 : Int
    , y2 : Int
    }


mirrorVertical : Rect -> Rect
mirrorVertical rect =
    { rect | y1 = mazeHeight - rect.y2, y2 = mazeHeight - rect.y1 }


mirrorHorizontal : Rect -> Rect
mirrorHorizontal rect =
    { rect | x1 = mazeWidth - rect.x2, x2 = mazeWidth - rect.x1 }


rectToPoints : Rect -> List ( Int, Int )
rectToPoints rect =
    List.range rect.x1 rect.x2
        |> List.concatMap
            (\x ->
                List.range rect.y1 rect.y2
                    |> List.map (\y -> ( x, y ))
            )


obstacles : Set ( Int, Int )
obstacles =
    let
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

        mirrorAll rect =
            [ rect
            , mirrorHorizontal rect
            , mirrorVertical rect
            , rect |> mirrorHorizontal |> mirrorVertical
            ]
    in
    (borders ++ List.concatMap mirrorAll topLeftObstacles)
        |> List.concatMap rectToPoints
        |> Set.fromList



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        (Position x y) =
            model.position

        toPx int =
            String.fromInt (step + int * step) ++ "px"

        toPacmanPos int =
            String.fromInt ((step // 2) + (int * step) // pacManStep) ++ "px"

        pacmanTransform =
            String.join " "
                [ "translate(-50%, -50%)"
                , "translate(" ++ toPacmanPos x ++ ", " ++ toPacmanPos y ++ ")"
                , "rotate(" ++ String.fromFloat (turn model.direction) ++ "turn)"
                ]
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
                            , style "transform" pacmanTransform
                            ]
                            []
                      ]
                    , model.ghosts
                        |> Dict.toList
                        |> List.map viewGhost
                    , [ viewGameEnd model.gameEnd ]
                    ]
                )
            ]
        ]
    }


viewGhost : ( String, Ghost ) -> Html Msg
viewGhost ( name, ghost ) =
    let
        (Position x y) =
            ghost.position

        toPx int =
            String.fromInt ((step // 2) + (int * step) // pacManStep) ++ "px"

        ghostTransform =
            String.join " "
                [ "translate(-50%, -50%)"
                , "translate(" ++ toPx x ++ ", " ++ toPx y ++ ")"
                ]
    in
    div
        [ class "ghost"
        , class (String.toLower name)
        , style "transform" ghostTransform
        ]
        []


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

        Outdated { browser, currentVersion, requiredVersion } ->
            div [ class "warning" ]
                [ text <|
                    String.join " "
                        [ "Your"
                        , browser
                        , "version"
                        , currentVersion
                        , "is outdated. Please update to"
                        , requiredVersion
                        , "or later for the best experience."
                        ]
                ]
