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
import Process
import Random
import Set exposing (Set)
import Task



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


type GhostMode
    = Chase
    | Scatter
    | Frightened


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
    , ghostMode : GhostMode
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
      , ghostMode = Chase
      , position = Position (pacManStep * 13) (pacManStep * 23)
      , visited = Set.empty
      , gameEnd = StillPlaying
      , browserSupport = browserSupport
      , ghosts = Dict.empty
      }
    , Process.sleep 1000
        |> Task.perform (always (AddGhost "Blinky"))
    )


initGhosts : Dict String Ghost
initGhosts =
    let
        toGhost name direction =
            ( name
            , { name = name
              , position = Position (pacManStep * 12) (pacManStep * 11)
              , direction = direction
              , target = toScatterTarget name
              }
            )
    in
    Dict.fromList
        [ toGhost "Blinky" Left
        , toGhost "Pinky" Right
        , toGhost "Inky" Left
        , toGhost "Clyde" Right
        ]


toScatterTarget : String -> Position
toScatterTarget name =
    let
        ( offsetX, offsetY ) =
            case name of
                "Blinky" ->
                    ( mazeWidth - 2, -2 )

                "Pinky" ->
                    ( 2, -2 )

                "Inky" ->
                    ( mazeWidth - 2, mazeHeight + 2 )

                "Clyde" ->
                    ( 2, mazeHeight + 2 )

                _ ->
                    ( 0, 0 )
    in
    Position (pacManStep * offsetX) (pacManStep * offsetY)


toChaseTarget : Model -> String -> Position
toChaseTarget model name =
    let
        (Position pacManX pacManY) =
            model.position

        ( pacManXOffset, pacManYOffset ) =
            ( pacManX // pacManStep, pacManY // pacManStep )

        (Position blinkyX blinkyY) =
            case Dict.get "Blinky" model.ghosts of
                Just blinky ->
                    blinky.position

                Nothing ->
                    Position 0 0

        (Position clydeX clydeY) =
            case Dict.get "Clyde" model.ghosts of
                Just clyde ->
                    clyde.position

                Nothing ->
                    Position 0 0

        ( targetX, targetY ) =
            case name of
                "Blinky" ->
                    -- Blinky's target is Pacman's position
                    ( pacManX, pacManY )

                "Pinky" ->
                    -- Pinky's target is 4 steps in front of Pacman's position,
                    -- in the direction that Pacman is facing
                    let
                        ( dx, dy ) =
                            case model.direction of
                                Left ->
                                    ( -4, 0 )

                                Right ->
                                    ( 4, 0 )

                                Up ->
                                    -- When Pacman is facing up,
                                    -- Pinky's target is also 4 to the left
                                    ( -4, -4 )

                                Down ->
                                    ( 0, 4 )
                    in
                    ( (pacManXOffset + dx) * pacManStep
                    , (pacManYOffset + dy) * pacManStep
                    )

                "Inky" ->
                    let
                        ( dx, dy ) =
                            -- Inky's target is calculated based on Pacman and Blinky's position,
                            -- creating a flank attack by using a vector from Blinky's position
                            -- to a target position 2 steps in front of Pacman's position,
                            -- in the direction that Pacman is facing
                            case model.direction of
                                Left ->
                                    ( -2, 0 )

                                Right ->
                                    ( 2, 0 )

                                Up ->
                                    -- Same as with Pinky, when Pacman is facing up,
                                    -- Inky's target is also 2 to the left
                                    ( -2, -2 )

                                Down ->
                                    ( 0, 2 )

                        -- Calculate the vector from Blinky's position to the target position
                        vectorX =
                            ((pacManXOffset + dx) * pacManStep) - blinkyX

                        vectorY =
                            ((pacManYOffset + dy) * pacManStep) - blinkyY

                        -- Inky's target is twice the vector from Blinky's position to the target position
                        inkyTargetX =
                            blinkyX + (2 * vectorX)

                        inkyTargetY =
                            blinkyY + (2 * vectorY)
                    in
                    ( inkyTargetX, inkyTargetY )

                "Clyde" ->
                    -- Clyde's target is Pacman's position unless he is close to Pacman
                    let
                        distance =
                            sqrt
                                ((toFloat (pacManXOffset - (clydeX // pacManStep)) ^ 2)
                                    + (toFloat (pacManYOffset - (clydeY // pacManStep)) ^ 2)
                                )

                        ( dx, dy ) =
                            if distance < 8 then
                                let
                                    (Position scatterX scatterY) =
                                        toScatterTarget "Clyde"
                                in
                                ( scatterX, scatterY )

                            else
                                ( pacManX, pacManY )
                    in
                    ( dx, dy )

                _ ->
                    ( 0, 0 )
    in
    Position targetX targetY



-- UPDATE


type Msg
    = GotDirection Direction
    | ToNewPosition
    | SetIsGhostMoving Bool
    | AddGhost String
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
                    gridPosition =
                        let
                            (Position currentX currentY) =
                                model.position
                        in
                        Position (snapToGrid currentX) (snapToGrid currentY)

                    nextPosition =
                        toNewPosition direction gridPosition

                    { visited, hasEatenPowerPellet } =
                        toVisited gridPosition model.visited
                in
                -- If next position would be blocked, ignore direction change
                if nextPosition.isBlocked then
                    ( model, Cmd.none )

                else
                    ( { model
                        | direction = direction
                        , position = gridPosition
                        , visited = visited
                        , isMoving = True
                      }
                    , Cmd.none
                    )
                        |> updateGhostTargets
                            (if hasEatenPowerPellet then
                                Frightened

                             else
                                model.ghostMode
                            )

        ToNewPosition ->
            if model.gameEnd == GameOver then
                ( model, Cmd.none )

            else
                let
                    { position, isBlocked } =
                        toNewPosition model.direction model.position

                    { visited, hasEatenPowerPellet } =
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
                    |> updateGhostTargets
                        (if hasEatenPowerPellet then
                            Frightened

                         else
                            model.ghostMode
                        )

        SetIsGhostMoving bool ->
            ( { model | isGhostMoving = bool }
            , Cmd.none
            )

        AddGhost name ->
            let
                newGhosts =
                    Dict.get name initGhosts
                        |> Maybe.map
                            (\ghost ->
                                Dict.insert name ghost model.ghosts
                            )
                        |> Maybe.withDefault model.ghosts

                nextName =
                    case name of
                        "Blinky" ->
                            Just "Pinky"

                        "Pinky" ->
                            Just "Inky"

                        "Inky" ->
                            Just "Clyde"

                        _ ->
                            Nothing
            in
            ( { model | ghosts = newGhosts }
            , nextName
                |> Maybe.map
                    (\nextName_ ->
                        Process.sleep 2000
                            |> Task.perform (always (AddGhost nextName_))
                    )
                |> Maybe.withDefault Cmd.none
            )

        MoveGhost ghost ->
            let
                newPosition =
                    toNewPosition ghost.direction ghost.position

                newGhost =
                    { ghost | position = newPosition.position }

                newGhosts =
                    Dict.update ghost.name
                        (Maybe.map (\_ -> newGhost))
                        model.ghosts
            in
            if newPosition.isBlocked || isAtIntersection ghost.direction newPosition.position then
                -- Todo: implement rules for chase and scatter modes
                -- https://www.youtube.com/watch?v=ataGotQ7ir8
                let
                    randomDirection : Random.Generator Direction
                    randomDirection =
                        Random.uniform Left [ Right, Up, Down ]
                in
                ( { model | ghosts = newGhosts }
                , Random.generate (NewGhostDirection newGhost) randomDirection
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
                newPosition =
                    toNewPosition newDirection ghost.position
            in
            if newPosition.isBlocked then
                -- Do nothing, MoveGhost will run again on next animation frame
                ( model, Cmd.none )

            else
                let
                    newGhosts =
                        Dict.update ghost.name
                            (Maybe.map
                                (\g ->
                                    { g
                                        | position = newPosition.position
                                        , direction = newDirection
                                    }
                                )
                            )
                            model.ghosts

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


updateGhostTargets : GhostMode -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateGhostTargets mode ( model, cmd ) =
    let
        newGhosts =
            model.ghosts
                |> Dict.map
                    (\_ ghost ->
                        { ghost
                            | target =
                                case mode of
                                    Chase ->
                                        toChaseTarget model ghost.name

                                    Scatter ->
                                        toScatterTarget ghost.name

                                    Frightened ->
                                        toScatterTarget ghost.name
                        }
                    )
    in
    ( { model
        | ghosts = newGhosts
        , ghostMode = mode
      }
    , cmd
    )


toVisited :
    Position
    -> Set ( Int, Int )
    ->
        { hasEatenPowerPellet : Bool
        , visited : Set ( Int, Int )
        }
toVisited (Position x y) visited =
    if modBy pacManStep x == 0 && modBy pacManStep y == 0 then
        let
            coordinates =
                ( x // pacManStep, y // pacManStep )

            hasEatenPowerPellet =
                Set.member coordinates powerPellets
        in
        { hasEatenPowerPellet = hasEatenPowerPellet
        , visited = Set.insert coordinates visited
        }

    else
        { hasEatenPowerPellet = False, visited = visited }


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
    if modBy pacManStep x == 0 && modBy pacManStep y == 0 then
        let
            sidewaysOffsets =
                case direction of
                    Left ->
                        [ ( 0, -1 )
                        , ( 0, 1 )
                        ]

                    Right ->
                        [ ( 0, -1 )
                        , ( 0, 1 )
                        ]

                    Up ->
                        [ ( -1, 0 )
                        , ( 1, 0 )
                        ]

                    Down ->
                        [ ( -1, 0 )
                        , ( 1, 0 )
                        ]
        in
        sidewaysOffsets
            |> List.all
                (\( dx, dy ) ->
                    Set.member ( x // pacManStep + dx, y // pacManStep + dy ) obstacles
                )
            |> not

    else
        False



-- MAZE


type alias Rect =
    { x1 : Int
    , y1 : Int
    , x2 : Int
    , y2 : Int
    }


step : Int
step =
    20


pacManStep : Int
pacManStep =
    10


mazeWidth : Int
mazeWidth =
    27


mazeHeight : Int
mazeHeight =
    30


grid : Set ( Int, Int )
grid =
    rectToPoints (Rect 0 0 mazeWidth mazeHeight)
        |> Set.fromList


powerPellets : Set ( Int, Int )
powerPellets =
    [ ( 1, 3 )
    , ( 1, 23 )
    , ( 26, 3 )
    , ( 26, 23 )
    ]
        |> Set.fromList


obstacles : Set ( Int, Int )
obstacles =
    let
        borders =
            [ Rect 0 0 0 13 -- left wall
            , Rect 0 15 0 mazeHeight -- left wall
            , Rect 0 0 mazeWidth 0 -- top wall
            , Rect 0 mazeHeight mazeWidth mazeHeight -- bottom wall
            , Rect mazeWidth 0 mazeWidth 13 -- right wall
            , Rect mazeWidth 15 mazeWidth mazeHeight -- right wall
            ]

        ghostHouse =
            Rect 10 12 17 16

        topLeftObstacles =
            [ Rect 2 2 5 4
            , Rect 7 2 11 4
            , Rect 13 1 13 4
            , Rect 13 8 13 10
            , Rect 10 6 13 7
            , Rect 9 9 11 10
            , Rect 2 6 5 7
            , Rect 7 6 8 13
            , Rect 1 9 5 13
            ]

        bottomLeftObstacles =
            [ Rect 1 15 5 19
            , Rect 7 15 8 19
            , Rect 2 21 5 22
            , Rect 4 23 5 25
            , Rect 1 24 2 25
            , Rect 2 27 11 28
            , Rect 13 26 13 28
            , Rect 10 18 13 19
            , Rect 13 20 13 22
            , Rect 7 21 11 22
            , Rect 7 24 8 26
            , Rect 10 24 13 25
            ]

        mirror rect =
            [ rect
            , mirrorHorizontal rect
            ]
    in
    (ghostHouse :: borders ++ List.concatMap mirror topLeftObstacles ++ List.concatMap mirror bottomLeftObstacles)
        |> List.concatMap rectToPoints
        |> Set.fromList


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
            , div [] [ text <| "Ghost mode: " ++ Debug.toString model.ghostMode ]
            , div [] [ text <| "Pacman position: " ++ Debug.toString ( x // pacManStep, y // pacManStep ) ]
            , div []
                (model.ghosts
                    |> Dict.toList
                    |> List.concatMap
                        (\( _, ghost ) ->
                            let
                                (Position targetX targetY) =
                                    ghost.target
                            in
                            [ text <| ghost.name ++ " target: " ++ Debug.toString ( targetX // pacManStep, targetY // pacManStep )
                            , br [] []
                            ]
                        )
                )
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
        , class (directionToString ghost.direction)
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
                        , ( "-is-power-pellet", Set.member ( x, y ) powerPellets )
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


directionToString : Direction -> String
directionToString direction =
    case direction of
        Left ->
            "left"

        Right ->
            "right"

        Up ->
            "up"

        Down ->
            "down"
