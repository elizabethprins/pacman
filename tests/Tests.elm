module Tests exposing (..)

import Expect
import Fuzz exposing (oneOfValues)
import Main exposing (..)
import Set
import Test exposing (..)


suite : Test
suite =
    describe "Pacman"
        [ fuzz
            (Fuzz.triple (oneOfValues [ 0, mazeWidth ])
                (oneOfValues [ 0, mazeHeight ])
                (oneOfValues [ Up, Down, Left, Right ])
            )
            "can't move to coordinates that are blocked by obstacles"
            (\( x, y, direction ) ->
                if Set.member ( x, y ) obstacles then
                    Expect.pass

                else
                    let
                        newPosition =
                            toNewPosition direction (Position x y)

                        (Position newX newY) =
                            newPosition.position
                    in
                    Set.member ( newX, newY ) obstacles
                        |> Expect.equal False
            )
        ]
