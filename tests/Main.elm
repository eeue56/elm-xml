port module Main exposing (..)

import Test exposing (..)
import Tests
import Doc.Tests
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)


main : TestProgram
main =
    describe "Everything"
        [ Tests.all
        , Doc.Tests.all
        ]
        |> run emit


port emit : ( String, Value ) -> Cmd msg
