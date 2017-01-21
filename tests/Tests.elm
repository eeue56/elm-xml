module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import Xml.Encode exposing (..)


example : Value
example =
    object
        [ ( "name", string "noah" )
        , ( "age", int 5 )
        ]


exampleAsString : String
exampleAsString =
    """
<name>noah</name>
<age>5</age>
"""
        |> String.trim


nestedExample : Value
nestedExample =
    object
        [ ( "person"
          , object
                [ ( "name", string "noah" )
                , ( "age", int 5 )
                ]
          )
        ]


nestedExampleAsString : String
nestedExampleAsString =
    """
<person>
    <name>noah</name>
    <age>5</age>
</person>
"""
        |> String.trim


all : Test
all =
    describe "Encode test"
        [ test "a basic tag is encoded properly" <|
            \_ ->
                Expect.equal exampleAsString (encode 0 example)
        , test "a nested tag is encoded properly" <|
            \_ ->
                Expect.equal nestedExampleAsString (encode 4 nestedExample)
        ]
