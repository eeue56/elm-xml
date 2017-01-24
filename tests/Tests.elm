module Tests exposing (..)

import Dict
import Test exposing (..)
import Expect
import String
import Xml.Encode exposing (..)
import Xml.Decode exposing (..)


example : Value
example =
    object
        [ ( "name", Dict.empty, string "noah" )
        , ( "age", Dict.empty, int 5 )
        ]


exampleAsString : String
exampleAsString =
    """
<name>noah</name>
<age>5</age>
"""
        |> String.trim


exampleWithProps : Value
exampleWithProps =
    object
        [ ( "person"
          , Dict.empty
          , object
                [ ( "name", Dict.fromList [ ( "is", string "me" ) ], string "noah" )
                , ( "age", Dict.fromList [ ( "max", int 10 ) ], int 5 )
                , ( "here", Dict.fromList [ ( "is", bool False ) ], null )
                ]
          )
        , ( "person"
          , Dict.empty
          , object
                [ ( "name", Dict.fromList [ ( "is", string "you" ) ], string "dave" )
                , ( "age", Dict.fromList [ ( "max", int 100 ), ( "inc", float 1.5 ) ], int 50 )
                , ( "here", Dict.fromList [ ( "is", bool True ) ], null )
                ]
          )
        ]


exampleWithPropsAsString : String
exampleWithPropsAsString =
    """
<person>
    <name is="me">noah</name>
    <age max=10>5</age>
    <here is=false></here>
</person>
<person>
    <name is="you">dave</name>
    <age inc=1.5 max=100>50</age>
    <here is=true></here>
</person>
"""
        |> String.trim


badXml : String
badXml =
    """ f<name>noah</name>"""


badXmlWithNoClose : String
badXmlWithNoClose =
    """ <name>noah</naoh>"""


nestedExample : Value
nestedExample =
    object
        [ ( "person"
          , Dict.empty
          , object
                [ ( "name", Dict.empty, string "noah" )
                , ( "age", Dict.empty, int 5 )
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
        , test "a basic tag is decoded properly" <|
            \_ ->
                Expect.equal (decode exampleAsString) (Ok example)
        , test "a tag with props is encoded properly" <|
            \_ ->
                Expect.equal exampleWithPropsAsString (encode 4 exampleWithProps)
        , test "a tag with props is decoded properly" <|
            \_ ->
                Expect.equal (decode exampleWithPropsAsString) (Ok exampleWithProps)
        , test "a nested tag is encoded properly" <|
            \_ ->
                Expect.equal nestedExampleAsString (encode 4 nestedExample)
        , test "a nested tag is decoded properly" <|
            \_ ->
                Expect.equal (decode nestedExampleAsString) (Ok nestedExample)
        , test "a bad xml is an error" <|
            \_ ->
                Expect.false "xml is not parsed without a closing tag"
                    (case decode badXmlWithNoClose of
                        Err _ ->
                            False

                        _ ->
                            True
                    )
        ]
