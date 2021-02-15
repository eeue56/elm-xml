module Tests exposing (..)

import Dict
import ExampleStuff
import Expect
import String
import Test exposing (..)
import Xml exposing (..)
import Xml.Decode exposing (..)
import Xml.Encode exposing (..)
import Xml.Query exposing (contains, tags)


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


exampleAsStringWithDocType : String
exampleAsStringWithDocType =
    """
<?xml encoding="UTF-8" version="1"?>
<name>noah</name>
<age>5</age>
"""
        |> String.trim


selfClosingExampleAsString : String
selfClosingExampleAsString =
    """
<person>
    <name is="me">kitofr</name>
    <here is="false" />
    <here is="true"/>
    <there/>
    <name is="me">kitofr</name>
</person>
"""


selfClosingExample : Value
selfClosingExample =
    object
        [ ( "person"
          , Dict.empty
          , object
                [ ( "name", Dict.fromList [ ( "is", string "me" ) ], string "kitofr" )
                , ( "here", Dict.fromList [ ( "is", bool False ) ], null )
                , ( "here", Dict.fromList [ ( "is", bool True ) ], null )
                , ( "there", Dict.fromList [], null )
                , ( "name", Dict.fromList [ ( "is", string "me" ) ], string "kitofr" )
                ]
          )
        ]


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
                [ ( "name", Dict.fromList [ ( "is", string "you i guess" ) ], string "dave" )
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
    <age max="10">5</age>
    <here is="false"></here>
</person>
<person>
    <name is="you i guess">dave</name>
    <age inc="1.5" max="100">50</age>
    <here is="true"></here>
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
                , ( "age", Dict.empty, int 50 )
                , ( "children"
                  , Dict.empty
                  , object
                        [ ( "person"
                          , Dict.fromList [ ( "gender", string "male" ) ]
                          , object
                                [ ( "name", Dict.empty, string "david" )
                                , ( "age", Dict.empty, int 13 )
                                ]
                          )
                        , ( "person"
                          , Dict.fromList [ ( "gender", string "female" ) ]
                          , object
                                [ ( "name", Dict.empty, string "daisy" )
                                , ( "age", Dict.empty, int 25 )
                                , ( "children"
                                  , Dict.empty
                                  , object
                                        [ ( "person"
                                          , Dict.empty
                                          , object
                                                [ ( "name", Dict.empty, string "daniel" )
                                                ]
                                          )
                                        , ( "person"
                                          , Dict.empty
                                          , object
                                                [ ( "name", Dict.empty, string "duncan" )
                                                ]
                                          )
                                        ]
                                  )
                                ]
                          )
                        ]
                  )
                ]
          )
        ]


nestedExampleAsString : String
nestedExampleAsString =
    """
<person>
    <name>noah</name>
    <age>50</age>
    <children>
        <person gender="male">
            <name>david</name>
            <age>13</age>
        </person>
        <person gender="female">
            <name>daisy</name>
            <age>25</age>
            <children>
                <person>
                    <name>daniel</name>
                </person>
                <person>
                    <name>duncan</name>
                </person>
            </children>
        </person>
    </children>
</person>
"""
        |> String.trim


decodedExampleStuff : Result String Value
decodedExampleStuff =
    decode ExampleStuff.stuff


all : Test
all =
    describe "Encode test"
        [ test "a basic tag is encoded properly" <|
            \_ ->
                Expect.equal exampleAsString (encode 0 example)
        , test "a basic tag is decoded properly" <|
            \_ ->
                Expect.equal (decode exampleAsString) (Ok example)
        , test "a basic tag with doc type is encoded properly" <|
            \_ ->
                Expect.equal (Result.map (encode 0) (decode exampleAsStringWithDocType)) (Ok exampleAsStringWithDocType)
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
        , test "a self closing tag is decoded properly" <|
            \_ ->
                Expect.equal (decode selfClosingExampleAsString) (Ok selfClosingExample)
        , test "a bad xml is an error" <|
            \_ ->
                Expect.false "xml is not parsed without a closing tag"
                    (case decode badXmlWithNoClose of
                        Err _ ->
                            False

                        _ ->
                            True
                    )
        , test "a good xml is parsed correctly" <|
            \_ ->
                Expect.true "xml is not parsed without a closing tag"
                    (case decodedExampleStuff of
                        Err m ->
                            False

                        _ ->
                            True
                    )
        , test "the XML contains a node we expect" <|
            \_ ->
                Expect.equal
                    (decodedExampleStuff
                        |> Result.toMaybe
                        |> Maybe.withDefault null
                        |> tags "ListBucketResult"
                        |> List.length
                    )
                    1
        , test "the XML contains a node we expect again" <|
            \_ ->
                Expect.equal
                    (decodedExampleStuff
                        |> Result.toMaybe
                        |> Maybe.withDefault null
                        |> tags "Contents"
                        |> List.length
                    )
                    100
        , test "The query for finding people should find the right tags" <|
            \_ ->
                Expect.equal
                    ExampleStuff.correctPeople
                    ExampleStuff.people
        , test "when parsing complex XML, it finds everything as it should" <|
            \_ ->
                Expect.equal
                    (List.length <| Result.withDefault [] <| ExampleStuff.fromXML <| ExampleStuff.stuff)
                    100
        ]
