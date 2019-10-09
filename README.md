# elm-xml [![Build Status](https://travis-ci.org/billstclair/elm-xml-eeue56.svg?branch=master)](https://travis-ci.org/billstclair/elm-xml-eeue56)
xml parser for elm

First bring XML into Elm as a `Value`. Once imported as a Value, you can then either query the values with `Xml.Query`.

Or you can turn it back to a string using `Xml.Encode.encode`. Or pull it apart using `Xml.Encode.Value`.

In order to turn an `Xml.Value` into a record, you probably want `Xml.Query`, paired with `Result.map`.

```elm

import Xml exposing (Value)
import Xml.Encode exposing (null)
import Xml.Decode exposing (decode)
import Xml.Query exposing (tags)

decodedXml : Value
decodedXml = 
	"""
<person>
	<name>noah</name>
	<age max="100">50</age>
</person>
<person>
	<name>josh</name>
	<age max="100">57</age>
</person>
	"""
		|> decode
		|> Result.toMaybe
		|> Maybe.withDefault null


type alias Person = 
	{ name: String
	, age: Int
	}

person : Value -> Result String Person
person value =
    Result.map2
        (\name age ->
            { name = name
            , age = age
            }
        )
        (tag "name" string value)
        (tag "age" int value)


people : List Person
people =
    tags "person" decodedXml
        |> collect person


```
