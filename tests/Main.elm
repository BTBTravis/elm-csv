module Main exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Csv exposing (..)
import Debug exposing (log, crash)


suite : Test
suite =
    describe "CSV"
        [ describe "Csv.parse"
            [ test "Convert a string of comma-separated values into a `Csv` structure." <|
                \_ -> Expect.equal { headers = [ "id", "value" ], records = [ [ "1", "one" ], [ "2", "two" ] ] } (Csv.parse "id,value\n1,one\n2,two\n")
            , test "Values that contain the character ',' can be quoted" <|
                \_ -> Expect.equal { headers = [ "id", "value" ], records = [ [ "1,2,3", "one,two,three" ] ] } (Csv.parse "id,value\n\"1,2,3\",\"one,two,three\"\n")
            , test "Double quotes can be escaped with a backslash or a second quote" <|
                \_ -> Expect.equal { headers = [ "value" ], records = [ [ "Here is a quote:\"" ], [ "Another one:\"" ] ] } (Csv.parse "value\n,Here is a quote:\"\"\nAnother one:\\\"\n")
            ]
        , describe "Csv.parseWith"
            [ test "Convert a string of values separated by a *separator* into a `Csv` structure." <|
                \_ -> Expect.equal { headers = [ "id", "value" ], records = [ [ "1", "one" ], [ "2", "two" ] ] } (Csv.parseWith ";" "id;value\n;1;one\n;2;two\n")
            ]
        , describe "Csv.split"
            [ test "Convert a string of comma-separated values into a list of lists." <|
                \_ -> Expect.equal [ [ "id", "value" ], [ "1", "one" ], [ "2", "two" ] ] (Csv.split "id,value\n1,one\n2,two\n")
            ]
        , describe "Csv.splitWith"
            [ test "Convert a string of values separated by a character into a list of lists." <|
                \_ -> Expect.equal [ [ "id", "value" ], [ "1", "one" ], [ "2", "two" ] ] (Csv.splitWith "," "id,value\n1,one\n2,two\n")
            ]
        , describe "Value parsing"
            [ test "Empty input" <|
                \_ -> Expect.equal { headers = [], records = [] } (Csv.parse "")
            , test "Simple values" <|
                \_ -> Expect.equal { headers = [ "a", "1" ], records = [] } (Csv.parse "a,1")
            , test "Special characters" <|
                \_ -> Expect.equal { headers = [ "< £200", "Allieds" ], records = [] } (Csv.parse "< £200,Allieds")
            , test "Empty value" <|
                \_ -> Expect.equal { headers = [ "a", "", "1" ], records = [] } (Csv.parse "a,,1")
            , test "Preserves spaces" <|
                \_ -> Expect.equal { headers = [ "a ", "  ", " 1" ], records = [] } (Csv.parse "a ,  , 1")
            , test "Quoted newlines" <|
                \_ -> Expect.equal { headers = [ "a", "\nb\n", "c" ], records = [] } (Csv.parse "a,\"\nb\n\",c")
            , test "Quoted quotes" <|
                \_ -> Expect.equal { headers = [ "a", "\"", "c" ], records = [] } (Csv.parse "a,\"\"\"\",c")
            , test "Quoted commas" <|
                \_ -> Expect.equal { headers = [ "a", "b,b", "c" ], records = [] } (Csv.parse "a,\"b,b\",c")
            ]
        , describe "Line terminators"
            [ test "NL only" <|
                \_ -> Expect.equal { headers = [ "a", "b", "c" ], records = [ [ "d", "e", "f" ], [ "g", "h", "i" ] ] } (Csv.parse "a,b,c\nd,e,f\ng,h,i\n")
            , test "CR only" <|
                \_ -> Expect.equal { headers = [ "a", "b", "c" ], records = [ [ "d", "e", "f" ], [ "g", "h", "i" ] ] } (Csv.parse "a,b,cÝ,e,f\x0Dg,h,i\x0D")
            , test "CR only 2" <|
                \_ -> Expect.equal { headers = [ "a", "b", "c" ], records = [ [ "d", "e", "f" ], [ "g", "h", "i" ] ] } (Csv.parse "a,b,c\x0D\nd,e,f\x0D\ng,h,i\x0D\n")
            , test "Mixed" <|
                \_ -> Expect.equal { headers = [ "a", "b", "c" ], records = [ [ "d", "e", "f" ], [ "g", "h", "i" ] ] } (Csv.parse "a,b,cÝ,e,f\ng,h,i\x0D\n")
            ]
        , describe "Row parsing"
            [ test "Empty headers" <|
                \_ -> Expect.equal { headers = [ "" ], records = [] } (Csv.parse "\n")
            , test "Empty headers, empty row" <|
                \_ -> Expect.equal { headers = [ "" ], records = [ [ "" ] ] } (Csv.parse "\n\n")
            , test "Trailing newline" <|
                \_ -> Expect.equal { headers = [ "a" ], records = [ [ "b" ] ] } (Csv.parse "a\nb\n")
            ]
        ]
