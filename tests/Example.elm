module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Route exposing (Route)
import Url


suite : Test
suite =
    describe "Route"
        [ test "should parse URL" <|
            \_ ->
                Url.fromString "http://example.com/" --Url 型の値を作る
                    |> Maybe.andThen Route.parse     --パースする
                    |> Expect.equal (Just Route.Top) --Topになっていることを確かめる

        ]
