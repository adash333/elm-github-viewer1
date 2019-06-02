module Route exposing (Route(..), parse)

import Url exposing (Url)
import Url.Parser exposing (..)


type Route
    = Top
    | User String
    | Repo String String


parse : Url -> Maybe Route
parse url =
    Just Top