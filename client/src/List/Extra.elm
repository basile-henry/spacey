module List.Extra exposing (..)

import List


update : Int -> (a -> a) -> List a -> List a
update n f =
    List.indexedMap
        (\i a ->
            if i == n then
                f a
            else
                a
        )
