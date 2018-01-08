{-|
Module      : Http

Definition of functions to handle HTTP requests
-}

module Http
    (
    isGETRequestOk
    ) where

-- |The 'isGETRequestOk' returns whether a GET request got a 200 status code
isGETRequestOk :: Int -> Bool
isGETRequestOk 200 = True
isGETRequestOk _ = False
