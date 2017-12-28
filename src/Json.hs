module Json
    (
    httpRequestOk
    ) where

httpRequestOk :: Int -> Bool
httpRequestOk 200 = True
httpRequestOk _ = False
