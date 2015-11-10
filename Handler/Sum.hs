module Handler.Sum where

import Import
import qualified Data.Text.Read as TR

sumArray :: [Text] -> Int
sumArray [] = 0

sumArray (x:xs) =
    case (readMaybe x) of
        Nothing -> 0
        Just i -> i + sumArray xs
    --(read x ) + sumArray xs

readMaybe ::  Text -> Maybe Int
readMaybe t =
    case TR.decimal t of
        (Right (i, "")) -> Just i
        (Right (_, _))  -> Nothing
        (Left _)        -> Nothing

getSumR :: [Text] -> Handler Html
getSumR messages =  defaultLayout $(widgetFile "sum")
