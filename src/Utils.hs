module Utils (
    getEndOfList,
    listToMaybe,
    readMaybeFile
    ) where
import Control.Exception (catch, IOException (..))


getEndOfList :: [a] -> Maybe a
getEndOfList (a:a2:as) = getEndOfList (a2:as)
getEndOfList (a:as) = Just a
getEndOfList [] = Nothing


listToMaybe :: [Maybe a] -> Maybe [a]
listToMaybe ((Just x):xs) = case listToMaybe xs of
    Just list -> Just (x:list)
    Nothing -> Nothing
listToMaybe [] = Just []
listToMaybe (Nothing:xs) = Nothing


handleNoRead :: IOException -> IO (Maybe String)
handleNoRead e = return Nothing


readMaybeFile :: String -> IO (Maybe String)
readMaybeFile path = (Just <$> readFile path) `catch` handleNoRead